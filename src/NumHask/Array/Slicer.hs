{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module NumHask.Array.Slicer where

import NumHask.Array hiding (Slice, slice)

import Data.Functor.Rep
import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TH ((:~:)(..))
import Data.Singletons.TypeLits
import GHC.TypeLits
import GHC.TypeLits.Compare
import Data.List ((!!))
import Data.Kind
import Data.Maybe
import Protolude
import qualified Data.Vector as V

data ProdMap :: (a -> b -> Type) -> [a] -> [b] -> Type where
    PMZ :: ProdMap f '[] '[]
    PMS :: f a b -> ProdMap f as bs -> ProdMap f (a ': as) (b ': bs)

data Slice :: Nat -> Nat -> Type where
    Slice :: Sing l -> Sing c -> Sing r -> Slice (l + c + r) c

unaryOp ::
    (SingI ns, SingI ms) =>
    ((SingI ms) => Array ns a -> Array ms a) ->
    ProdMap Slice ns ms ->
    Array ns a ->
    Array ms a
unaryOp f _ t = f t

data IsLTE :: Nat -> Nat -> Type where
    IsLTE :: (n <= m) => Sing n -> Sing m -> IsLTE n m 

-- given a type-level list `ns` of the number of items from each dimension to take,
-- returns the `ProdMap Slice ms ns` that encodes that.
encode :: forall ms ns. ProdMap IsLTE ns ms -> ProdMap Slice ms ns
encode = \case
    PMZ -> PMZ
    IsLTE sN sM `PMS` ltes ->
      Slice (SNat @0) sN (sM %:- sN) `PMS` encode ltes

data CurrySing :: k1 -> k2 -> Type where
    CurrySing :: Sing a -> Sing b -> CurrySing a b

mkLTE :: ProdMap CurrySing ns ms -> Maybe (ProdMap IsLTE ns ms)
mkLTE = \case
    PMZ -> Just PMZ
    CurrySing (n@SNat :: Sing n) (m@SNat :: Sing m) `PMS` pms -> case 
    -- %<=? from http://hackage.haskell.org/package/typelits-witnesses-0.2.3.0/docs/GHC-TypeLits-Compare.html
     Proxy @n %<=? Proxy @m of
       LE Refl -> (IsLTE n m `PMS`) <$> mkLTE pms
       _       -> Nothing

zipSings
    :: Sing as
    -> Sing bs
    -> Maybe (ProdMap CurrySing as bs)
zipSings = \case
    SNil -> \case
      SNil -> Just PMZ
      _ `SCons` _ -> Nothing
    sA `SCons` sAs -> \case
      SNil        -> Nothing
      sB `SCons` sBs ->
        (CurrySing sA sB `PMS`) <$> zipSings sAs sBs

slicer
    :: SingI ms
    => [[Integer]]
    -> Array ms a
    -> (forall ns. SingI ns => Array ns a -> Either Text r)
    -> Either Text r
slicer nss t f = withSomeSing (fmap (fromIntegral . length) nss) $ \nsSing ->
                       withSingI nsSing $ do
    nsms <- maybe (Left "dimensions don't line up") Right $
                zipSings nsSing sing
    lte  <- maybe (Left "dimensions out of range") Right $
                mkLTE nsms
    f $ (unaryOp $ unsafeSlice (fmap fromIntegral <$> nss)) (encode lte) t

-- |
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedLists
-- >>> let a = Array [1..24] :: Array '[2,3,4] Int
-- >>> slicer [[0,1],[2],[1,2]] a (Right . show)
-- Right "[[[10, 11]],\n [[22, 23]]]"
