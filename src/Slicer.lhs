\begin{code}


{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

module Slicer where

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
import Tensor
import Protolude

data ProdMap :: (a -> b -> Type) -> [a] -> [b] -> Type where
    PMZ :: ProdMap f '[] '[]
    PMS :: f a b -> ProdMap f as bs -> ProdMap f (a ': as) (b ': bs)

data Slice :: Nat -> Nat -> Type where
    Slice :: Sing l -> Sing c -> Sing r -> Slice (l + c + r) c

unaryOp ::
    (SingI ns, SingI ms) =>
    ((SingI ms) => Tensor ns a -> Tensor ms a) ->
    ProdMap Slice ns ms ->
    Tensor ns a ->
    Tensor ms a
unaryOp f _ t = f t

indexSlice' :: (SingI ms, SingI ns) => [[Int]] -> Tensor ns a -> Tensor ms a
indexSlice' xs t = tabulate $ \xs' -> index t (fromIntegral <$> zipWith (!!) xs xs')

headSlice
    :: (SingI ns, SingI ms) 
    => ProdMap Slice ns ms
    -> Tensor ns a
    -> Tensor ms a
headSlice _ t = tabulate $ \xs' -> index t xs'

indexSlice
    :: (SingI ns, SingI ms) 
    => ProdMap Slice ns ms
    -> [[Integer]]
    -> Tensor ns a
    -> Tensor ms a
indexSlice _ xs t = tabulate $ \xs' -> index t (fromIntegral <$> zipWith (!!) xs xs')

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
     (Proxy @n) %<=? (Proxy @m) of
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

zipSingsUnsafe a b = fromMaybe PMZ (zipSings a b)

sliceHeads
    :: SingI ms
    => [Integer]
    -> Tensor ms a
    -> (forall ns. SingI ns => Tensor ns a -> Either Text r)
    -> Either Text r
sliceHeads ns t f = withSomeSing ns $ \nsSing ->
                       withSingI nsSing $ do
    nsms <- maybe (Left "dimensions don't line up") Right $
                zipSings nsSing sing
    lte  <- maybe (Left "dimensions out of range") Right $
                mkLTE nsms
    f $ headSlice (encode lte) t

slice
    :: SingI ms
    => [[Integer]]
    -> Tensor ms a
    -> (forall ns. SingI ns => Tensor ns a -> Either Text r)
    -> Either Text r
slice nss t f = withSomeSing (fmap (fromIntegral . length) nss) $ \nsSing ->
                       withSingI nsSing $ do
    nsms <- maybe (Left "dimensions don't line up") Right $
                zipSings nsSing sing
    lte  <- maybe (Left "dimensions out of range") Right $
                mkLTE nsms
    f $ (unaryOp $ indexSlice' (fmap fromIntegral <$> nss)) (encode lte) t

slice'
    :: SingI ms
    => [[Integer]]
    -> Tensor ms a
    -> (forall ns. SingI ns => Tensor ns a -> Either Text r)
    -> Either Text r
slice' nss t f =
    applyEither
    (indexSlice' (fmap fromIntegral <$> nss))
    (fmap (fromIntegral . length) nss) t f

applyEither
    :: forall ms ns os x a b.
    SingI ms
    => (forall ms os. (SingI ms, SingI os) => Tensor ms a -> Tensor os a)
    -> [Integer]
    -> Tensor ms a
    -> (forall ns. SingI ns => Tensor ns a -> Either Text b)
    -> Either Text b
applyEither op x t f = withSomeSing x $ \nsSing ->
                       withSingI nsSing $ do
    nsms <- maybe (Left "dimensions don't line up") Right $
                zipSings nsSing sing
    lte  <- maybe (Left "dimensions out of range") Right $
                mkLTE nsms
    f $ (unaryOp op) (encode lte) t

newtype TensorS m b a =
    TensorS { runTensor_ :: forall ns. (SingI ns) => StateT (Tensor ns b) m a }

instance (Functor m) => Functor (TensorS m b) where
    fmap f m@(TensorS t) = TensorS $ fmap f t

instance (Monad m) => Applicative (TensorS m b) where
    pure a = TensorS $ pure a
    TensorS a <*> TensorS b = TensorS $ a <*> b

-- |
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedLists
-- >>> let a = Tensor [1..24] :: Tensor '[2,3,4] Int
-- >>> sliceHeads [2,2,2] a (Left . show)
-- Left "[[[1, 2],\n  [5, 6]],\n [[13, 14],\n  [17, 18]]]"
-- >>> slice [[0,1],[2],[1,2]] a (Left . show)
-- Left "[[[10, 11]],\n [[22, 23]]]"

\end{code}
