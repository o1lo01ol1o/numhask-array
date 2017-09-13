{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module NumHask.Array.Constraints
  ( IsValidConcat
  , Squeeze
  , Concatenate
  , IsValidTranspose
  , DimShuffle
  , dimShuffle
  , Fold
  , FoldAlong
  , TailModule
  , HeadModule
  , Transpose
  ) where

import Data.Singletons.Prelude hiding (Max)
import Data.Singletons.Prelude.List
       ((:!!$), Drop, Filter, Head, Insert,
        Length, Minimum, SplitAt, Sum, Take, ZipWith,)
import Data.Singletons.Prelude.Tuple (Fst, Snd)
import Data.Singletons.TH (promote)
import Data.Singletons.TypeLits (Nat)
import qualified Protolude as P
import GHC.Err (error)

#if ( __GLASGOW_HASKELL__ < 801 )
instance P.Eq Nat where
  x == y = P.not (x P./= y)
  x /= y = P.not (x P.== y)

instance P.Ord Nat where
  x > y = P.not (x P./= y) P.&& P.not (x P.< y)
  x < y = P.not (x P./= y) P.&& P.not (x P.> y)
  x <= y = (x P.== y) P.|| P.not (x P.> y)
  x >= y = (x P.== y) P.|| P.not (x P.< y)
#endif

(!!) :: [a] -> Nat -> a
[] !! _ = error "Data.Singletons.List.!!: index too large"
(x:xs) !! n =
  if n P.== 0
    then x
    else xs !! (n P.- 1)

type family DropDim d a :: [b] where
  DropDim 0 xs = Drop 1 xs
  DropDim d xs = Take (d :- 1) (Fst (SplitAt d xs)) :++ Snd (SplitAt d xs)

type family IsValidConcat i (a :: [Nat]) (b :: [Nat]) :: P.Bool where
  IsValidConcat _ '[] _ = 'P.False
  IsValidConcat _ _ '[] = 'P.False
  IsValidConcat i a b = And (ZipWith (:==$) (DropDim i a) (DropDim i b))

type family Squeeze (a :: [Nat]) where
  Squeeze '[] = '[]
  Squeeze a = Filter ((:/=$$) 1) a

type family IsValidTranspose (p :: [Nat]) (a :: [Nat]) :: P.Bool where
  IsValidTranspose p a =
    (Minimum p :>= 0) :&& (Minimum a :>= 0) :&& (Sum a :== Sum p) :&& Length p :== Length a

type family Transpose a where
  Transpose a = Reverse a

type family AddDimension (d :: Nat) t :: [Nat] where
  AddDimension d t = Insert d t

type family Concatenate i (a :: [Nat]) (b :: [Nat]) :: [Nat] where
  Concatenate i a b =
    Take i (Fst (SplitAt (i :+ 1) a)) :++
    ('[ Head (Drop i a) :+ Head (Drop i b)]) :++
    Snd (SplitAt (i :+ 1) b)

-- | Reduces axis i in shape s.  Maintains singlton dimension
type family FoldAlong i (s :: [Nat]) where
  FoldAlong _ '[] = '[]
  FoldAlong d xs = Take d (Fst (SplitAt (d :+ 1) xs)) :++ '[ 1] :++ Snd (SplitAt (d :+ 1) xs)

-- | Reduces axis i in shape s. Does not maintain singlton dimension.
type family Fold i (s :: [Nat]) where
  Fold _ '[] = '[]
  Fold d xs = Take d (Fst (SplitAt (d :+ 1) xs)) :++ Snd (SplitAt (d :+ 1) xs)

type family TailModule i (s :: [Nat]) where
  TailModule _ '[] = '[]
  TailModule d xs = (Snd (SplitAt d xs))

type family HeadModule i (s :: [Nat]) where
  HeadModule _ '[] = '[]
  HeadModule d xs = (Fst (SplitAt d xs))

$(promote
    [d|
  dimShuffle :: P.Eq a => [a] -> [Nat] -> [a]
  dimShuffle _ [] = []
  dimShuffle [] _ = []
  dimShuffle (x : xs) (b : bs)
    = if b P.== 0 then x : dimShuffle xs bs else
        (xs !! (b P.- 1)) : dimShuffle xs bs
  |])
