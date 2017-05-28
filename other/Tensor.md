The code below is an attempt to define Tensors (n-dimensional arrays)
using Representable Functors. Representable functors are the
[dual](https://www.reddit.com/r/haskell/comments/2y25ly/language_deriveapplicative/)
of traversable ones, and are also known as
[Naperian](https://www.reddit.com/r/haskell/comments/2ckmvb/can_anyone_give_any_references_on/)
functors, named after the
[ghost](http://stackoverflow.com/questions/12963733/writing-cojoin-or-cobind-for-n-dimensional-grid-type)
of John Napier.

A Representable instance for Tensors provides a separation of concerns;
between keeping track of functor shape, which happens at the type level;
and value-level computation, which can be better optimised free of shape
concern.

But type-level coding is hard, and especially hard in Haskell. Being
able to extract a slice of a tensor using an index list is an essential
step for generalising many concepts such as inner product and matrix
multiplication, but calculation of the resultant tensor shape is beyond
the type-fu of this author.

[ghc options](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#flag-reference)
--------------------------------------------------------------------------------------------------------

``` {.sourceCode .literate .haskell}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
```

[pragmas](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/lang.html)
------------------------------------------------------------------------------------

Hands up who thinks pragmas are getting out of hand and a productivity
drain.

``` {.sourceCode .literate .haskell}
-- doctest doesn't look at the cabal file, so you need pragmas here
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
```

[libraries](https://www.stackage.org/)
--------------------------------------

-   [doctest](https://www.stackage.org/package/doctest)
-   [protolude](https://www.stackage.org/package/protolude)
-   [singletons](https://www.stackage.org/package/singletons)
-   [vector](https://www.stackage.org/package/vector)
-   [GHC.Typelits](https://www.stackage.org/haddock/lts-8.12/base-4.9.1.0/GHC-TypeLits.html)
-   [GHC.Exts](https://www.stackage.org/haddock/lts-8.12/base-4.9.1.0/GHC-Exts.html)
-   [adjunctions](https://www.stackage.org/package/adjunctions)
-   [distributive](https://www.stackage.org/package/distributive)

``` {.sourceCode .literate .haskell}
module Tensor where

import Data.Distributive
import Data.Functor.Rep
import Data.List ((!!))
import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import GHC.Exts
import GHC.Show
-- import GHC.TypeLits
import Protolude hiding (show, (<.>))
import qualified Data.Vector as V
```

code
----

-   [hoogle](https://www.stackage.org/package/hoogle)

``` {.sourceCode .literate .haskell}
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedLists
-- >>> :set -XTypeFamilies
-- >>> let a = [1..24] :: Tensor '[2,3,4] Int

-- | an n-dimensional array where shape is specified at the type level
-- The main purpose of this, beyond safe typing, is to supply the Representable instance with an initial object.
-- A single Boxed 'Data.Vector.Vector' is used underneath for efficient slicing, but this may change or become polymorphic in the future.
-- >>> a
-- [[[1, 2, 3, 4],
--   [5, 6, 7, 8],
--   [9, 10, 11, 12]],
--  [[13, 14, 15, 16],
--   [17, 18, 19, 20],
--   [21, 22, 23, 24]]]
newtype Tensor (r::[Nat]) a = Tensor { flattenTensor :: V.Vector a }
    deriving (Functor, Eq, Foldable)
```

Haskell lacks the category theory concept of an initial object at the
value level. You can't look inside a Functor to discover it's shape, so
you must make a Representable Functor by attaching shape information at
the type level.

What haskell also lacks is a pleasant experience when coding at the type
level. Whenever type-level trickery is not needed, I downgrade to a
represnetation with shape specified at the value level.

``` {.sourceCode .literate .haskell}

-- | an n-dimensional array where shape is specified at the value level
-- >>> let b = someTensor a
-- >>> :t b
-- b :: SomeTensor Int

data SomeTensor a = SomeTensor [Int] (V.Vector a)
    deriving (Functor, Eq, Foldable)

-- | convert a 'Tensor' to a 'SomeTensor', losing the type level shape
someTensor :: (SingI r) => Tensor (r::[Nat]) a -> SomeTensor a
someTensor n = SomeTensor (shape n) (flattenTensor n)

-- | extracts shape from the type level
-- >>> shape a
-- [2,3,4]
shape :: forall (r::[Nat]) a. (SingI r) => Tensor r a -> [Int]
shape _ = case (sing :: Sing r) of
            SNil -> []
            (SCons x xs) -> fromIntegral <$> (fromSing x: fromSing xs)

-- | convert from n-dim shape index to a flat index
-- >>> ind [2,3,4] [1,1,1]
-- 17
ind :: [Int] -> [Int] -> Int
ind ns xs = sum $ zipWith (*) xs (drop 1 $ scanr (*) 1 ns)

unfoldI :: forall t. Integral t => [t] -> t -> ([t], t)
unfoldI ns x =
    foldr
    (\a (acc, r) -> let (d,m) = divMod r a in (m:acc,d))
    ([],x)
    ns

-- | convert from a flat index to a shape index
-- >>> unind [2,3,4] 17
-- [1,1,1]
unind :: [Int] -> Int -> [Int]
unind ns x= fst $ unfoldI ns x

instance forall r. (SingI r) => Distributive (Tensor (r::[Nat])) where
    distribute f = Tensor $ V.generate n
        $ \i -> fmap (\(Tensor v) -> V.unsafeIndex v i) f
      where
        n = case (sing :: Sing r) of
          SNil -> 1
          (SCons x xs) -> product $ fromInteger <$> (fromSing x: fromSing xs)

instance forall (r :: [Nat]). (SingI r) => Representable (Tensor r) where
    type Rep (Tensor r) = [Int]
    tabulate f = Tensor $ V.generate (product ns) (f . unind ns)
      where
        ns = case (sing :: Sing r) of
          SNil -> []
          (SCons x xs) -> fromIntegral <$> (fromSing x: fromSing xs)
    index (Tensor xs) rs = xs V.! ind ns rs
      where
        ns = case (sing :: Sing r) of
          SNil -> []
          (SCons x xs') -> fromIntegral <$> (fromSing x: fromSing xs')

-- | from flat list
instance (SingI r, Num a) => IsList (Tensor (r::[Nat]) a) where
    type Item (Tensor r a) = a
    fromList l = Tensor $ V.fromList $ take n $ l ++ repeat 0
      where
        n = case (sing :: Sing r) of
          SNil -> 1
          (SCons x xs') -> product $ fromIntegral <$> (fromSing x: fromSing xs')
    toList (Tensor v) = V.toList v

instance (Show a) => Show (SomeTensor a) where
    show r@(SomeTensor l _) = go (length l) r
      where
        go n r'@(SomeTensor l' v') = case length l' of
          0 -> show $ V.head v'
          1 -> "[" ++ intercalate ", " (show <$> GHC.Exts.toList v') ++ "]"
          x -> 
              "[" ++
              intercalate
              (",\n" ++ replicate (n-x+1) ' ')
              (go n <$> flatten1 r') ++
              "]"

-- | convert the top layer of a SomeTensor to a [SomeTensor]
flatten1 :: SomeTensor a -> [SomeTensor a]
flatten1 (SomeTensor rep v) = (\s -> SomeTensor (drop 1 rep) (V.unsafeSlice (s*l) l v)) <$> ss
    where
      n = fromMaybe 0 $ head rep
      ss = take n [0..]
      l = product $ drop 1 rep

instance (Show a, SingI r) => Show (Tensor (r::[Nat]) a) where
    show = show . someTensor

-- ** Operations

-- | inner product
-- >>> let v = [1,2,3] :: Tensor '[3] Int
-- >>> v <.> v
-- 14
(<.>) :: (Num a, Foldable m, Representable m) => m a -> m a -> a
(<.>) a b = sum $ liftR2 (*) a b
```

around here, ghc and myself start to diverge. The outer product below
works in ghci but fails the doctest.

``` {.sourceCode .literate .haskell}
-- | outer product
-- >>> v >< v
-- ...
-- ... Couldn't match type ‘r0 :++ s0’ with ‘r :++ s’
-- ... Expected type: Tensor (r :++ s) a
-- ... Actual type: Tensor (r0 :++ s0) a
-- ... NB: ‘:++’ is a type function, and may not be injective
-- ... The type variables ‘r0’, ‘s0’ are ambiguous
-- ...

-- [[1,2,3],
--  [2,4,6],
--  [3,6,9]]
(><) :: forall (r::[Nat]) (s::[Nat]) a. (Num a, SingI r, SingI s, SingI (r :++ s)) => Tensor r a -> Tensor s a -> Tensor (r :++ s) a
(><) m n = tabulate (\i -> index m (take dimm i) * index n (drop dimm i))
  where
    dimm = length (shape m)
```

The representable instance allows the classical definition of matrix
multiplication. But here I had to switch to KnownNat constraints, and
ditch the SingI ones I had been using. To generalise this to an n-tensor
requires a switch back.

``` {.sourceCode .literate .haskell}
-- | matrix multiplication for a '2-Tensor'
mmult :: forall m n k a. (Num a, KnownNat m, KnownNat n, KnownNat k) =>
    Tensor '[m,k] a ->
    Tensor '[k,n] a ->
    Tensor '[m,n] a
mmult x y = tabulate (\[i,j] -> col i x <.> row j y)

-- | extract the first dimension (aka the row) from a '2-Tensor' as a '1-Tensor'
row :: forall a m n. (KnownNat m, KnownNat n) =>
    Int ->
    Tensor '[m,n] a ->
    Tensor '[m] a
row i t@(Tensor a) = Tensor $ V.unsafeSlice (i*m) n a
  where
    [m,n] = shape t

-- | extract the second dimension (aka the col) from a '2-Tensor' as a '1-Tensor'
col :: forall a m n. (KnownNat m, KnownNat n) =>
    Int ->
    Tensor '[m,n] a ->
    Tensor '[n] a
col i t@(Tensor a) = Tensor $ V.generate m (\x -> a V.! (i+x*n))
  where
    [m,n] = shape t
```

This is the key to making it all work, and where by type-fu runs out.

The doctest below only works because the new shape of the resultant
tensor is supplied.

The value-level calculation is easy:

    vslice :: [[Nat]] -> [Nat]
    vslice xs = fromIntegral . length <$> xs

But how do you do this computation at the type level?

``` {.sourceCode .literate .haskell}
-- | take a slice of a Tensor
-- >>>  vslice [[0,1],[2],[1,2]] a :: Tensor '[2,1,2] Int
-- [[[10, 11]],
--  [[22, 23]]]
vslice :: forall a r s. (SingI r, SingI s) =>
    [[Int]] -> Tensor (r::[Nat]) a -> Tensor (s::[Nat]) a
vslice xs f = tabulate $ \xs' -> index f (zipWith (!!) xs xs')
```
