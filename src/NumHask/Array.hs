{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
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

-- | safe-typed n-dimensional arrays
module NumHask.Array where

import Data.Distributive
import Data.Functor.Rep
import Data.Promotion.Prelude
import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import GHC.Exts
import NumHask.Array.Constraints
import GHC.Show
import Protolude hiding (show, (<.>), Map, All)
import qualified Data.Vector as V

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedLists
-- >>> :set -XTypeFamilies
-- >>> let a = [1..24] :: Array '[2,3,4] Int
-- >>> let v = [1,2,3] :: Array '[3] Int

-- | an n-dimensional array where shape is specified at the type level
-- The main purpose of this, beyond safe typing, is to supply the Representable instance with an initial object.
-- >>> a
-- [[[1, 2, 3, 4],
--   [5, 6, 7, 8],
--   [9, 10, 11, 12]],
--  [[13, 14, 15, 16],
--   [17, 18, 19, 20],
--   [21, 22, 23, 24]]]
newtype Array (r::[Nat]) a = Array { flattenArray :: V.Vector a }
    deriving (Functor, Eq, Foldable)

-- | an n-dimensional array where shape is specified at the value level
data SomeArray a = SomeArray [Int] (V.Vector a)
    deriving (Functor, Eq, Foldable)

-- | convert a 'Array' to a 'SomeArray', losing the type level shape
someArray :: (SingI r) => Array (r::[Nat]) a -> SomeArray a
someArray n = SomeArray (shape n) (flattenArray n)

-- | extracts shape from the type level
-- >>> shape a
-- [2,3,4]
shape :: forall (r::[Nat]) a. (SingI r) => Array r a -> [Int]
shape _ = fmap fromIntegral (fromSing (sing :: Sing r))

-- | convert from n-dim shape index to a flat index
-- >>> ind [2,3,4] [1,1,1]
-- 17
ind :: [Int] -> [Int] -> Int
ind ns xs = sum $ zipWith (*) xs (drop 1 $ scanr (*) 1 ns)

-- | convert from a flat index to a shape index
-- >>> unind [2,3,4] 17
-- [1,1,1]
unind :: [Int] -> Int -> [Int]
unind ns x=
    fst $
    foldr
    (\a (acc, r) -> let (d,m) = divMod r a in (m:acc,d))
    ([],x)
    ns

instance forall r. (SingI r) => Distributive (Array (r::[Nat])) where
    distribute f = Array $ V.generate n
        $ \i -> fmap (\(Array v) -> V.unsafeIndex v i) f
      where
        n = case (sing :: Sing r) of
          SNil -> 1
          (SCons x xs) -> product $ fromInteger <$> (fromSing x: fromSing xs)

instance forall (r :: [Nat]). (SingI r) => Representable (Array r) where
    type Rep (Array r) = [Int]
    tabulate f = Array $ V.generate (product ns) (f . unind ns)
      where
        ns = case (sing :: Sing r) of
          SNil -> []
          (SCons x xs) -> fromIntegral <$> (fromSing x: fromSing xs)
    index (Array xs) rs = xs V.! ind ns rs
      where
        ns = case (sing :: Sing r) of
          SNil -> []
          (SCons x xs') -> fromIntegral <$> (fromSing x: fromSing xs')

-- | from flat list
instance (SingI r, Num a) => IsList (Array (r::[Nat]) a) where
    type Item (Array r a) = a
    fromList l = Array $ V.fromList $ take n $ l ++ repeat 0
      where
        n = case (sing :: Sing r) of
          SNil -> 1
          (SCons x xs') -> product $ fromIntegral <$> (fromSing x: fromSing xs')
    toList (Array v) = V.toList v

instance (Show a) => Show (SomeArray a) where
    show r@(SomeArray l _) = go (length l) r
      where
        go n r'@(SomeArray l' v') = case length l' of
          0 -> show $ V.head v'
          1 -> "[" ++ intercalate ", " (show <$> GHC.Exts.toList v') ++ "]"
          x -> 
              "[" ++
              intercalate
              (",\n" ++ replicate (n-x+1) ' ')
              (go n <$> flatten1 r') ++
              "]"

-- | convert the top layer of a SomeArray to a [SomeArray]
flatten1 :: SomeArray a -> [SomeArray a]
flatten1 (SomeArray rep v) = (\s -> SomeArray (drop 1 rep) (V.unsafeSlice (s*l) l v)) <$> ss
    where
      (n, l) = case rep of
        [] -> (0, 1)
        x : r -> (x, product r)
      ss = take n [0..]

instance (Show a, SingI r) => Show (Array (r::[Nat]) a) where
    show = show . someArray

-- ** Operations

-- | inner product
-- >>> let v = [1,2,3] :: Array '[3] Int
-- >>> v <.> v
-- 14
(<.>) :: (Num a, Foldable m, Representable m) => m a -> m a -> a
(<.>) a b = sum $ liftR2 (*) a b

-- | outer product
-- >>> v >< v
-- [[1, 2, 3],
--  [2, 4, 6],
--  [3, 6, 9]]
(><)
    :: forall (r::[Nat]) (s::[Nat]) a
    . (Num a, SingI r, SingI s, SingI (r :++ s))
    => Array r a -> Array s a -> Array (r :++ s) a
(><) m n = tabulate (\i -> index m (take dimm i) * index n (drop dimm i))
  where
    dimm = length (shape m)

-- | matrix multiplication for a '2-Array'
--
-- >>> let a = [1, 2, 3, 4] :: Array '[2, 2] Int
-- >>> let b = [5, 6, 7, 8] :: Array '[2, 2] Int
-- >>> a
-- [[1, 2],
--  [3, 4]]
-- >>> b
-- [[5, 6],
--  [7, 8]]
-- >>> mmult a b
-- [[19, 22],
--  [43, 50]]
mmult :: forall m n k a. (Num a, KnownNat m, KnownNat n, KnownNat k) =>
    Array '[m,k] a ->
    Array '[k,n] a ->
    Array '[m,n] a
mmult x y = tabulate (\[i,j] -> unsafeRow i x <.> unsafeCol j y)

-- | extract the row of a matrix
row
  :: forall i a m n
  .  (KnownNat m, KnownNat n, KnownNat i, (i :< m) ~ 'True)
  => Proxy i
  -> Array '[m,n] a
  -> Array '[n] a
row i_ t = unsafeRow i t
  where
    i = (fromIntegral . fromSing . singByProxy) i_

unsafeRow
  :: forall a m n
  .  (KnownNat m, KnownNat n)
  => Int -> Array '[m, n] a -> Array '[n] a
unsafeRow i t@(Array a) = Array $ V.unsafeSlice (i * n) n a
  where
    [_,n] = shape t

-- | extract the column of a matrix
col
  :: forall j a m n
  .  (KnownNat m, KnownNat n, KnownNat j, (j :< n) ~ 'True)
  => Proxy j
  -> Array '[m,n] a
  -> Array '[m] a
col j_ t = unsafeCol j t
  where
    j = (fromIntegral . fromSing . singByProxy) j_

unsafeCol :: forall a m n. (KnownNat m, KnownNat n) =>
    Int ->
    Array '[m,n] a ->
    Array '[m] a
unsafeCol j t@(Array a) = Array $ V.generate n (\x -> a V.! (j+x*m))
  where
    [m,n] = shape t

-- |
--
-- >>> unsafeIndex a [0,2,1]
-- 10
unsafeIndex :: SingI r => Array r a -> [Int] -> a
unsafeIndex t@(Array a) i = a V.! ind (shape t) i

-- |
--
-- >>> unsafeSlice [[0,1],[2],[1,2]] a :: Array '[2,1,2] Int
-- [[[10, 11]],
--  [[22, 23]]]
unsafeSlice :: (SingI r) => [[Int]] -> Array r a -> Array r0 a
unsafeSlice s t = Array (V.fromList [unsafeIndex t i | i <- sequence s])

-- | Slice xs = Map Length xs
type family Slice (xss :: [[Nat]]) :: [Nat] where
    Slice xss = Map LengthSym0 xss

-- | AllLT xs n = All (n >) xs
data AllLTSym0 (a :: TyFun [Nat] (TyFun Nat Bool -> Type))
data AllLTSym1 (l :: [Nat]) (a :: TyFun Nat Bool)

type instance Apply AllLTSym0 l = AllLTSym1 l
type instance Apply (AllLTSym1 l) n = All ((:>$$) n) l

-- |
--
-- >>> slice (Proxy :: Proxy '[ '[0,1],'[2],'[1,2]]) a
-- [[[10, 11]],
--  [[22, 23]]]
slice
  :: forall s r a
  . (SingI s, SingI r, And (ZipWith AllLTSym0 s r) ~ 'True)
  => Proxy s -> Array r a -> Array (Slice s) a
slice s_ t = unsafeSlice s t
  where
    s = ((fmap . fmap) fromInteger . fromSing . singByProxy) s_


chunkItUp :: [V.Vector a] -> Int -> V.Vector a -> [V.Vector a]
chunkItUp acc i v = case null v of
                      False -> let (c, r) = V.splitAt i v
                               in chunkItUp (c : acc) i r
                      True -> acc

concatenate
  :: forall s r t a
  . (SingI s, SingI r, SingI t, (IsValidConcat s t r) ~ 'True)
  => Proxy s -> Array r a  -> Array t a -> Array (Concatenate s t r) a
concatenate s_ r t = Array . V.concat $ (concat . transpose) [rm, tm]
  where
    s = (fromInteger . fromSing . singByProxy) s_
    rm = chunkItUp [] (product $ drop s $ shape t) $ flattenArray t
    tm =  chunkItUp [] (product $ drop s $ shape r) $ flattenArray r
    
