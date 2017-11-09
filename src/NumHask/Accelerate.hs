{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | safe-typed n-dimensional arrays
module NumHask.Accelerate
  ( Array(..) 
  , SomeArray(..)
  , row
  , col
  , slice
  , index
  , foldAlong
  , mapAlong
  , concatenate
  , zipWith
  , transpose
  , squeeze
  , (><)
  , mmult
  , fromList
  ) where

import Data.Distributive
import Data.Functor.Rep
import Data.Promotion.Prelude
import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import GHC.Exts
import GHC.Show
import Data.Coerce (coerce)
import NumHask.Array.Constraints
import NumHask.Prelude hiding (All, Map, (><), mmult, show, row, col, zipWith, transpose)
import qualified Data.Vector as V
import qualified NumHask.Prelude as P
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Array.Representation as AR (listToShape, Shape)

type family NatsToShape (ns :: [Nat]) where
  NatsToShape '[] = A.Z
  NatsToShape (x:xs) = NatsToShape xs A.:. Int

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedLists
-- >>> :set -XTypeFamilies
-- >>> let a = [1..24] :: Array '[2,3,4] Int
-- >>> let v = [1,2,3] :: Array '[3] Int

-- | an n-dimensional array where shape is specified at the type level
-- The main purpose of this, beyond safe typing, is to supply the Representable instance with an initial object.
--
-- >>> a
-- [[[1, 2, 3, 4],
--   [5, 6, 7, 8],
--   [9, 10, 11, 12]],
--  [[13, 14, 15, 16],
--   [17, 18, 19, 20],
--   [21, 22, 23, 24]]]

newtype Array (r :: [Nat]) a = Array (A.Acc (A.Array (NatsToShape r) a))

instance (A.Arrays a) => Functor (A.Acc ) where
  fmap = A.map

instance Functor (Array r) where
    fmap :: (a -> b) -> Array r a -> Array r b
    fmap f (Array ar) = Array $ A.map ((A.lift1 f) :: (A.Exp (A.Plain a) -> A.Exp (A.Plain b))) ar


-- | an n-dimensional array where shape is specified at the value level
data SomeArray a =
  SomeArray [Int]
            (a)

-- | convert a 'Array' to a 'SomeArray', losing the type level shape
-- someArray :: (SingI r) => Array (r :: [Nat]) a -> SomeArray a
-- someArray n@(Array v) = SomeArray (shape n) v

instance forall (r :: [Nat]). (SingI r) => HasShape (Array r) where
  type Shape (Array r) = [Int]
  shape _ = fmap fromIntegral (fromSing (sing :: Sing r))

-- | convert from n-dim shape index to a flat index
--
-- >>> ind [2,3,4] [1,1,1]
-- 17
ind :: [Int] -> [Int] -> Int
ind ns xs = sum $ P.zipWith (*) xs (drop 1 $ scanr (*) 1 ns)

-- | convert from a flat index to a shape index
--
-- >>> unind [2,3,4] 17
-- [1,1,1]
unind :: [Int] -> Int -> [Int]
unind ns x =
  fst $
  foldr
    (\a (acc, r) ->
       let (d, m) = divMod r a
       in (m : acc, d))
    ([], x)
    ns

instance forall r. (SingI r) => Distributive (Array (r :: [Nat]))

instance forall (r :: [Nat]). (SingI r) => Representable (Array r)  where
  type Rep (Array r) = A.Exp Int
  tabulate f = Array $ A.indexed (AR.listToShape ns)
    where
      ns =
        case (sing :: Sing r) of
          SNil -> []
          (SCons x xs) -> fromIntegral <$> (fromSing x : fromSing xs)
  index (Array xs) rs = xs A.!! rs



-- | from flat list
instance ( SingI r
         -- , AR.Shape sh
         -- , A.Shape sh
         -- , A.Elt a
         -- , Num [Int]
         -- , Multiplicative [Int]
         -- , P.FromInteger [Int]
         ) =>
         IsList (Array (r :: [Nat]) a) where
  type Item (Array r a) = A.Exp a
  fromList l = Array $ A.use $ A.fromList (AR.listToShape n) l
    where
      n =
        case (sing :: Sing r) of
          SNil -> 1
          (SCons x xs') ->
            product $ fromIntegral <$> (fromSing x : fromSing xs')
  --toList (Array v) = A.toList v

-- instance (Show a) => Show (SomeArray a) where
--   show r@(SomeArray l _) = go (length l) r
--     where
--       go n r'@(SomeArray l' v') =
--         case length l' of
--           0 -> show $ V.head v'
--           1 -> "[" ++ intercalate ", " (show <$> GHC.Exts.toList v') ++ "]"
--           x ->
--             "[" ++
--             intercalate
--               (",\n" ++ replicate (n - x + 1) ' ')
--               (go n <$> flatten1 r') ++
--             "]"

-- | convert the top layer of a SomeArray to a [SomeArray]
-- flatten1 :: SomeArray a -> [SomeArray a]
-- flatten1 (SomeArray rep v) =
--   (\s -> SomeArray (drop 1 rep) (V.unsafeSlice (s * l) l v)) <$> ss
--   where
--     (n, l) =
--       case rep of
--         [] -> (0, 1)
--         x:r -> (x, product r)
--     ss = take n [0 ..]

-- instance (Show a, SingI r) => Show (Array (r :: [Nat]) a) where
--   show = show . someArray

-- ** Operations
-- | outer product
--
-- todo: reconcile with numhask version
--
-- >>> v >< v
-- [[1, 2, 3],
--  [2, 4, 6],
--  [3, 6, 9]]
(><) ::
     forall (r :: [Nat]) (s :: [Nat]) a sh rsh ssh.
     ( CRing a
     , SingI r
     , SingI s
     , SingI (r :++ s)
     , A.Shape sh
     , A.Shape rsh
     , A.Shape ssh
     , A.Elt a
     , Num (A.Exp a)
     , rsh ~ (A.Z A.:. Int)
     , ssh ~ (A.Z A.:. Int)
     , sh ~ ((A.Z A.:. Int) A.:. Int)
     )
  => Array r a
  -> Array s a
  -> Array (r :++ s) a
(><) (Array m) (Array n) =
  let r = A.size m
      c = A.size n
      mn =
        A.zipWith
          (A.*)
          (A.replicate (A.lift $ A.Any A.:. r A.:. A.All) m)
          (A.replicate (A.lift $ A.Any A.:. A.All A.:. c) n)
  in Array mn

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
mmult ::
     forall m n k sh a.
     ( Semiring a
     , Num a
     , CRing a
     , KnownNat m
     , KnownNat n
     , KnownNat k
     , sh ~ ((A.Z A.:. Int) A.:. Int)
     , Additive (A.Exp a)
     , A.Elt a
     , Multiplicative (A.Exp a)
     )
  => Array '[ m, k] (A.Acc (A.Array sh a))
  -> Array '[ k, n] (A.Acc (A.Array sh a))
  -> Array '[ m, n] (A.Acc (A.Array sh a))
mmult (Array arr) (Array brr) =
  Array $ A.fold1 (+) (A.zipWith (*) arrRepl brrRepl)
  where
    A.Z A.:. rowsA A.:. _ =
      A.unlift (A.shape arr) :: A.Z A.:. A.Exp Int A.:. A.Exp Int
    A.Z A.:. _ A.:. colsB =
      A.unlift (A.shape brr) :: A.Z A.:. A.Exp Int A.:. A.Exp Int
    arrRepl = A.replicate (A.lift $ A.Z A.:. A.All A.:. colsB A.:. A.All) arr
    brrRepl =
      A.replicate
        (A.lift $ A.Z A.:. rowsA A.:. A.All A.:. A.All)
        (A.transpose brr)

-- | extract the row of a matrix
row ::
     forall i a m n shm shv.
     ( KnownNat m
     , KnownNat n
     , KnownNat i
     , (i :< m) ~ 'True
     , A.Elt a
     , A.Shape shv
     , A.Shape shm
     , shm ~ (A.Z A.:. Int A.:. Int)
     , shv ~ (A.Z A.:. Int)
     )
  => Proxy i
  -> Array '[ m, n] (A.Acc (A.Array shm a))
  -> Array '[ n] (A.Acc (A.Array shv a))
row i_ = unsafeRow $ A.constant i
  where
    i = (fromIntegral . fromSing . singByProxy) i_

unsafeRow ::
     forall a m n shm shv.
     ( KnownNat m
     , KnownNat n
     , A.Elt a
     , A.Shape shv
     , A.Shape shm
     , shm ~ (A.Z A.:. Int A.:. Int)
     , shv ~ (A.Z A.:. Int)
     )
  => A.Exp Int
  -> Array '[ m, n] (A.Acc (A.Array shm a))
  -> Array '[ n] (A.Acc (A.Array shv a))
unsafeRow i t@(Array a) = Array $ A.slice a (A.lift (A.Z A.:. i A.:. A.All))


-- | extract the column of a matrix
col ::
     forall j a m n shv shm.
     ( KnownNat m
     , KnownNat n
     , KnownNat j
     , (j :< n) ~ 'True
     , A.Elt a
     , A.Shape shv
     , A.Shape shm
     , shm ~ (A.Z A.:. Int A.:. Int)
     , shv ~ (A.Z A.:. Int)
     )
  => Proxy j
  -> Array '[ m, n] (A.Acc (A.Array shm a))
  -> Array '[ m] (A.Acc (A.Array shv a))
col j_ = unsafeCol $ A.constant j
  where
    j = (fromIntegral . fromSing . singByProxy) j_

unsafeCol ::
     forall a m n shm shv.
     ( KnownNat m
     , KnownNat n
     , A.Elt a
     -- , A.Shape shv
     -- , A.Shape shm
     -- , shm ~ (A.Z A.:. Int A.:. Int)
     -- , shv ~ (A.Z A.:. Int)
     )
  => A.Exp Int
  -> Array '[ m, n] a
  -> Array '[ m] a
  
unsafeCol  i t@(Array a) = Array $ A.slice a (A.lift (A.Z A.:. A.All A.:. i))


-- |
--
-- >>> unsafeIndex a [0,2,1]
-- 10
unsafeIndex :: (SingI r, A.Elt a
     , A.Shape sh
     , sh ~ A.DIM0
     , AR.Shape (A.Exp shm)
     , A.Shape shm) => Array r (A.Acc (A.Array shm a)) -> [Int] -> (A.Acc (A.Array sh a))
unsafeIndex t@(Array a) i =  A.unit (a A.! AR.listToShape i)

-- |
-- FIXME: api had to change here, we have to pass the computed indicies to unsafeslice so that it can extract them from the flattend A.Array
-- >>> unsafeSlice [[0,1],[2],[1,2]] a :: Array '[2,1,2] Int
-- [[[10, 11]],
--  [[22, 23]]]
unsafeSlice :: (SingI r, A.Elt a) => Array r (A.Acc (A.Array (A.Z A.:. Int) Int)) -> Array r (A.Acc (A.Array (A.Z A.:. Int) a)) -> Array r0 (A.Acc (A.Array (A.Z A.:. Int) a))
unsafeSlice (Array s) (Array t) = Array p
  where
    ft = A.flatten t
    p = A.gather s ft


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
slice ::
     forall s r a. (SingI s, SingI r, And (ZipWith AllLTSym0 s r) ~ 'True, A.Elt a)
  => Proxy s
  -> Array r (A.Acc (A.Array (A.Z A.:. Int) a))
  -> Array (Slice s) (A.Acc (A.Array (A.Z A.:. Int) a))
slice s_ r = unsafeSlice src r
  where
    s = ((fmap . fmap) fromInteger . fromSing . singByProxy) s_
    d = shape r
    c =  [ ind d i | i <- sequence s] :: [Int]
    src = Array $ A.use ( A.fromList (A.Z A.:. (P.length c) ) c)

zipWith :: (a ~ A.Acc (A.Array sh b), A.Shape sh, A.Elt b ) => (A.Exp b -> A.Exp b -> A.Exp b) -> Array s a -> Array s a -> Array s a
zipWith fn (Array a) (Array b) = Array $ A.zipWith fn a b

dimToHead ::  Int -> [Int] -> [Int]
dimToHead i is = rs
  where
    rs = case splitAt i is of
           (a,x:xs) -> x:a ++ xs
           (a,xs) -> a ++ xs
           
headToDim ::  Int -> [Int] -> [Int]
headToDim i is = rs
  where
    (h : ts) = is
    rs = case splitAt (i - 1) ts of
           (nh,x:xs) -> (nh ++ [x, h] ++ xs)
           (nh,xs) -> nh ++ (h : xs)

sumHeads :: [Int] -> [Int] -> [Int]
sumHeads a b = s
  where
    ha:as = a
    hb:bs = b
    s = case as == bs of
          True -> (ha + hb) : as
          False -> error "Concatenation requires all dimensions to match except one!"

-- |
--
-- >>> foldAlong (Proxy :: Proxy 1) (\_ -> ([0..3] :: Array '[4] Int)) a
-- [[0, 1, 2, 3],
--  [0, 1, 2, 3]]

--Fixme: API doesn't expect an initial value for fold but acc lets us have one.
--From accelerate: The first argument needs to be an associative function to enable an efficient parallel implementation. The initial element does not need to be an identity element of the combination function.
--There is also an acclerate Fold module analoguous to Foldl that allows an applicative interface to folds for efficient composition.
-- A.fold wants a (Exp a -> Exp a -> Exp a)` :\
-- foldAlong ::
--      forall s vw uvw uw w a.
--      ( SingI s
--      , SingI uvw
--      , uw ~ (Fold s uvw)
--      , w ~ (Drop 1 vw)
--      , vw ~ (TailModule s uvw)
--      )
--   => Proxy s
--   -> (Array vw a -> Array w a)
--   -> Array uvw a
--   -> Array uw a
-- foldAlong s_ f a@(Array v) =
--   Array $ A.fold f (A.constant 0) md
--   where
--     s = (fromInteger . fromSing . singByProxy) s_
--     md = A.backpermute (doreshape (A.shape v)) doreshape v

-- |
--
-- >>> mapAlong (Proxy :: Proxy 0) (\x -> NumHask.Array.zipWith (*) x x) a
-- [[[1, 4, 9, 16],
--   [25, 36, 49, 64],
--   [81, 100, 121, 144]],
--  [[169, 196, 225, 256],
--   [289, 324, 361, 400],
--   [441, 484, 529, 576]]]
-- mapAlong ::
--      forall s uvw vw a. (SingI s, SingI uvw, vw ~ (HeadModule s uvw))
--   => Proxy s
--   -> (Array vw a -> Array vw a)
--   -> Array uvw a
--   -> Array uvw a
-- mapAlong s_ f a@(Array v) =
--   Array $
--   V.concat
--     (foldl'
--        (\xs x ->
--           let (Array vx) = f (Array x)
--           in vx : xs)
--        []
--        md)
--   where
--     s = (fromInteger . fromSing . singByProxy) s_
--     md = chunkItUp [] (product $ drop s $ shape a) v

-- |
--
-- >>> concatenate (Proxy :: Proxy 2) a a
-- [[[1, 2, 3, 4, 1, 2, 3, 4],
--   [5, 6, 7, 8, 5, 6, 7, 8],
--   [9, 10, 11, 12, 9, 10, 11, 12]],
--  [[13, 14, 15, 16, 13, 14, 15, 16],
--   [17, 18, 19, 20, 17, 18, 19, 20],
--   [21, 22, 23, 24, 21, 22, 23, 24]]]
concatenate ::
     forall s r t a sh sh' ssh'. (SingI s, SingI r, SingI t, (IsValidConcat s t r) ~ 'True, AR.Shape sh, AR.Shape sh', AR.Shape ssh', A.Shape sh, A.Shape sh', A.Shape ssh')
  => Proxy s
  -> Array r (A.Acc (A.Array sh a))
  -> Array t (A.Acc (A.Array sh' a))
  -> Array (Concatenate s t r) (A.Acc (A.Array ssh' a))
concatenate s_ r@(Array vr) t@(Array vt) = Array rc
  where -- push axis to head, concat, and return to original shape
    s = (fromInteger . fromSing . singByProxy) s_
    rs = dimToHead s (shape r)
    ts = dimToHead s (shape t)
    ir = A.reshape (AR.listToShape rs) vr
    it = A.reshape (AR.listToShape ts) vt
    c = ir A.++ it
    rc =  A.reshape ( AR.listToShape $ headToDim s (sumHeads rs ts)) c


-- |
--
-- >>> NumHask.Array.transpose a
-- [[[1, 2],
--   [3, 4],
--   [5, 6]],
--  [[7, 8],
--   [9, 10],
--   [11, 12]],
--  [[13, 14],
--   [15, 16],
--   [17, 18]],
--  [[19, 20],
--   [21, 22],
--   [23, 24]]]
transpose ::
     forall s t a sh' sh. (t ~ Transpose s)
  => Array s (A.Acc (A.Array sh a))
  -> Array t (A.Acc (A.Array sh' a))
transpose a@(Array x) = Array $ A.backpermute (P.swap (A.shape x)) P.swap x

-- |
--
-- >>> let a = [1..24] :: Array '[2,1,3,4,1] Int
-- >>> a
-- [[[[[1],
--     [2],
--     [3],
--     [4]],
--    [[5],
--     [6],
--     [7],
--     [8]],
--    [[9],
--     [10],
--     [11],
--     [12]]]],
--  [[[[13],
--     [14],
--     [15],
--     [16]],
--    [[17],
--     [18],
--     [19],
--     [20]],
--    [[21],
--     [22],
--     [23],
--     [24]]]]]
-- >>> squeeze a
-- [[[1, 2, 3, 4],
--   [5, 6, 7, 8],
--   [9, 10, 11, 12]],
--  [[13, 14, 15, 16],
--   [17, 18, 19, 20],
--   [21, 22, 23, 24]]]
squeeze ::
     forall s t a. (t ~ Squeeze s)
  => Array s a
  -> Array t a
squeeze (Array x) = Array x

instance (SingI r, AdditiveMagma a) => AdditiveMagma (Array r  (A.Acc (A.Array sh a))) where
  plus = liftR2 plus

instance (SingI r, AdditiveUnital a) => AdditiveUnital (Array r  (A.Acc (A.Array sh a))) where
  zero = pureRep zero

instance (SingI r, AdditiveAssociative a) =>
         AdditiveAssociative (Array r  (A.Acc (A.Array sh a)))

instance (SingI r, AdditiveCommutative a) =>
         AdditiveCommutative (Array r  (A.Acc (A.Array sh a)))

instance (SingI r, AdditiveInvertible a) => AdditiveInvertible (Array r a) where
  negate = fmapRep negate

instance (SingI r, Additive a) => Additive (Array r a)

instance (SingI r, AdditiveGroup a) => AdditiveGroup (Array r a)

instance (SingI r, MultiplicativeMagma a) =>
         MultiplicativeMagma (Array r a) where
  times = liftR2 times

instance (SingI r, MultiplicativeUnital a) =>
         MultiplicativeUnital (Array r a) where
  one = pureRep one

instance (SingI r, MultiplicativeAssociative a) =>
         MultiplicativeAssociative (Array r a)

instance (SingI r, MultiplicativeCommutative a) =>
         MultiplicativeCommutative (Array r a)

instance (SingI r, MultiplicativeInvertible a) =>
         MultiplicativeInvertible (Array r a) where
  recip = fmapRep recip

instance (SingI r, Multiplicative a) => Multiplicative (Array r a)

instance (SingI r, MultiplicativeGroup a) =>
         MultiplicativeGroup (Array r a)

instance (SingI r, MultiplicativeMagma a, Additive a) =>
         Distribution (Array r a)

instance (SingI r, Semiring a) => Semiring (Array r a)

instance (SingI r, Ring a) => Ring (Array r a)

instance (SingI r, CRing a) => CRing (Array r a)

instance (SingI r, Field a) => Field (Array r a)

instance (SingI r, ExpField a) => ExpField (Array r a) where
  exp = fmapRep exp
  log = fmapRep log

instance (SingI r, BoundedField a) => BoundedField (Array r a) where
  isNaN f = or (fmapRep isNaN f)

instance (SingI r, Signed a) => Signed (Array r a) where
  sign = fmapRep sign
  abs = fmapRep abs

instance (ExpField a) => Normed (Array r a) a where
  size r = sqrt $ foldr (+) zero $ (** (one + one)) <$> r

instance (SingI r, Epsilon a) => Epsilon (Array r a) where
  nearZero f = and (fmapRep nearZero f)
  aboutEqual a b = and (liftR2 aboutEqual a b)

instance (SingI r, ExpField a) => Metric (Array r a) a where
  distance a b = size (a - b)

instance (SingI r, Integral a) => Integral (Array r a) where
  divMod a b = (d, m)
    where
      x = liftR2 divMod a b
      d = fmap fst x
      m = fmap snd x

instance (CRing a, Num a, Semiring a, SingI r) => Hilbert (Array r) a where
  a <.> b = sum $ liftR2 (*) a b
