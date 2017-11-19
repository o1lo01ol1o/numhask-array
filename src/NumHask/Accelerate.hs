{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | safe-typed n-dimensional arrays with Accelerate arrays under the hood
module NumHask.Accelerate where

import Data.Array.Accelerate.Array.Sugar (listToShape)
import Data.Array.Accelerate.LLVM.Native (run)
import Data.Singletons
import Data.Singletons.TypeLits
import GHC.Exts
import GHC.Show
import NumHask.Prelude hiding (All, Map)
import NumHask.Shape
import qualified Data.Array.Accelerate as A

type family NatsToShape (ns :: [Nat]) where
  NatsToShape '[] = A.Z
  NatsToShape (x:xs) = NatsToShape xs A.:. Int

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedLists
-- >>> :set -XTypeFamilies
-- >>> let a = [1..24] :: ArrayAcc '[2,3,4] Int
-- >>> let v = [1,2,3] :: ArrayAcc '[3] Int
-- >>> a
-- Array (Z :. 4 :. 3 :. 2) [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24]

newtype ArrayAcc (r :: [Nat]) a = ArrayAcc (A.Acc (A.Array (NatsToShape r) a))

instance forall (r :: [Nat]). (SingI r) => HasShape (ArrayAcc r) where
  type Shape (ArrayAcc r) = [Int]
  shape _ = fmap fromIntegral (fromSing (sing :: Sing r))

instance
    ( SingI r
    , Num a
    , A.Elt a
    , A.Shape (NatsToShape r)
    ) => IsList (ArrayAcc (r :: [Nat]) a) where
  type Item (ArrayAcc r a) = a
  fromList l = ArrayAcc $ A.use $ A.fromList (listToShape sh) l
    where
      sh = fmap fromIntegral (fromSing (sing :: Sing r))
  toList (ArrayAcc a) = A.toList $ run a

instance
    ( Show a
    , A.Elt a
    , SingI r
    , A.Shape (NatsToShape r)
    ) => Show (ArrayAcc r a) where
  show (ArrayAcc l) = GHC.Show.show $ run l

instance
    ( Eq a
    , A.Elt a
    , SingI r
    , A.Shape (NatsToShape r)
    , Eq (NatsToShape r)
    ) => Eq (ArrayAcc r a) where
  (==) (ArrayAcc a) (ArrayAcc b) = run a == run b

bin :: (A.Elt a, A.Shape (NatsToShape r)) =>
    (A.Exp a -> A.Exp a -> A.Exp a) -> ArrayAcc r a -> ArrayAcc r a -> ArrayAcc r a
bin f (ArrayAcc a) (ArrayAcc b) = ArrayAcc (A.zipWith f a b)

{-
singleton ::
    ( SingI r
    , A.Elt a
    , Num a
    , A.Shape (NatsToShape r)
    ) => [a] -> ArrayAcc (r :: [Nat]) a
singleton a = ArrayAcc $ A.use $ A.fromList (listToShape sh) a
    where
      sh = fmap fromIntegral (fromSing (sing :: Sing r))
-}

-- Exp additive instances
instance
    ( A.Num a
    , AdditiveMagma a) =>
    AdditiveMagma (A.Exp a) where
    plus = (A.+)

instance
    ( A.Num a
    , AdditiveUnital a) =>
    AdditiveUnital (A.Exp a) where
    zero = A.constant zero

instance
    ( A.Num a
    , AdditiveAssociative a) =>
    AdditiveAssociative (A.Exp a)

instance
    ( A.Num a
    , AdditiveCommutative a) =>
    AdditiveCommutative (A.Exp a)

instance
    ( A.Num a
    , Additive a) =>
    Additive (A.Exp a)

instance
    ( A.Num a
    , AdditiveInvertible a) =>
    AdditiveInvertible (A.Exp a) where
    negate = A.negate

instance
    ( A.Num a
    , AdditiveGroup a) =>
    AdditiveGroup (A.Exp a)

-- Exp multiplivcative instances
instance
    ( A.Num a
    , MultiplicativeMagma a) =>
    MultiplicativeMagma (A.Exp a) where
    times = (A.*)

instance
    ( A.Num a
    , MultiplicativeUnital a) =>
    MultiplicativeUnital (A.Exp a) where
    one = A.constant one

instance
    ( A.Num a
    , MultiplicativeAssociative a) =>
    MultiplicativeAssociative (A.Exp a)

instance
    ( A.Num a
    , MultiplicativeCommutative a) =>
    MultiplicativeCommutative (A.Exp a)

instance
    ( A.Num a
    , Multiplicative a) =>
    Multiplicative (A.Exp a)

instance
    ( A.Num a
    , Fractional (A.Exp a)
    , MultiplicativeInvertible a) =>
    MultiplicativeInvertible (A.Exp a) where
    recip = A.recip

instance
    ( A.Num a
    , Fractional (A.Exp a)
    , MultiplicativeGroup a) =>
    MultiplicativeGroup (A.Exp a)


-- ArrayAcc additive instances
instance
    ( A.Shape (NatsToShape r)
    , SingI r
    , A.Num a
    , AdditiveMagma a
    ) =>
    AdditiveMagma (ArrayAcc r a) where
    plus = bin plus

instance
    ( A.Shape (NatsToShape r)
    , SingI r
    , A.Num a
    , AdditiveUnital a
    ) =>
    AdditiveUnital (ArrayAcc r a) where
    zero = ArrayAcc $ A.use (A.fromList (listToShape sh) (repeat zero))
      where
        sh = fmap fromIntegral (fromSing (sing :: Sing r))

instance
    ( A.Shape (NatsToShape r)
    , SingI r
    , A.Num a
    , AdditiveAssociative a
    ) =>
    AdditiveAssociative (ArrayAcc r a)

instance
    ( A.Shape (NatsToShape r)
    , SingI r
    , A.Num a
    , AdditiveCommutative a
    ) =>
    AdditiveCommutative (ArrayAcc r a)

instance
    ( A.Shape (NatsToShape r)
    , SingI r
    , A.Num a
    , Additive a
    ) =>
    Additive (ArrayAcc r a)

instance
    ( A.Shape (NatsToShape r)
    , SingI r
    , A.Num a
    , AdditiveInvertible a
    ) =>
    AdditiveInvertible (ArrayAcc r a) where
    negate (ArrayAcc a) = ArrayAcc $ A.map A.negate a

instance
    ( A.Shape (NatsToShape r)
    , SingI r
    , A.Num a
    , AdditiveGroup a
    ) =>
    AdditiveGroup (ArrayAcc r a)

-- $additive tests
-- >>> let m = [0..] :: ArrayAcc '[2,3] Int
-- >>> m
-- Array (Z :. 3 :. 2) [0,1,2,3,4,5]
--
-- >>> m+zero
-- Array (Z :. 3 :. 2) [0,1,2,3,4,5]
--
-- >>> m+m
-- Array (Z :. 3 :. 2) [0,2,4,6,8,10]
--
-- >>> m-m == zero
-- True




