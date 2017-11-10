{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | safe-typed n-dimensional arrays with Accelerate arrays under the hood
module NumHask.ArrayAcc where

import Data.Singletons
import Data.Singletons.TypeLits
import GHC.Exts
import GHC.Show
import NumHask.Prelude hiding (All, Map)
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Array.Representation as AR (listToShape, Shape)
import Data.Array.Accelerate.LLVM.Native (run)

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
-- [[[1, 2, 3, 4],
--   [5, 6, 7, 8],
--   [9, 10, 11, 12]],
--  [[13, 14, 15, 16],
--   [17, 18, 19, 20],
--   [21, 22, 23, 24]]]

newtype ArrayAcc (r :: [Nat]) a = ArrayAcc (A.Acc (A.Array (NatsToShape r) a))

instance forall (r :: [Nat]). (SingI r) => HasShape (ArrayAcc r) where
  type Shape (ArrayAcc r) = [Int]
  shape _ = fmap fromIntegral (fromSing (sing :: Sing r))

instance
    ( SingI r
    , Num a
    , A.Elt a
    , AR.Shape (NatsToShape r)
    , A.Shape (NatsToShape r)
    ) => IsList (ArrayAcc (r :: [Nat]) a) where
  type Item (ArrayAcc r a) = a
  fromList l = ArrayAcc $ A.use $ A.fromList (AR.listToShape sh) l
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
