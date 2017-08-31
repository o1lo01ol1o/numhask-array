{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module NumHask.Array.Constraints (IsValidConcat,Squeeze, Concatenate, IsValidTranspose, DimShuffle ) where
import           Data.Singletons.Prelude       hiding (Max)
import           Data.Singletons.Prelude.Base  (MapSym0, MapSym1)
import           Data.Singletons.Prelude.List  ((:!!$), All, Concat, Drop,
                                                Filter, Foldl, Head, Insert,
                                                Last, Length, Map, Maximum,
                                                Minimum, Nub, SplitAt, Sum,
                                                Take, ZipWith, ZipWithSym2)
import           Data.Singletons.Prelude.Ord   (Max, MaxSym0, MaxSym1, MaxSym2)
import           Data.Singletons.Prelude.Tuple (Curry, Fst, Snd)
import           Data.Singletons.TH            (promote)
import           Data.Singletons.TypeLits      (KnownNat, Nat)
import qualified Protolude                     as P


instance P.Eq Nat where
  x == y = P.not (x P./= y)
  x /= y = P.not (x P.== y)

instance P.Ord Nat where
  x > y = ((P.not (x P./= y) ) P.&& (P.not (x P.< y)))
  x < y = ((P.not (x P./=y) ) P.&& (P.not (x P.> y)))
  x <= y = (((x P.== y) ) P.|| (P.not (x P.> y)))
  x >= y = (((x P.== y) ) P.|| (P.not (x P.< y)))



(!!) :: [a] -> Nat -> a
[] !! _ = P.error "Data.Singletons.List.!!: index too large"
(x:xs) !! n =
    if n P.== 0
        then x
        else xs !!
             (n P.- 1)


type family DropDim d a :: [b] where
        DropDim 0 xs = Drop 1 xs
        DropDim d xs =
                     (Take (d :- 1) (Fst (SplitAt d xs))) :++ (Snd (SplitAt d xs))

type family IsValidConcat i (a::[Nat]) (b::[Nat]) :: P.Bool where
  IsValidConcat _ '[] _ = P.False
  IsValidConcat _ _ '[] = P.False
  IsValidConcat i a b = And (ZipWith (:==$) (DropDim i a) (DropDim i b))

type family Squeeze (a::[Nat]) where
  Squeeze '[] = '[]
  Squeeze a = Filter ((:/=$$) 1 ) a

type family IsValidTranspose (p :: [Nat]) (a :: [Nat]) :: P.Bool
     where
        IsValidTranspose p a =
                             (Minimum p :>= 0) :&&
                               (Minimum a :>= 0) :&&
                                 (Sum a :== Sum p) :&& (Length p) :== (Length a)

type family AddDimension (d :: Nat) t :: [Nat] where
        AddDimension d t = Insert d t

type family Concatenate i (a::[Nat]) (b::[Nat]) :: [Nat] where
  Concatenate i a b = (Take (i :- 1) (Fst (SplitAt i a))) :++
                        ('[(Head (Drop (i :- 1) a)) :+ (Head (Drop (i :-1) b))]) :++
                          (Snd (SplitAt i b))


$(promote
      [d|

  dimShuffle :: P.Eq a => [a] -> [Nat] -> [a]
  dimShuffle _ [] = []
  dimShuffle [] _ = []
  dimShuffle (x : xs) (b : bs)
    = if b P.== 0 then x : dimShuffle xs bs else
        (xs !! (b P.- 1)) : dimShuffle xs bs
  |])

