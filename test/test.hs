{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Test.Tasty (TestName, TestTree, testGroup, defaultMain)
import Test.Tasty.QuickCheck
import Test.DocTest
import NumHask.Prelude
import NumHask.Naperian
import NumHask.Tensor

main :: IO ()
main = do
    doctest ["src/Tensor.lhs", "src/Slicer.lhs"]
    defaultMain tests

tests :: TestTree
tests =
    testGroup "NumHask.Tensor"
    [ testsNInt
    , testsNShow
    ]

data LawArity a =
    Nonary Bool |
    Unary (a -> Bool) |
    Binary (a -> a -> Bool) |
    Ternary (a -> a -> a -> Bool) |
    Ornary (a -> a -> a -> a -> Bool) |
    Failiary (a -> Property)

data LawArity2 a b =
    Unary2 (a -> Bool) |
    Binary2 (a -> b -> Bool) |
    Ternary2 (a -> a -> b -> Bool) |
    Ternary2' (a -> b -> b -> Bool) |
    Failiary2 (a -> Property)

type Law a = (TestName, LawArity a)

type Law2 a b = (TestName, LawArity2 a b)

testLawOf  :: (Arbitrary a, Show a) => [a] -> Law a -> TestTree
testLawOf _ (name, Nonary f) = testProperty name f
testLawOf _ (name, Unary f) = testProperty name f
testLawOf _ (name, Binary f) = testProperty name f
testLawOf _ (name, Ternary f) = testProperty name f
testLawOf _ (name, Ornary f) = testProperty name f
testLawOf _ (name, Failiary f) = testProperty name f

testLawOf2  :: (Arbitrary a, Show a, Arbitrary b, Show b) =>
    [(a,b)] -> Law2 a b -> TestTree
testLawOf2 _ (name, Unary2 f) = testProperty name f
testLawOf2 _ (name, Binary2 f) = testProperty name f
testLawOf2 _ (name, Ternary2 f) = testProperty name f
testLawOf2 _ (name, Ternary2' f) = testProperty name f
testLawOf2 _ (name, Failiary2 f) = testProperty name f

testsNInt :: TestTree
testsNInt = testGroup "Tensor [2,3,2] Int"
    [ testGroup "Additive" $ testLawOf ([]::[Tensor [2,3,2] Int]) <$>
      additiveLaws
    , testGroup "Additive Group" $ testLawOf ([]::[Tensor [2,3,2] Int]) <$>
      additiveGroupLaws
    , testGroup "Multiplicative" $ testLawOf ([]::[Tensor [2,3,2] Int]) <$>
      multiplicativeLaws
    , testGroup "Distribution" $ testLawOf ([]::[Tensor [2,3,2] Int])
      <$> distributionLaws
    , testGroup "Additive Module" $ testLawOf2 ([]::[(Tensor [2,3,2] Int, Int)]) <$>
      additiveModuleLaws
    , testGroup "Additive Group Module" $ testLawOf2 ([]::[(Tensor [2,3,2] Int, Int)]) <$>
      additiveGroupModuleLaws
    , testGroup "Multiplicative Module" $ testLawOf2 ([]::[(Tensor [2,3,2] Int, Int)]) <$>
      multiplicativeModuleLaws
    , testGroup "Additive Basis" $ testLawOf ([]::[Tensor [2,3,2] Int]) <$>
      additiveBasisLaws
    , testGroup "Additive Group Basis" $ testLawOf ([]::[Tensor [2,3,2] Int]) <$>
      additiveGroupBasisLaws
    , testGroup "Multiplicative Basis" $ testLawOf ([]::[Tensor [2,3,2] Int]) <$>
      multiplicativeBasisLaws
    ]

testsNShow :: TestTree
testsNShow = testGroup "NRep Int"
    [ testProperty "ok arbitrary" (const True :: SomeTensor Int -> Bool)
    ]

additiveLaws ::
    ( Eq a
    , Additive a
    ) => [Law a]
additiveLaws =
    [ ( "associative: (a + b) + c = a + (b + c)"
      , Ternary (\a b c -> (a + b) + c == a + (b + c)))
    , ("left id: zero + a = a", Unary (\a -> zero + a == a))
    , ("right id: a + zero = a", Unary (\a -> a + zero == a))
    , ("commutative: a + b == b + a", Binary (\a b -> a + b == b + a))
    ]

additiveGroupLaws ::
    ( Eq a
    , AdditiveGroup a
    ) => [Law a]
additiveGroupLaws =
    [ ("minus: a - a = zero", Unary (\a -> (a - a) == zero))
    , ("negate minus: negate a == zero - a", Unary (\a -> negate a == zero - a))
    , ("negate cancel: negate a + a == zero", Unary (\a -> negate a + a == zero))
    ]

multiplicativeLaws ::
    ( Eq a
    , Multiplicative a
    ) => [Law a]
multiplicativeLaws =
    [ ( "associative: (a * b) * c = a * (b * c)"
      , Ternary (\a b c -> (a * b) * c == a * (b * c)))
    , ("left id: one * a = a", Unary (\a -> one * a == a))
    , ("right id: a * one = a", Unary (\a -> a * one == a))
    , ("commutative: a * b == b * a", Binary (\a b -> a * b == b * a))
    ]

distributionLaws ::
    ( Eq a
    , Distribution a
    ) => [Law a]
distributionLaws =
    [ ("annihilation: a * zero == zero", Unary (\a -> a `times` zero == zero))
    , ("left distributivity: a * (b + c) == a * b + a * c"
      , Ternary (\a b c -> a `times` (b + c) == a `times` b + a `times` c))
    , ("right distributivity: (a + b) * c == a * c + b * c"
      , Ternary (\a b c -> (a + b) `times` c == a `times` c + b `times` c))
    ]

additiveModuleLaws ::
    ( Eq (r a)
    , Epsilon a
    , Epsilon (r a)
    , AdditiveModule r a
    ) => [Law2 (r a) a]
additiveModuleLaws =
    [ 
      ("additive module associative: (a + b) .+ c ≈ a + (b .+ c)"
        , Ternary2 (\a b c -> (a + b) .+ c ≈ a + (b .+ c)))
    , ("additive module commutative: (a + b) .+ c ≈ (a .+ c) + b"
        , Ternary2 (\a b c -> (a + b) .+ c ≈ (a .+ c) + b))
    , ("additive module unital: a .+ zero == a"
        , Unary2 (\a -> a .+ zero == a))
    , ("module additive equivalence: a .+ b ≈ b +. a"
        , Binary2 (\a b -> a .+ b ≈ b +. a))
    ]

additiveGroupModuleLaws ::
    ( Eq (r a)
    , Epsilon a
    , Epsilon (r a)
    , Naperian r
    , AdditiveGroupModule r a
    ) => [Law2 (r a) a]
additiveGroupModuleLaws =
    [ 
      ("additive group module associative: (a + b) .- c ≈ a + (b .- c)"
        , Ternary2 (\a b c -> (a + b) .- c ≈ a + (b .- c)))
    , ("additive group module commutative: (a + b) .- c ≈ (a .- c) + b"
        , Ternary2 (\a b c -> (a + b) .- c ≈ (a .- c) + b))
    , ("additive group module unital: a .- zero == a"
        , Unary2 (\a -> a .- zero == a))
    , ("additive group module basis unital: a .- zero ≈ pureRep a"
        , Binary2 (\a b -> b -. (a-a) ≈ pureRep b))
    , ("module additive group equivalence: a .- b ≈ negate b +. a"
        , Binary2 (\a b -> a .- b ≈ negate b +. a))
    ]

multiplicativeModuleLaws ::
    ( Eq (r a)
    , Epsilon a
    , Epsilon (r a)
    , Multiplicative (r a)
    , MultiplicativeModule r a
    ) => [Law2 (r a) a]
multiplicativeModuleLaws =
    [ ("multiplicative module associative: (a * b) .* c ≈ a * (b .* c)"
        , Ternary2 (\a b c -> (a * b) .* c ≈ a * (b .* c)))
    , ("multiplicative module commutative: (a * b) .* c ≈ (a .* c) * b"
        , Ternary2 (\a b c -> (a * b) .* c ≈ a * (b .* c)))
    , ("multiplicative module unital: a .* one == a"
        , Unary2 (\a -> a .* one == a))
    , ("module right distribution: (a + b) .* c ≈ (a .* c) + (b .* c)"
        , Ternary2 (\a b c -> (a + b) .* c ≈ (a .* c) + (b .* c)))
    , ("module left distribution: c *. (a + b) ≈ (c *. a) + (c *. b)"
        , Ternary2 (\a b c -> c *. (a + b) ≈ (c *. a) + (c *. b)))
    , ("annihilation: a .* zero == zero", Unary2 (\a -> a .* zero == zero))
    , ("module multiplicative equivalence: a .* b ≈ b *. a"
        , Binary2 (\a b -> a .* b ≈ b *. a))
    ]

additiveBasisLaws ::
    ( Eq (r a)
    , Epsilon (r a)
    , AdditiveBasis r a
    ) => [Law (r a)]
additiveBasisLaws =
    [ ( "associative: (a .+. b) .+. c ≈ a .+. (b .+. c)"
      , Ternary (\a b c -> (a .+. b) .+. c ≈ a .+. (b .+. c)))
    , ("left id: zero .+. a = a", Unary (\a -> zero .+. a == a))
    , ("right id: a .+. zero = a", Unary (\a -> a .+. zero == a))
    , ("commutative: a .+. b == b .+. a", Binary (\a b -> a .+. b == b .+. a))
    ]

additiveGroupBasisLaws ::
    ( Eq (r a)
    , AdditiveGroupBasis r a
    ) => [Law (r a)]
additiveGroupBasisLaws =
    [ ("minus: a .-. a = pureRep zero", Unary (\a -> (a .-. a) == pureRep zero))
    ]

multiplicativeBasisLaws ::
    ( Eq (r a)
    , Multiplicative (r a)
    , MultiplicativeBasis r a
    ) => [Law (r a)]
multiplicativeBasisLaws =
    [ ("associative: (a .*. b) .*. c == a .*. (b .*. c)"
      , Ternary (\a b c -> (a .*. b) .*. c == a .*. (b .*. c)))
    , ("left id: one .*. a = a", Unary (\a -> one .*. a == a))
    , ("right id: a .*. one = a", Unary (\a -> a .*. one == a))
    , ("commutative: a .*. b == b .*. a", Binary (\a b -> a .*. b == b * a))
    ]
