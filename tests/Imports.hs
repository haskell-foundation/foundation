{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Imports
    ( module X
    , testCase
    , testGroup
    , testProperty
    , assertFailure
    , Positive(..)
    , NonZero(..)
    , (===?)
    , diffList
    ) where

import Foundation
import Test.Tasty              as X hiding (testGroup)
import Test.Tasty.QuickCheck   as X (Arbitrary(..), Gen, suchThat, Property, (===), (==>)
                                    , Small(..), QuickCheckTests(..)
                                    , forAll, vectorOf, frequency, choose, elements)
#if MIN_VERSION_tasty_quickcheck(0,8,4)
import Test.Tasty.QuickCheck   as X (QuickCheckVerbose(..))
#endif
import Test.Tasty.HUnit        as X hiding (testCase, assert, assertFailure)
import Test.QuickCheck.Monadic as X

import qualified Test.Tasty            as Y
import qualified Test.Tasty.QuickCheck as Y
import qualified Test.Tasty.HUnit      as Y

testCase :: String -> X.Assertion -> X.TestTree
testCase x f = Y.testCase (toList x) f

assertFailure :: String -> X.Assertion
assertFailure x = Y.assertFailure (toList x)

testGroup :: String -> [TestTree] -> TestTree
testGroup x l = Y.testGroup (toList x) l

testProperty :: Y.Testable a => String -> a -> TestTree
testProperty x l = Y.testProperty (toList x) l

newtype Positive a = Positive { getPositive :: a }
    deriving ( Eq, Ord, Show, Enum)
instance (Ord a, Integral a, Arbitrary a) => Arbitrary (Positive a) where
    arbitrary = Positive <$> (arbitrary `suchThat` \i -> i > 0)
    shrink (Positive x) = [ Positive x' | x' <- shrink x , x' > 0 ]
newtype NonZero a = NonZero { getNonZero :: a }
    deriving (Eq, Ord, Show, Enum)
instance (Ord a, Integral a, Arbitrary a) => Arbitrary (NonZero a) where
    arbitrary = NonZero <$> (arbitrary `suchThat` \i -> i /= 0)
    shrink (NonZero x) = [ NonZero x' | x' <- shrink x , x' /= 0 ]

infix 4 ===?

(===?) :: (Eq a, Show a) => [a] -> [a] -> Property
x ===? y =
    Y.counterexample (toList msg) (x == y)
  where
    msg = diffList x y

diffList :: (Eq a, Show a) => [a] -> [a] -> String
diffList a b = "left : " <> show a <> "\nright: " <> show b <> "\ndiff : " <> show d
  where
    d = loop 0 a b
    loop :: (Eq a, Show a) => Int -> [a] -> [a] -> String
    loop _ [] []       = "internal error : list is equal"
    loop n l1@(_:_) [] = "offset=" <> show n <> " extra left=" <> show l1
    loop n [] l2@(_:_) = "offset=" <> show n <> " extra right=" <> show l2
    loop n l1@(x:xs) l2@(y:ys)
        | x == y    = loop (n+1) xs ys
        | otherwise = "offset=" <> show n <> " left=" <> show l1 <> " right= " <> show l2

