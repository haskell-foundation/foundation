{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
module Test.Foundation.Number
    ( testNumber
    , testNumberRefs
    ) where

import Imports
import Foundation
import Foundation.Numerical -- hiding (Positive)
import qualified Prelude

#if !(MIN_VERSION_base(4,8,0))
instance Arbitrary Natural where
    arbitrary = fromInteger <$> (arbitrary `suchThat` \i -> i >= 0)
#endif

testAddNullElementRight :: (Show a, Eq a, Additive a, Arbitrary a)
                        => Proxy a -> a -> Property
testAddNullElementRight _ a = a + azero === a
testAddNullElementLeft :: (Show a, Eq a, Additive a, Arbitrary a)
                       => Proxy a -> a -> Property
testAddNullElementLeft  _ a = azero + a === a
testAddCommutatif :: (Show a, Eq a, Additive a, Arbitrary a)
                  => Proxy a -> a -> a -> Property
testAddCommutatif _ a b = a + b === b + a

testMultiplyByIdentityRight :: (Show a, Eq a, Multiplicative a, Arbitrary a)
                            => Proxy a -> a -> Property
testMultiplyByIdentityRight _ a = a * midentity === a
testMultiplyByIdentityLeft :: (Show a, Eq a, Multiplicative a, Arbitrary a)
                           => Proxy a -> a -> Property
testMultiplyByIdentityLeft _ a = midentity * a === a

testDivMulPlusRest :: (Show a, Eq a, Integral a, IDivisible a, Arbitrary a)
                   => Proxy a -> a -> (NonZero a) -> Property
testDivMulPlusRest _ a (NonZero b) =
    a === (a `div` b) * b + (a `mod` b)

testAdditive :: (Show a, Eq a, Additive a, Arbitrary a)
             => Proxy a -> TestTree
testAdditive proxy = testGroup "Additive"
    [ testProperty "a + azero == a" (testAddNullElementRight proxy)
    , testProperty "azero + a == a" (testAddNullElementLeft proxy)
    , testProperty "a + b == b + a" (testAddCommutatif proxy)
    ]

testMultiplicative :: (Show a, Eq a, Multiplicative a, Arbitrary a)
                   => Proxy a -> TestTree
testMultiplicative proxy = testGroup "Multiplicative"
    [ testProperty "a * midentity == a" (testMultiplyByIdentityRight proxy)
    , testProperty "midentity * a == a" (testMultiplyByIdentityLeft proxy)
    ]

testDividible :: (Show a, Eq a, IsIntegral a, IDivisible a, Arbitrary a)
              => Proxy a -> TestTree
testDividible proxy = testGroup "Divisible"
    [ testProperty "(x `div` y) * y + (x `mod` y) == x" (testDivMulPlusRest proxy)
    ]

withP3 :: (Show a, Eq a, IsIntegral a, Additive a, Multiplicative a, Subtractive a, Difference a ~ a, Arbitrary a)
       => Proxy a -> (a -> a -> a -> Property) -> (a -> a -> a -> Property)
withP3 _ f = f

withP3Pos2 :: (Show a, Eq a, IsIntegral a, Difference a ~ a, Arbitrary a)
           => Proxy a -> (a -> Natural -> a -> Property) -> (a -> Natural -> a -> Property)
withP3Pos2 _ f = f

testOperatorPrecedence :: (Show a, Eq a, Prelude.Num a, IsIntegral a, Additive a, Subtractive a, Multiplicative a, Difference a ~ a, Arbitrary a) => Proxy a -> TestTree
testOperatorPrecedence proxy = testGroup "Precedence"
    [ testProperty "+ and - (1)" $ withP3 proxy $ \a b c -> (a + b - c) === ((a + b) - c)
    , testProperty "+ and - (2)" $ withP3 proxy $ \a b c -> (a - b + c) === ((a - b) + c)
    , testProperty "+ and * (1)" $ withP3 proxy $ \a b c -> (a + b * c) === (a + (b * c))
    , testProperty "+ and * (2)" $ withP3 proxy $ \a b c -> (a * b + c) === ((a * b) + c)
    , testProperty "- and * (1)" $ withP3 proxy $ \a b c -> (a - b * c) === (a - (b * c))
    , testProperty "- and * (2)" $ withP3 proxy $ \a b c -> (a * b - c) === ((a * b) - c)
    , testProperty "* and ^ (1)" $ withP3Pos2 proxy $ \a b c -> (a ^ b * c) === ((a ^ b) * c)
    , testProperty "* and ^ (2)" $ withP3Pos2 proxy $ \a c b -> (a * b ^ c) === (a * (b ^ c))
    ]


testNumber :: (Show a, Eq a, Prelude.Num a, IsIntegral a, Additive a, Multiplicative a, Subtractive a, Difference a ~ a, IDivisible a, Arbitrary a)
           => String -> Proxy a -> TestTree
testNumber name proxy = testGroup name
    [ testAdditive proxy
    , testMultiplicative proxy
    , testDividible proxy
    , testOperatorPrecedence proxy
    ]

testNumberRefs :: [TestTree]
testNumberRefs =
    [ testNumber "Int" (Proxy :: Proxy Int)
    , testNumber "Int8" (Proxy :: Proxy Int8)
    , testNumber "Int16" (Proxy :: Proxy Int16)
    , testNumber "Int32" (Proxy :: Proxy Int32)
    , testNumber "Int64" (Proxy :: Proxy Int64)
    , testNumber "Integer" (Proxy :: Proxy Integer)
    , testNumber "Word" (Proxy :: Proxy Word)
    , testNumber "Word8" (Proxy :: Proxy Word8)
    , testNumber "Word16" (Proxy :: Proxy Word16)
    , testNumber "Word32" (Proxy :: Proxy Word32)
    , testNumber "Word64" (Proxy :: Proxy Word64)
    ]
