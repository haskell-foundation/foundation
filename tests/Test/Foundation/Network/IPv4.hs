{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Foundation.Network.IPv4
    ( testNetworkIPv4
    ) where

import Foundation
import Foundation.Network.IPv4

import Test.Tasty
import Test.Tasty.QuickCheck

import Test.Data.Network
import Test.Foundation.Storable

-- | test property equality for the given Collection
testEquality :: Gen IPv4 -> TestTree
testEquality genElement = testGroup "equality"
    [ testProperty "x == x" $ forAll genElement (\x -> x === x)
    , testProperty "x == y" $ forAll ((,) <$> genElement <*> genElement) $
        \(x,y) -> (toTuple x == toTuple y) === (x == y)
    ]

-- | test ordering
testOrdering :: Gen IPv4 -> TestTree
testOrdering genElement = testProperty "ordering" $
    forAll ((,) <$> genElement <*> genElement) $ \(x, y) ->
        (toTuple x `compare` toTuple y) === x `compare` y

testNetworkIPv4 :: TestTree
testNetworkIPv4 = testGroup "IPv4"
    [ testProperty "toTuple . fromTuple == id" $
        forAll genIPv4Tuple $ \x -> x === toTuple (fromTuple x)
    , testProperty "toString . fromString == id" $
        forAll genIPv4String $ \x -> x === toString (fromString $ toList x)
    , testEquality genIPv4
    , testOrdering genIPv4
    , testPropertyStorable      "Storable" (Proxy :: Proxy IPv4)
    , testPropertyStorableFixed "StorableFixed" (Proxy :: Proxy IPv4)
    ]
