{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Foundation.Network.IPv6
    ( testNetworkIPv6
    ) where

import Foundation
import Foundation.Network.IPv6

import Test.Tasty
import Test.Tasty.QuickCheck

import Test.Data.Network
import Test.Foundation.Storable

-- | test property equality for the given Collection
testEquality :: Gen IPv6 -> TestTree
testEquality genElement = testGroup "equality"
    [ testProperty "x == x" $ forAll genElement (\x -> x === x)
    , testProperty "x == y" $ forAll ((,) <$> genElement <*> genElement) $
        \(x,y) -> (toTuple x == toTuple y) === (x == y)
    ]

-- | test ordering
testOrdering :: Gen IPv6 -> TestTree
testOrdering genElement = testProperty "ordering" $
    forAll ((,) <$> genElement <*> genElement) $ \(x, y) ->
        (toTuple x `compare` toTuple y) === x `compare` y

testNetworkIPv6 :: TestTree
testNetworkIPv6 = testGroup "IPv6"
    [ testProperty "toTuple . fromTuple == id" $
        forAll genIPv6Tuple $ \x -> x === toTuple (fromTuple x)
    , testProperty "toString . fromString == id" $
        forAll genIPv6String $ \x -> x === toString (fromString $ toList x)
    , testEquality genIPv6
    , testOrdering genIPv6
    , testPropertyStorable      "Storable" (Proxy :: Proxy IPv6)
    , testPropertyStorableFixed "StorableFixed" (Proxy :: Proxy IPv6)
    ]
