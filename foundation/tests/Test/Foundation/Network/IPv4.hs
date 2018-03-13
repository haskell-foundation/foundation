{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Foundation.Network.IPv4
    ( testNetworkIPv4
    ) where

import Foundation
import Foundation.Network.IPv4
import Foundation.Check

import Test.Data.Network
import Test.Foundation.Storable

-- | test property equality for the given Collection
testEquality :: Gen IPv4 -> Test
testEquality genElement = Group "equality"
    [ Property "x == x" $ forAll genElement (\x -> x === x)
    , Property "x == y" $ forAll ((,) <$> genElement <*> genElement) $
        \(x,y) -> (toTuple x == toTuple y) === (x == y)
    ]

-- | test ordering
testOrdering :: Gen IPv4 -> Test
testOrdering genElement = Property "ordering" $
    forAll ((,) <$> genElement <*> genElement) $ \(x, y) ->
        (toTuple x `compare` toTuple y) === x `compare` y

testNetworkIPv4 :: Test
testNetworkIPv4 = Group "IPv4"
    [ Property "toTuple . fromTuple == id" $
        forAll genIPv4Tuple $ \x -> x === toTuple (fromTuple x)
    , Property "toString . fromString == id" $
        forAll genIPv4String $ \x -> x === toString (fromString $ toList x)
    , testEquality genIPv4
    , testOrdering genIPv4
    , testPropertyStorable      "Storable" (Proxy :: Proxy IPv4)
    , testPropertyStorableFixed "StorableFixed" (Proxy :: Proxy IPv4)
    ]
