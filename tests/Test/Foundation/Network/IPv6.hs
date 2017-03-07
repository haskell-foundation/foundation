{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Foundation.Network.IPv6
    ( testNetworkIPv6
    ) where

import Foundation
import Foundation.Network.IPv6

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

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
    , testGroup "parse"
        [ testCase "::"  $ assert $ fromTuple (0,0,0,0,0,0,0,0) == fromString "::"
        , testCase "::1" $ assert $ fromTuple (0,0,0,0,0,0,0,1) == fromString "::1"
        , testCase "2001:DB8::8:800:200C:417A" $ assert $ fromTuple (0x2001,0xDB8,0,0,0x8,0x800,0x200c,0x417a) == fromString "2001:DB8::8:800:200C:417A"
        , testCase "FF01::101" $ assert $ fromTuple (0xff01,0,0,0,0,0,0,0x101) == fromString "FF01::101"
        , testCase "::13.1.68.3" $ assertEq (fromTuple (0,0,0,0,0,0,0x0d01,0x4403)) (fromString "::13.1.68.3")
        , testCase "::FFFF:129.144.52.38" $ assertEq (fromTuple (0,0,0,0,0,0xffff,0x8190,0x3426)) (fromString "::FFFF:129.144.52.38")
        , testCase "0::FFFF:129.144.52.38" $ assertEq (fromTuple (0,0,0,0,0,0xffff,0x8190,0x3426)) (fromString "0::FFFF:129.144.52.38")
        , testCase "0:0::FFFF:129.144.52.38" $ assertEq (fromTuple (0,0,0,0,0,0xffff,0x8190,0x3426)) (fromString "0:0::FFFF:129.144.52.38")
        ]
    ]

assertEq a b
    | a /= b = error $ show a <> " /= " <> show b
    | otherwise = return ()
