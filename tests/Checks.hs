{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Foundation
import Foundation.Check

testAdditive :: forall a . (Eq a, Additive a, Arbitrary a) => Proxy a -> Test
testAdditive _ = Group "Additive"
    [ Unit "eq"                 $ azero === azero
    , Property "a + azero == a" $ \(v :: a)     -> v + azero === v
    , Property "azero + a == a" $ \(v :: a)     -> azero + v === v
    , Property "a + b == b + a" $ \(v1 :: a) v2 -> v1 + v2 === v2 + v1
    ]

main = defaultMain $ Group "foundation"
    [ testAdditive (Proxy :: Proxy Int)
    ]
