{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Foundation
import Foundation.Check
import Foundation.String.Read

testAdditive :: forall a . (Show a, Eq a, Additive a, Arbitrary a) => Proxy a -> Test
testAdditive _ = Group "Additive"
    [ Property "eq"             $ azero === (azero :: a)
    , Property "a + azero == a" $ \(v :: a)     -> v + azero === v
    , Property "azero + a == a" $ \(v :: a)     -> azero + v === v
    , Property "a + b == b + a" $ \(v1 :: a) v2 -> v1 + v2 === v2 + v1
    ]

main = defaultMain $ Group "foundation"
    [ Group "Numerical"
        [ Group "Int"
            [ testAdditive (Proxy :: Proxy Int)
            ]
        , Group "Word64"
            [ testAdditive (Proxy :: Proxy Word64)
            ]
        ]
    , Group "String"
        [ Property "Reading-integer" $ \i -> readInteger (show i) == Just i
        ]
    ]
