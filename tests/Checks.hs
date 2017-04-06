{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Foundation
import Foundation.Primitive
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
        [ Group "reading" 
            [ Group "integer" 
                [ Property "empty"         $ readInteger "" === Nothing
                , Property "just-sign"     $ readInteger "-" === Nothing
                , Property "extra-content" $ readInteger "-123a" === Nothing
                , Property "any"           $ \i -> readInteger (show i) === Just i
                ]
            , Group "floating-exact"
                [ Property "empty"         $ readFloatingExact "" === Nothing
                , Property "just-sign"     $ readFloatingExact "-" === Nothing
                , Property "extra-content" $ readFloatingExact "-123a" === Nothing
                , Property "no-dot-after"  $ readFloatingExact "-123." === Nothing
                , Property "case1"         $ readFloatingExact "-123.1" === Just (-123, 0, 1)
                , Property "case2"         $ readFloatingExact "10001.001" === Just (10001, 2, 1)
                , Property "any"           $ \i (v :: Word8) n ->
                                                let vw = integralUpsize v
                                                    vwExpected = if n == 0 then 0 else vw
                                                 in readFloatingExact (show i <> "." <> replicate vw '0' <> show n) === Just (i, vwExpected, n)
                ]
            ]
        ]
    ]
