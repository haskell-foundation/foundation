{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Foundation
import Foundation.Primitive
import Foundation.Check
import Foundation.String.Read
import qualified Prelude

testAdditive :: forall a . (Show a, Eq a, Additive a, Arbitrary a) => Proxy a -> Test
testAdditive _ = Group "Additive"
    [ Property "eq"             $ azero === (azero :: a)
    , Property "a + azero == a" $ \(v :: a)     -> v + azero === v
    , Property "azero + a == a" $ \(v :: a)     -> azero + v === v
    , Property "a + b == b + a" $ \(v1 :: a) v2 -> v1 + v2 === v2 + v1
    ]

readFloatingExact' :: String -> Maybe (Integer, Maybe (Word, Natural), Maybe Int)
readFloatingExact' s = readFloatingExact s (\x y z -> Just (x,y,z))

doubleEqualApprox :: Double -> Double -> PropertyCheck
doubleEqualApprox d1 d2 = (propertyCompare pName1 (<) (negate lim) d) `propertyAnd` (propertyCompare pName2 (>) lim d)
  where d = d2 - d1
        pName1 = show d1 <> " - " <> show d2 <> " < " <> show (negate lim)
        pName2 = show d1 <> " - " <> show d2 <> " > " <> show lim
        lim = 1.0e-8



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
                [ Property "empty"         $ readFloatingExact' "" === Nothing
                , Property "just-sign"     $ readFloatingExact' "-" === Nothing
                , Property "extra-content" $ readFloatingExact' "-123a" === Nothing
                , Property "no-dot-after"  $ readFloatingExact' "-123." === Nothing
                , Property "case1"         $ readFloatingExact' "-123.1" === Just (-123, Just (1, 1), Nothing)
                , Property "case2"         $ readFloatingExact' "10001.001" === Just (10001, Just (3, 1), Nothing)
                , Property "any"           $ \i (v :: Word8) n ->
                                                let vw = integralUpsize v :: Word
                                                    sfloat = show n
                                                    digits = integralCast (length sfloat) + vw
                                                 in readFloatingExact' (show i <> "." <> replicate vw '0' <> sfloat) === Just (i, Just (digits, n), Nothing)
                ]
            , Group "Double"
                [ Property "case1" $ readDouble "96152.5" === Just 96152.5
                , Property "case2" $ maybe (propertyFail "Nothing") (doubleEqualApprox 123.0e100) $ readDouble "123e100"
                , Property "case3" $ maybe (propertyFail "Nothing") (doubleEqualApprox 5.719543847727751e-37) $ readDouble "5.719543847727751E-37"
                , Property "Prelude.read" $ \(d :: Double) -> case readDouble (show d) of
                                                                  Nothing -> propertyFail "Nothing"
                                                                  Just d' -> d' `doubleEqualApprox` (Prelude.read $ toList $ show d)
                ]
            ]
        ]
    ]
