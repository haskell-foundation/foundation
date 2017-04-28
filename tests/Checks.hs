{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Main where


import Foundation
import Foundation.Array
import Foundation.Foreign
import Foundation.List.DList
import Foundation.Primitive
import Foundation.Check
import Foundation.String
import Foundation.String.Read
import qualified Prelude
import Data.Ratio

import Test.Checks.Property.Collection

testAdditive :: forall a . (Enum a, Show a, Eq a, Typeable a, Additive a, Arbitrary a) => Proxy a -> Test
testAdditive _ = Group "Additive"
    [ Property "eq"             $ azero === (azero :: a)
    , Property "a + azero == a" $ \(v :: a)     -> v + azero === succ v
    , Property "azero + a == a" $ \(v :: a)     -> azero + v === v
    , Property "a + b == b + a" $ \(v1 :: a) v2 -> v1 + v2 === v2 + v1
    ]

readFloatingExact' :: String -> Maybe (Bool, Natural, Word, Maybe Int)
readFloatingExact' str = readFloatingExact str (\s x y z -> Just (s,x,y,z))

doubleEqualApprox :: Double -> Double -> PropertyCheck
doubleEqualApprox d1 d2 = (propertyCompare pName1 (<) (negate lim) d) `propertyAnd` (propertyCompare pName2 (<) d lim)
  where
        d = d2 - d1

        pName1 = show (negate lim) <> " < " <> show d2 <> " - " <> show d1 <> " (== " <> show d <> " )"
        pName2 = show d1 <> " - " <> show d2 <> " (== " <> show d <> " )" <> " < " <> show lim
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
                , Property "case0"         $ readFloatingExact' "124890" === Just (False, 124890, 0, Nothing)
                , Property "case1"         $ readFloatingExact' "-123.1" === Just (True, 1231, 1, Nothing)
                , Property "case2"         $ readFloatingExact' "10001.001" === Just (False, 10001001, 3, Nothing)
{-
                , Property "any"           $ \s i (v :: Word8) n ->
                                                let (integral,floating) = i `divMod` (10^v)
                                                let vw = integralUpsize v :: Word
                                                    sfloat = show n
                                                    digits = integralCast (length sfloat) + vw
                                                 in readFloatingExact' ((if s then "-" else "") <> show i <> "." <> replicate vw '0' <> sfloat) === Just (s, i, Just (digits, n), Nothing)
-}
                ]
            , Group "Double"
                [ Property "case1" $ readDouble "96152.5" === Just 96152.5
                , Property "case2" $ maybe (propertyFail "Nothing") (doubleEqualApprox 1.2300000000000002e102) $ readDouble "1.2300000000000002e102"
                , Property "case3" $ maybe (propertyFail "Nothing") (doubleEqualApprox 0.00001204) $ readDouble "0.00001204"
                , Property "case4" $ maybe (propertyFail "Nothing") (doubleEqualApprox 2.5e12) $ readDouble "2.5e12"
                , Property "case5" $ maybe (propertyFail "Nothing") (doubleEqualApprox 6.0e-4) $ readDouble "6.0e-4"
                , Property "case6" $ maybe (propertyFail "Nothing") ((===) (-31.548)) $ readDouble "-31.548"
                , Property "case7" $ readDouble "1e100000000" === Just (1/0)
                , Property "Prelude.read" $ \(d :: Double) -> case readDouble (show d) of
                                                                  Nothing -> propertyFail "Nothing"
                                                                  Just d' -> d' `doubleEqualApprox` (Prelude.read $ toList $ show d)
                ]
            , Group "rational"
                [ Property "case1" $ readRational "124.098" === Just (124098 % 1000)
                ]
            ]
        , Group "conversion"
            [ Property "lower" $ lower "This is MY test" === "this is my test"
            , Property "upper" $ upper "This is MY test" === "THIS IS MY TEST"
            ]
        ]
    , collectionProperties "DList a" (Proxy :: Proxy (DList Word8)) arbitrary
    , Group "Array"
      [ Group "Block"
        [ collectionProperties "Block(W8)"  (Proxy :: Proxy (Block Word8))  arbitrary
        , collectionProperties "Block(W16)" (Proxy :: Proxy (Block Word16)) arbitrary
        , collectionProperties "Block(W32)" (Proxy :: Proxy (Block Word32)) arbitrary
        , collectionProperties "Block(W64)" (Proxy :: Proxy (Block Word64)) arbitrary
        , collectionProperties "Block(I8)"  (Proxy :: Proxy (Block Int8))   arbitrary
        , collectionProperties "Block(I16)" (Proxy :: Proxy (Block Int16))  arbitrary
        , collectionProperties "Block(I32)" (Proxy :: Proxy (Block Int32))  arbitrary
        , collectionProperties "Block(I64)" (Proxy :: Proxy (Block Int64))  arbitrary
        , collectionProperties "Block(F32)" (Proxy :: Proxy (Block Float))  arbitrary
        , collectionProperties "Block(F64)" (Proxy :: Proxy (Block Double)) arbitrary
        , collectionProperties "Block(CChar)"  (Proxy :: Proxy (Block CChar))  (CChar <$> arbitrary)
        , collectionProperties "Block(CUChar)" (Proxy :: Proxy (Block CUChar)) (CUChar <$> arbitrary)
        , collectionProperties "Block(BE W16)" (Proxy :: Proxy (Block (BE Word16))) (toBE <$> arbitrary)
        , collectionProperties "Block(BE W32)" (Proxy :: Proxy (Block (BE Word32))) (toBE <$> arbitrary)
        , collectionProperties "Block(BE W64)" (Proxy :: Proxy (Block (BE Word64))) (toBE <$> arbitrary)
        , collectionProperties "Block(LE W16)" (Proxy :: Proxy (Block (LE Word16))) (toLE <$> arbitrary)
        , collectionProperties "Block(LE W32)" (Proxy :: Proxy (Block (LE Word32))) (toLE <$> arbitrary)
        , collectionProperties "Block(LE W64)" (Proxy :: Proxy (Block (LE Word64))) (toLE <$> arbitrary)
        ]
      , Group "Unboxed"
        [ collectionProperties "UArray(W8)"  (Proxy :: Proxy (UArray Word8))  arbitrary
        , collectionProperties "UArray(W16)" (Proxy :: Proxy (UArray Word16)) arbitrary
        , collectionProperties "UArray(W32)" (Proxy :: Proxy (UArray Word32)) arbitrary
        , collectionProperties "UArray(W64)" (Proxy :: Proxy (UArray Word64)) arbitrary
        , collectionProperties "UArray(I8)"  (Proxy :: Proxy (UArray Int8))   arbitrary
        , collectionProperties "UArray(I16)" (Proxy :: Proxy (UArray Int16))  arbitrary
        , collectionProperties "UArray(I32)" (Proxy :: Proxy (UArray Int32))  arbitrary
        , collectionProperties "UArray(I64)" (Proxy :: Proxy (UArray Int64))  arbitrary
        , collectionProperties "UArray(F32)" (Proxy :: Proxy (UArray Float))  arbitrary
        , collectionProperties "UArray(F64)" (Proxy :: Proxy (UArray Double)) arbitrary
        , collectionProperties "UArray(CChar)"  (Proxy :: Proxy (UArray CChar))  (CChar <$> arbitrary)
        , collectionProperties "UArray(CUChar)" (Proxy :: Proxy (UArray CUChar)) (CUChar <$> arbitrary)
        , collectionProperties "UArray(BE W16)" (Proxy :: Proxy (UArray (BE Word16))) (toBE <$> arbitrary)
        , collectionProperties "UArray(BE W32)" (Proxy :: Proxy (UArray (BE Word32))) (toBE <$> arbitrary)
        , collectionProperties "UArray(BE W64)" (Proxy :: Proxy (UArray (BE Word64))) (toBE <$> arbitrary)
        , collectionProperties "UArray(LE W16)" (Proxy :: Proxy (UArray (LE Word16))) (toLE <$> arbitrary)
        , collectionProperties "UArray(LE W32)" (Proxy :: Proxy (UArray (LE Word32))) (toLE <$> arbitrary)
        , collectionProperties "UArray(LE W64)" (Proxy :: Proxy (UArray (LE Word64))) (toLE <$> arbitrary)
        ]
      , Group "Boxed"
        [ collectionProperties "Array(W8)"  (Proxy :: Proxy (Array Word8))  arbitrary
        , collectionProperties "Array(W16)" (Proxy :: Proxy (Array Word16)) arbitrary
        , collectionProperties "Array(W32)" (Proxy :: Proxy (Array Word32)) arbitrary
        , collectionProperties "Array(W64)" (Proxy :: Proxy (Array Word64)) arbitrary
        , collectionProperties "Array(I8)"  (Proxy :: Proxy (Array Int8))   arbitrary
        , collectionProperties "Array(I16)" (Proxy :: Proxy (Array Int16))  arbitrary
        , collectionProperties "Array(I32)" (Proxy :: Proxy (Array Int32))  arbitrary
        , collectionProperties "Array(I64)" (Proxy :: Proxy (Array Int64))  arbitrary
        , collectionProperties "Array(F32)" (Proxy :: Proxy (Array Float))  arbitrary
        , collectionProperties "Array(F64)" (Proxy :: Proxy (Array Double)) arbitrary
        , collectionProperties "Array(Int)" (Proxy :: Proxy (Array Int))  arbitrary
        , collectionProperties "Array(Int,Int)" (Proxy :: Proxy (Array (Int,Int)))  arbitrary
        , collectionProperties "Array(Integer)" (Proxy :: Proxy (Array Integer)) arbitrary
        , collectionProperties "Array(CChar)"   (Proxy :: Proxy (Array CChar))  (CChar <$> arbitrary)
        , collectionProperties "Array(CUChar)"  (Proxy :: Proxy (Array CUChar)) (CUChar <$> arbitrary)
        , collectionProperties "Array(BE W16)"  (Proxy :: Proxy (Array (BE Word16))) (toBE <$> arbitrary)
        , collectionProperties "Array(BE W32)"  (Proxy :: Proxy (Array (BE Word32))) (toBE <$> arbitrary)
        , collectionProperties "Array(BE W64)"  (Proxy :: Proxy (Array (BE Word64))) (toBE <$> arbitrary)
        , collectionProperties "Array(LE W16)"  (Proxy :: Proxy (Array (LE Word16))) (toLE <$> arbitrary)
        , collectionProperties "Array(LE W32)"  (Proxy :: Proxy (Array (LE Word32))) (toLE <$> arbitrary)
        , collectionProperties "Array(LE W64)"  (Proxy :: Proxy (Array (LE Word64))) (toLE <$> arbitrary)
        ]
      ]
    , Group "ChunkedUArray"
      [ Group "Unboxed"
        [ collectionProperties "ChunkedUArray(W8)"  (Proxy :: Proxy (ChunkedUArray Word8))  arbitrary
        , collectionProperties "ChunkedUArray(W16)" (Proxy :: Proxy (ChunkedUArray Word16)) arbitrary
        , collectionProperties "ChunkedUArray(W32)" (Proxy :: Proxy (ChunkedUArray Word32)) arbitrary
        , collectionProperties "ChunkedUArray(W64)" (Proxy :: Proxy (ChunkedUArray Word64)) arbitrary
        , collectionProperties "ChunkedUArray(I8)"  (Proxy :: Proxy (ChunkedUArray Int8))   arbitrary
        , collectionProperties "ChunkedUArray(I16)" (Proxy :: Proxy (ChunkedUArray Int16))  arbitrary
        , collectionProperties "ChunkedUArray(I32)" (Proxy :: Proxy (ChunkedUArray Int32))  arbitrary
        , collectionProperties "ChunkedUArray(I64)" (Proxy :: Proxy (ChunkedUArray Int64))  arbitrary
        , collectionProperties "ChunkedUArray(F32)" (Proxy :: Proxy (ChunkedUArray Float))  arbitrary
        , collectionProperties "ChunkedUArray(F64)" (Proxy :: Proxy (ChunkedUArray Double)) arbitrary
        , collectionProperties "ChunkedUArray(BE W16)" (Proxy :: Proxy (ChunkedUArray (BE Word16))) (toBE <$> arbitrary)
        , collectionProperties "ChunkedUArray(BE W32)" (Proxy :: Proxy (ChunkedUArray (BE Word32))) (toBE <$> arbitrary)
        , collectionProperties "ChunkedUArray(BE W64)" (Proxy :: Proxy (ChunkedUArray (BE Word64))) (toBE <$> arbitrary)
        , collectionProperties "ChunkedUArray(LE W16)" (Proxy :: Proxy (ChunkedUArray (LE Word16))) (toLE <$> arbitrary)
        , collectionProperties "ChunkedUArray(LE W32)" (Proxy :: Proxy (ChunkedUArray (LE Word32))) (toLE <$> arbitrary)
        , collectionProperties "ChunkedUArray(LE W64)" (Proxy :: Proxy (ChunkedUArray (LE Word64))) (toLE <$> arbitrary)
        ]
      ]
    ]
