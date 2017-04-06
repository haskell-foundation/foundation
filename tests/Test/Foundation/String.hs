{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE OverloadedStrings   #-}
module Test.Foundation.String
    ( testStringRefs
    ) where

import Foundation
import Foundation.Primitive
import Foundation.String
import Foundation.String.ASCII (AsciiString)
import Foundation.String.Read
import Foundation.Collection

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Test.Data.Unicode
import Test.Data.ASCII
import Test.Data.List
import Test.Foundation.Collection
import Test.Foundation.Encoding

testStringRefs :: TestTree
testStringRefs = testGroup "String"
    [ testGroup "UTF8" $
        [  testCollection "Sequential" (Proxy :: Proxy String) genUnicodeChar ]
        <> testStringCases
        <> [ testGroup "Encoding Sample0" (testEncodings sample0)
           , testGroup "Encoding Sample1" (testEncodings sample1)
           , testGroup "Encoding Sample2" (testEncodings sample2)
           ]
    , testGroup "ASCII" $
        [  testCollection "Sequential" (Proxy :: Proxy AsciiString) genAsciiChar
        ]
    , testGroup "Reading" $
        [ testProperty "Integer" $ \i -> readInteger (show i) === Just i
        , testProperty "any"     $ \i (v :: Word8) n ->
            let vw         = integralUpsize v
                vwExpected = if n == 0 then 0 else vw
             in readFloatingExact (show i <> "." <> replicate vw '0' <> show n) === Just (i, vwExpected, n)
        ]
    ]

testStringCases :: [TestTree]
testStringCases =
    [ testGroup "Validation"
        [ testProperty "fromBytes . toBytes == valid" $ \(LUString l) ->
            let s = fromList l
             in (fromBytes UTF8 $ toBytes UTF8 s) === (s, Nothing, mempty)
        , testProperty "Streaming" $ \(LUString l, randomInts) ->
            let wholeS  = fromList l
                wholeBA = toBytes UTF8 wholeS
                reconstruct (prevBa, errs, acc) ba =
                    let ba' = prevBa `mappend` ba
                        (s, merr, nextBa) = fromBytes UTF8 ba'
                     in (nextBa, merr : errs, s : acc)

                (remainingBa, allErrs, chunkS) = foldl reconstruct (mempty, [], []) $ chunks randomInts wholeBA
             in (catMaybes allErrs === []) .&&. (remainingBa === mempty) .&&. (mconcat (reverse chunkS) === wholeS)
        ]
    , testGroup "Cases"
        [ testGroup "Invalid-UTF8"
            [ testCase "ff" $ expectFromBytesErr ("", Just InvalidHeader, 0) (fromList [0xff])
            , testCase "80" $ expectFromBytesErr ("", Just InvalidHeader, 0) (fromList [0x80])
            , testCase "E2 82 0C" $ expectFromBytesErr ("", Just InvalidContinuation, 0) (fromList [0xE2,0x82,0x0c])
            , testCase "30 31 E2 82 0C" $ expectFromBytesErr ("01", Just InvalidContinuation, 2) (fromList [0x30,0x31,0xE2,0x82,0x0c])
            ]
        ]
    ]
  where
    expectFromBytesErr (expectedString,expectedErr,positionErr) ba = do
        let (s', merr, ba') = fromBytes UTF8 ba
        assertEqual "error" expectedErr merr
        assertEqual "remaining" (drop positionErr ba) ba'
        assertEqual "string" expectedString (toList s')

chunks :: Sequential c => RandomList -> c -> [c]
chunks (RandomList randomInts) = loop (randomInts <> [1..])
  where
    loop rx c
        | null c  = []
        | otherwise =
            case rx of
                r:rs ->
                    let (c1,c2) = splitAt r c
                     in c1 : loop rs c2
                [] ->
                    loop randomInts c
