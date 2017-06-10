{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE OverloadedStrings   #-}
module Test.Foundation.String
    ( testStringRefs
    ) where

import Foundation
import Foundation.String
import Foundation.String.ASCII (AsciiString)
import Control.Exception
import Data.Either

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
    , testGroup "replace" [
          testCase "indices 'aa' 'bb' == (0, [])" $ do
            indices "aa" "bb" @?= []
        , testCase "indices 'aa' 'aabbccabbccEEaaaaabb' is correct" $ do
            indices "aa" "aabbccabbccEEaaaaabb" @?= [Offset 0,Offset 13,Offset 15]
        , testCase "indices 'aa' 'aaccaadd' is correct" $ do
            indices "aa" "aaccaadd" @?= [Offset 0,Offset 4]
        , testCase "replace '' 'bb' 'foo' raises an error" $ do
            (res :: Either SomeException String) <- try (evaluate $ replace "" "bb" "foo")
            assertBool "Expecting an error to be thrown, but it did not." (isLeft res)
        , testCase "replace 'aa' 'bb' '' == ''" $ do
            replace "aa" "bb" "" @?= ""
        , testCase "replace 'aa' '' 'aabbcc' == 'aabbcc'" $ do
            replace "aa" "" "aabbcc" @?= "bbcc"
        , testCase "replace 'aa' 'bb' 'aa' == 'bb'" $ do
            replace "aa" "bb" "aa" @?= "bb"
        , testCase "replace 'aa' 'bb' 'aabb' == 'bbbb'" $ do
            replace "aa" "bb" "aabb" @?= "bbbb"
        , testCase "replace 'aa' 'bb' 'aaccaadd' == 'bbccbbdd'" $ do
            replace "aa" "bb" "aaccaadd" @?= "bbccbbdd"
        , testCase "replace 'aa' 'LongLong' 'aaccaadd' == 'LongLongccLongLongdd'" $ do
            replace "aa" "LongLong" "aaccaadd" @?= "LongLongccLongLongdd"
        , testCase "replace 'aa' 'bb' 'aabbccabbccEEaaaaabb' == 'bbbbccabbccEEbbbbabb'" $ do
            replace "aa" "bb" "aabbccabbccEEaaaaabb" @?= "bbbbccabbccEEbbbbabb"
        , testCase "replace '😀' '😺' '😀 🙂 😉 😌' == '😺 🙂 😉 😌'" $ do
            replace "😀" "😺" "😀 🙂 😉 😌" @?= "😺 🙂 😉 😌"
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
                    let (c1,c2) = splitAt (CountOf r) c
                     in c1 : loop rs c2
                [] ->
                    loop randomInts c
