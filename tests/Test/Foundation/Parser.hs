{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Foundation.Parser
  ( testParsers
  ) where

import Foundation
import Foundation.Parser

import Test.Tasty
import Test.Tasty.HUnit

data TestCaseRes a
    = TestCaseOk String a
    | TestCaseMore (Maybe String) (TestCaseRes a)
    | TestCaseFail
  deriving (Show)

parseTestCase :: (Show a, Eq a)
              => String
              -> Parser String a
              -> TestCaseRes a
              -> Assertion
parseTestCase buff parser = check (parse parser buff)
check :: (Show a, Eq a) => Result String a -> TestCaseRes a -> Assertion
check r e = case (r, e) of
    (ParseOK remain a, TestCaseOk eRemain ea) -> do
        assertEqual "remaining buffer" eRemain remain
        assertEqual "returned value" ea a
    (ParseMore fr, TestCaseMore mb res') -> check (fr mb) res'
    (ParseFail _, TestCaseFail) -> return ()
    _ -> assertFailure $ toList $
            "parseTestCase failed: "
                <> "expected: " <> show e <> " "
                <> "buf received: " <> show r

-- Some custom test cases
parseTestCases :: TestTree
parseTestCases = testGroup "units"
    [ testGroup "element"
        [ testCase "Ok" $ parseTestCase "a" (element 'a') (TestCaseOk "" ())
        , testCase "Fail" $ parseTestCase "b" (element 'a') TestCaseFail
        , testCase "MoreOk" $ parseTestCase "" (element 'a') (TestCaseMore (Just "a") (TestCaseOk "" ()))
        , testCase "MoreFail" $ parseTestCase "" (element 'a') (TestCaseMore Nothing TestCaseFail)
        ]
    , testGroup "elements"
        [ testCase "Ok" $ parseTestCase "abc" (elements "ab") (TestCaseOk "c" ())
        , testCase "Fail" $ parseTestCase "ac" (elements "ab") TestCaseFail
        , testCase "MoreOk" $ parseTestCase "a" (elements "abc") (TestCaseMore (Just "bc") (TestCaseOk "" ()))
        , testCase "MoreMoreOk" $ parseTestCase "a" (elements "abc") (TestCaseMore (Just "b") $ TestCaseMore (Just "c") (TestCaseOk "" ()))
        , testCase "MoreMoreFail" $ parseTestCase "a" (elements "abc") (TestCaseMore (Just "b") $ TestCaseMore Nothing TestCaseFail)
        ]
    , testGroup "anyElement"
        [ testCase "OK" $ parseTestCase "a"   anyElement (TestCaseOk "" 'a')
        , testCase "OkRemains" $ parseTestCase "abc" anyElement (TestCaseOk "bc" 'a')
        , testCase "MoreOk" $ parseTestCase ""    anyElement $ TestCaseMore (Just "abc")(TestCaseOk "bc" 'a')
        , testCase "MoreFail" $ parseTestCase ""    anyElement $ TestCaseMore Nothing TestCaseFail
        ]
    , testGroup "take"
        [ testCase "OK" $ parseTestCase "a" (take 1) (TestCaseOk "" "a")
        , testCase "OkRemains" $ parseTestCase "abc" (take 2) (TestCaseOk "c" "ab")
        , testCase "MoreOk" $ parseTestCase "" (take 2) $ TestCaseMore (Just "abc")(TestCaseOk "c" "ab")
        , testCase "MoreFail" $ parseTestCase "a" (take 2) $ TestCaseMore Nothing TestCaseFail
        ]
    , testGroup "takeWhile"
        [ testCase "OK" $ parseTestCase "a " (takeWhile (' ' /=)) (TestCaseOk " " "a")
        , testCase "OkRemains" $ parseTestCase "ab bc" (takeWhile (' ' /=)) (TestCaseOk " bc" "ab")
        , testCase "MoreOk" $ parseTestCase "ab" (takeWhile (' ' /=)) $ TestCaseMore (Just "cd ")(TestCaseOk " " "abcd")
        , testCase "MoreFail" $ parseTestCase "aa" (takeWhile (' ' /=)) $ TestCaseMore Nothing TestCaseFail
        ]
    , testGroup "takeAll"
        [ testCase "OK" $ parseTestCase "abc" takeAll (TestCaseMore Nothing $ TestCaseOk "" "abc")
        ]
    , testGroup "skip"
        [ testCase "OK" $ parseTestCase "a" (skip 1) (TestCaseOk "" ())
        , testCase "OkRemains" $ parseTestCase "abc" (skip 2) (TestCaseOk "c" ())
        , testCase "MoreOk" $ parseTestCase "" (skip 2) $ TestCaseMore (Just "abc")(TestCaseOk "c" ())
        , testCase "MoreFail" $ parseTestCase "a" (skip 2) $ TestCaseMore Nothing TestCaseFail
        ]
    , testGroup "skipWhile"
        [ testCase "OK" $ parseTestCase "a " (skipWhile (' ' /=)) (TestCaseOk " " ())
        , testCase "OkRemains" $ parseTestCase "ab bc" (skipWhile (' ' /=)) (TestCaseOk " bc" ())
        , testCase "MoreOk" $ parseTestCase "ab" (skipWhile (' ' /=)) $ TestCaseMore (Just "cd ")(TestCaseOk " " ())
        , testCase "MoreFail" $ parseTestCase "aa" (skipWhile (' ' /=)) $ TestCaseMore Nothing TestCaseFail
        ]
    , testGroup "skipAll"
        [ testCase "OK" $ parseTestCase "abc" skipAll (TestCaseMore Nothing $ TestCaseOk "abc" ())
        ]
    , testGroup "optional"
        [ testCase "Nothing" $ parseTestCase "aaa" (optional $ elements "bbb") (TestCaseOk "aaa" Nothing)
        , testCase "Just"    $ parseTestCase "aaa" (optional $ elements "a") (TestCaseOk "aa" (Just ()))
        ]
    , testGroup "many"
        [ testCase "many elements" $ parseTestCase "101010\0"
            (many ((element '1' >> pure True) <|> (element '0' >> pure False) ) )
            (TestCaseOk "\0" [True, False, True, False, True, False])
        ]
    ]

testParsers :: TestTree
testParsers = testGroup "Parsers"
    [ parseTestCases
    ]
