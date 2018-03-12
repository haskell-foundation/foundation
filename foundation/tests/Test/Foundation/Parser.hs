{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Foundation.Parser
  ( testParsers
  ) where

import           Foundation
import           Foundation.Parser
import qualified Foundation.Parser as P

import Test.Tasty
import Test.Tasty.HUnit

data TestCaseRes a
    = TestCaseOk String a
    | TestCaseMore String (TestCaseRes a)
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
    (ParseOk remain a, TestCaseOk eRemain ea) -> do
        assertEqual "remaining buffer" eRemain remain
        assertEqual "returned value" ea a
    (ParseMore fr, TestCaseMore mb res') -> check (fr mb) res'
    (ParseFailed _, TestCaseFail) -> return ()
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
        , testCase "MoreOk" $ parseTestCase "a" (element 'a' >> element 'a') (TestCaseMore "a" (TestCaseOk "" ()))
        , testCase "MoreFail" $ parseTestCase "a" (element 'a' >> element 'a') (TestCaseMore mempty TestCaseFail)
        ]
    , testGroup "elements"
        [ testCase "Ok" $ parseTestCase "abc" (elements "ab") (TestCaseOk "c" ())
        , testCase "Fail" $ parseTestCase "ac" (elements "ab") TestCaseFail
        , testCase "MoreOk" $ parseTestCase "a" (elements "abc") (TestCaseMore "bc" (TestCaseOk "" ()))
        , testCase "MoreMoreOk" $ parseTestCase "a" (elements "abc") (TestCaseMore "b" $ TestCaseMore "c" (TestCaseOk "" ()))
        , testCase "MoreMoreFail" $ parseTestCase "a" (elements "abc") (TestCaseMore "b" $ TestCaseMore mempty TestCaseFail)
        ]
    , testGroup "anyElement"
        [ testCase "OK" $ parseTestCase "a"   anyElement (TestCaseOk "" 'a')
        , testCase "OkRemains" $ parseTestCase "abc" anyElement (TestCaseOk "bc" 'a')
        , testCase "MoreOk" $ parseTestCase "a" (anyElement *> anyElement) $ TestCaseMore "abc" (TestCaseOk "bc" 'a')
        , testCase "MoreFail" $ parseTestCase "a" (anyElement <* anyElement) $ TestCaseMore mempty TestCaseFail
        ]
    , testGroup "take"
        [ testCase "OK" $ parseTestCase "a" (P.take 1) (TestCaseOk "" "a")
        , testCase "OkRemains" $ parseTestCase "abc" (P.take 2) (TestCaseOk "c" "ab")
        , testCase "MoreOk" $ parseTestCase "a" (P.take 2) $ TestCaseMore "bc" (TestCaseOk "c" "ab")
        , testCase "MoreFail" $ parseTestCase "a" (P.take 2) $ TestCaseMore mempty TestCaseFail
        ]
    , testGroup "takeWhile"
        [ testCase "OK" $ parseTestCase "a " (P.takeWhile (' ' /=)) (TestCaseOk " " "a")
        , testCase "OkRemains" $ parseTestCase "ab bc" (P.takeWhile (' ' /=)) (TestCaseOk " bc" "ab")
        , testCase "MoreOk" $ parseTestCase "ab" (P.takeWhile (' ' /=)) $ TestCaseMore "cd " (TestCaseOk " " "abcd")
        , testCase "MoreEmptyOK" $ parseTestCase "aa" (P.takeWhile (' ' /=)) $ TestCaseMore mempty (TestCaseOk "" "aa")
        ]
    , testGroup "takeAll"
        [ testCase "OK" $ parseTestCase "abc" takeAll (TestCaseMore mempty $ TestCaseOk "" "abc")
        ]
    , testGroup "skip"
        [ testCase "OK" $ parseTestCase "a" (skip 1) (TestCaseOk "" ())
        , testCase "OkRemains" $ parseTestCase "abc" (skip 2) (TestCaseOk "c" ())
        , testCase "MoreOk" $ parseTestCase "a" (skip 2) $ TestCaseMore "bc" (TestCaseOk "c" ())
        , testCase "MoreFail" $ parseTestCase "a" (skip 2) $ TestCaseMore mempty TestCaseFail
        ]
    , testGroup "skipWhile"
        [ testCase "OK" $ parseTestCase "a " (skipWhile (' ' /=)) (TestCaseOk " " ())
        , testCase "OkRemains" $ parseTestCase "ab bc" (skipWhile (' ' /=)) (TestCaseOk " bc" ())
        , testCase "MoreOk" $ parseTestCase "ab" (skipWhile (' ' /=)) $ TestCaseMore "cd " (TestCaseOk " " ())
        , testCase "MoreEmptyOk" $ parseTestCase "aa" (skipWhile (' ' /=)) $ TestCaseMore mempty (TestCaseOk "" ())
        ]
    , testGroup "skipAll"
        [ testCase "OK" $ parseTestCase "abc" skipAll (TestCaseMore mempty $ TestCaseOk "" ())
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
    , testGroup "parseOnly"
        [ testCase "takeWhile" $ case parseOnly (P.takeWhile (' ' /=)) ("abc" :: [Char]) of
            Right "abc" -> return ()
            _           -> error "failed"
        ]
    ]

testParsers :: TestTree
testParsers = testGroup "Parsers"
    [ parseTestCases
    ]
