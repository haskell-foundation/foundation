{-# LANGUAGE NoImplicitPrelude #-}
module Imports
    ( module X
    , testCase
    , testGroup
    , assertFailure
    ) where

import Foundation
import Test.Tasty              as X hiding (testGroup)
import Test.Tasty.QuickCheck   as X
import Test.Tasty.HUnit        as X hiding (testCase, assert, assertFailure)
import Test.QuickCheck.Monadic as X

import qualified Test.Tasty       as Y
import qualified Test.Tasty.HUnit as Y

testCase :: String -> X.Assertion -> X.TestTree
testCase x f = Y.testCase (toList x) f

assertFailure :: String -> X.Assertion
assertFailure x = Y.assertFailure (toList x)

testGroup :: String -> [TestTree] -> TestTree
testGroup x l = Y.testGroup (toList x) l
