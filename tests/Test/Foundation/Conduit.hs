{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Foundation.Conduit
  ( testConduit
  ) where

import Foundation
import Foundation.Conduit
import Foundation.IO

import Imports

testConduit :: TestTree
testConduit = testGroup "Conduit"
    [ testCase "sourceHandle gives same data as readFile" testSourceFile
    , testCase "sourceHandle/sinkHandle copies data" testCopyFile
    ]
  where
    testSourceFile :: Assertion
    testSourceFile = do
        let fp = "foundation.cabal"
        arrs <- withFile fp ReadMode
            $ \h -> runConduit $ sourceHandle h .| sinkList
        arr <- readFile fp
        assertEqual "foundation.cabal contents" arr (mconcat arrs)

    testCopyFile :: Assertion
    testCopyFile = do
        let src = "foundation.cabal"
            dst = "temp-file" -- FIXME some temp file API?
        withFile src ReadMode $ \hin -> withFile dst WriteMode $ \hout ->
            runConduit $ sourceHandle hin .| sinkHandle hout
        orig <- readFile src
        new <- readFile dst
        assertEqual "copied foundation.cabal contents" orig new
