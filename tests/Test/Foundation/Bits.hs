{-# LANGUAGE OverloadedStrings #-}
module Test.Foundation.Bits
    ( tests
    ) where

import Foundation.Bits
import Imports
import Foundation

tests = testGroup "Bits"
    [ testProperty "round-up" $ \(Positive m) n' -> (n' >= 1 && n' < 8) ==>
        let n = 2 ^ (n' :: Word)
            md = alignRoundUp m n
         in (md `mod` n) == 0 && md >= m
    ]

