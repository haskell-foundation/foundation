{-# LANGUAGE OverloadedStrings #-}
module Test.Foundation.Bits
    ( tests
    ) where

import Foundation.Bits
import Imports
import Foundation
import Foundation.Numerical

tests = testGroup "Bits"
    [ testProperty "round-up" $ \(Positive m) n' -> n' >= 1 ==>
        let n = 2 ^ ((n' `mod` 30) :: Word)
            md = alignRoundUp m n
         in (md `mod` n) == 0 && md >= m
    ]

