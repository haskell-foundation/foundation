-- |
-- Module: Test.Data.ASCII
--

{-# LANGUAGE NoImplicitPrelude #-}

module Test.Data.ASCII
    ( genAsciiChar
    ) where

import Foundation
import Foundation.Foreign
import Test.Tasty.QuickCheck

-- | a better generator for unicode Character
genAsciiChar :: Gen CChar
genAsciiChar = toEnum <$> choose (1, 127)
