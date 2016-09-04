-- |
-- Module: Test.Data.ASCII
--

{-# LANGUAGE NoImplicitPrelude #-}

module Test.Data.ASCII
    ( genAsciiChar
    ) where

import Foundation
import Test.Tasty.QuickCheck

-- | a better generator for unicode Character
genAsciiChar :: Gen Char
genAsciiChar = toEnum <$> choose (1, 128)
