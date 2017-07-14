-- |
-- Module: Test.Data.ASCII
--

{-# LANGUAGE NoImplicitPrelude #-}

module Test.Data.ASCII
    ( genAsciiChar
    ) where

import Foundation
import Test.Tasty.QuickCheck
import qualified Foundation.Primitive.Types.Char7 as C7

-- | a better generator for unicode Character
genAsciiChar :: Gen C7.Char7
genAsciiChar = C7.fromByteMask <$> choose (1, 127)
