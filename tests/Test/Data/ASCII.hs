-- |
-- Module: Test.Data.ASCII
--

module Test.Data.ASCII
    ( genAsciiChar
    ) where

import Test.Tasty.QuickCheck

-- | a better generator for unicode Character
genAsciiChar :: Gen Char
genAsciiChar = toEnum <$> choose (1, 128)
