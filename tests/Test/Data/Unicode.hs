-- |
-- Module: Test.Data.Unicode
--

{-# LANGUAGE TypeFamilies #-}

module Test.Data.Unicode
    ( LUString(..)
    , genUnicodeChar
    ) where

import Test.Tasty.QuickCheck
import Control.Monad (replicateM)
import Foundation

-- | a better generator for unicode Character
genUnicodeChar :: Gen Char
genUnicodeChar =
    toEnum <$> oneof
        [ choose (1, 0xff)
        , choose (0x100, 0x1000)
        , choose (0x100, 0x10000)
        , choose (0x1, 0x1000)
        ]

-- | data type instance to generate a Lazy String (list of Char `[Char]`) but
-- with higher probability of generating unicode characters
data LUString = LUString { toLString :: LString }
  deriving (Show, Eq, Ord)

instance IsString LUString where
    fromString = LUString
instance IsList LUString where
    type Item LUString = Char
    fromList = LUString
    toList (LUString l) = l
instance Arbitrary LUString where
    arbitrary = do
        n <- choose (0,200)
        oneof
            [ LUString <$> replicateM n (toEnum <$> choose (1, 0xff))
            , LUString <$> replicateM n (toEnum <$> choose (0x100, 0x1000))
            , LUString <$> replicateM n (toEnum <$> choose (0x100, 0x10000))
            , LUString <$> replicateM n (toEnum <$> choose (0x1, 0x1000))
            ]
