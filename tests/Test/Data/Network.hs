-- |
-- Module:
-- Author: Nicolas Di Prima <nicolas>
-- Date:   2017-01-18T17:34:06+00:00
-- Email:  nicolasdiprima@gmail.com
--

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Test.Data.Network
    ( genIPv4
    , genIPv4Tuple
    , genIPv4String
    ) where

import Foundation
import Foundation.Network.IPv4
import Foundation.Class.Storable as F
import Test.Tasty.QuickCheck
import qualified Foreign.Storable as Foreign

instance Arbitrary IPv4 where
    arbitrary = genIPv4
instance Foreign.Storable IPv4 where
    sizeOf a = let Size b = F.size (Just a) in b
    alignment a = let Size b = F.alignment (Just a) in b
    peek = F.peek
    poke = F.poke

genIPv4Tuple :: Gen (Word8, Word8, Word8, Word8)
genIPv4Tuple =
    (,,,) <$> choose (0, 255)
          <*> choose (0, 255)
          <*> choose (0, 255)
          <*> choose (0, 255)

genIPv4String :: Gen String
genIPv4String = do
    (w1, w2, w3, w4) <- genIPv4Tuple
    return $ show w1 <> "." <> show w2 <> "." <> show w3 <> "." <> show w4

-- | a better generator for unicode Character
genIPv4 :: Gen IPv4
genIPv4 = fromTuple <$> genIPv4Tuple
