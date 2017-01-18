-- |
-- Module      : Foundation.Internal.ByteSwap
-- License     : BSD-style
-- Maintainer  : Foundation
-- Stability   : experimental
-- Portability : portable
--

{-# LANGUAGE CPP #-}

module Foundation.Internal.ByteSwap
    ( byteSwap16
    , byteSwap32
    , byteSwap64
    ) where

#if MIN_VERSION_base(4,7,0)

import Data.Word (byteSwap16, byteSwap32, byteSwap64)

#else

import Foundation.Internal.Base (fromInteger)

import Data.Bits ((.|.), (.&.), unsafeShiftL, unsafeShiftR)

import Data.Word (Word16, Word32, Word64)

-- | compatibility implementation (i.e. may be slow)
--
-- Swap the bytes position (inverting order) as would be done in GHC >= 7.8
byteSwap16 :: Word16 -> Word16
byteSwap16 w = w1 .|. w2
  where
    w1,w2 :: Word16
    w1 = (w `unsafeShiftL` 8) .&. 0xFF00
    w2 = (w `unsafeShiftR` 8) .&. 0x00FF

-- | compatibility implementation (i.e. may be slow)
--
-- Swap the bytes position (inverting order) as would be done in GHC >= 7.8
byteSwap32 :: Word32 -> Word32
byteSwap32 w = w1 .|. w2 .|. w3 .|. w4
  where
    w1,w2,w3,w4 :: Word32
    w1 = (w `unsafeShiftL` 24) .&. 0xFF000000
    w2 = (w `unsafeShiftR` 24) .&. 0x000000FF
    w3 = (w `unsafeShiftL` 8)  .&. 0x00FF0000
    w4 = (w `unsafeShiftR` 8)  .&. 0x0000FF00

-- | compatibility implementation (i.e. may be slow)
--
-- Swap the bytes position (inverting order) as would be done in GHC >= 7.8
byteSwap64 :: Word64 -> Word64
byteSwap64 w = w1 .|. w2 .|. w3 .|. w4 .|. w5 .|. w6 .|. w7 .|. w8
  where
    w1,w2,w3,w4,w5,w6,w7,w8 :: Word64
    w1 = (w `unsafeShiftL` 56) .&. 0xFF00000000000000
    w2 = (w `unsafeShiftR` 56) .&. 0x00000000000000FF
    w3 = (w `unsafeShiftL` 40) .&. 0x00FF000000000000
    w4 = (w `unsafeShiftR` 40) .&. 0x000000000000FF00
    w5 = (w `unsafeShiftL` 24) .&. 0x0000FF0000000000
    w6 = (w `unsafeShiftR` 24) .&. 0x0000000000FF0000
    w7 = (w `unsafeShiftL`  8) .&. 0x000000FF00000000
    w8 = (w `unsafeShiftR`  8) .&. 0x00000000FF000000
#endif
