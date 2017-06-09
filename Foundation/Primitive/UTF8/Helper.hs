-- |
-- Module      : Foundation.Primitive.UTF8.Helper
-- License     : BSD-style
-- Maintainer  : Foundation
--
-- Some low level helpers to use UTF8
--
-- Most helpers are lowlevel and unsafe, don't use
-- directly.
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE CPP                        #-}
module Foundation.Primitive.UTF8.Helper
    where

import           Foundation.Internal.Base
import           Foundation.Internal.Primitive
import           Foundation.Bits
import           Foundation.Primitive.Types.OffsetSize
import           Foundation.Numerical
import           Foundation.Primitive.Types
import           GHC.Prim
import           GHC.Types
import           GHC.Word

-- same as nextAscii but with a ByteArray#
nextAsciiBA :: ByteArray# -> Offset8 -> (# Word8, Bool #)
nextAsciiBA ba n = (# w, not (testBit w 7) #)
  where
    !w = primBaIndex ba n
{-# INLINE nextAsciiBA #-}

-- same as nextAscii but with a ByteArray#
nextAsciiPtr :: Ptr Word8 -> Offset8 -> (# Word8, Bool #)
nextAsciiPtr (Ptr addr) n = (# w, not (testBit w 7) #)
  where !w = primAddrIndex addr n
{-# INLINE nextAsciiPtr #-}

-- | nextAsciiBa specialized to get a digit between 0 and 9 (included)
nextAsciiDigitBA :: ByteArray# -> Offset8 -> (# Word8, Bool #)
nextAsciiDigitBA ba n = (# d, d < 0xa #)
  where !d = primBaIndex ba n - 0x30
{-# INLINE nextAsciiDigitBA #-}

nextAsciiDigitPtr :: Ptr Word8 -> Offset8 -> (# Word8, Bool #)
nextAsciiDigitPtr (Ptr addr) n = (# d, d < 0xa #)
  where !d = primAddrIndex addr n - 0x30
{-# INLINE nextAsciiDigitPtr #-}

expectAsciiBA :: ByteArray# -> Offset8 -> Word8 -> Bool
expectAsciiBA ba n v = primBaIndex ba n == v
{-# INLINE expectAsciiBA #-}

expectAsciiPtr :: Ptr Word8 -> Offset8 -> Word8 -> Bool
expectAsciiPtr (Ptr ptr) n v = primAddrIndex ptr n == v
{-# INLINE expectAsciiPtr #-}

-- | Different way to encode a Character in UTF8 represented as an ADT
data UTF8Char =
      UTF8_1 {-# UNPACK #-} !Word8
    | UTF8_2 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
    | UTF8_3 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
    | UTF8_4 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8

-- | Transform a Unicode code point 'Char' into
--
-- note that we expect here a valid unicode code point in the *allowed* range.
-- bits will be lost if going above 0x10ffff
asUTF8Char :: Char -> UTF8Char
asUTF8Char !c
  | bool# (ltWord# x 0x80##   ) = encode1
  | bool# (ltWord# x 0x800##  ) = encode2
  | bool# (ltWord# x 0x10000##) = encode3
  | otherwise                   = encode4
    where
      !(I# xi) = fromEnum c
      !x       = int2Word# xi

      encode1 = UTF8_1 (W8# x)
      encode2 =
          let !x1 = W8# (or# (uncheckedShiftRL# x 6#) 0xc0##)
              !x2 = toContinuation x
           in UTF8_2 x1 x2
      encode3 =
          let !x1 = W8# (or# (uncheckedShiftRL# x 12#) 0xe0##)
              !x2 = toContinuation (uncheckedShiftRL# x 6#)
              !x3 = toContinuation x
           in UTF8_3 x1 x2 x3
      encode4 =
          let !x1 = W8# (or# (uncheckedShiftRL# x 18#) 0xf0##)
              !x2 = toContinuation (uncheckedShiftRL# x 12#)
              !x3 = toContinuation (uncheckedShiftRL# x 6#)
              !x4 = toContinuation x
           in UTF8_4 x1 x2 x3 x4

      toContinuation :: Word# -> Word8
      toContinuation w = W8# (or# (and# w 0x3f##) 0x80##)
      {-# INLINE toContinuation #-}

-- given the encoding of UTF8 Char, get the number of bytes of this sequence
numBytes :: UTF8Char -> Size8
numBytes UTF8_1{} = CountOf 1
numBytes UTF8_2{} = CountOf 2
numBytes UTF8_3{} = CountOf 3
numBytes UTF8_4{} = CountOf 4

-- given the leading byte of a utf8 sequence, get the number of bytes of this sequence
skipNextHeaderValue :: Word8 -> CountOf Word8
skipNextHeaderValue !x
    | x < 0xC0  = CountOf 1 -- 0b11000000
    | x < 0xE0  = CountOf 2 -- 0b11100000
    | x < 0xF0  = CountOf 3 -- 0b11110000
    | otherwise = CountOf 4
{-# INLINE skipNextHeaderValue #-}

charToBytes :: Int -> Size8
charToBytes c
    | c < 0x80     = CountOf 1
    | c < 0x800    = CountOf 2
    | c < 0x10000  = CountOf 3
    | c < 0x110000 = CountOf 4
    | otherwise    = error ("invalid code point: " `mappend` show c)
