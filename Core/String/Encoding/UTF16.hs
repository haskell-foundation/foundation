-- |
-- Module      : Core.String.Encoding.UTF16
-- License     : BSD-style
-- Maintainer  : Foundation
-- Stability   : experimental
-- Portability : portable
--
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
module Core.String.Encoding.UTF16
    ( write
    , next
    ) where

import Core.Internal.Base
import Core.Internal.Primitive
import Core.Internal.Types
import Core.Primitive.Types
import Core.Primitive.Monad
import Core.Array.Unboxed
import qualified Core.Array.Unboxed as Vec
import Core.Array.Unboxed.Mutable (MUArray, unsafeWrite)
import GHC.Prim
import GHC.Word
import GHC.Types
import Core.Number
import Data.Bits
import qualified Prelude

--
-- U+0000 to U+D7FF and U+E000 to U+FFFF : 1 bytes
-- U+10000 to U+10FFFF :
--    * 0x010000 is subtracted from the code point, leaving a 20-bit number in the range 0..0x0FFFFF.
--    * The top ten bits (a number in the range 0..0x03FF) are added to 0xD800 to give the first 16-bit code unit
--      or high surrogate, which will be in the range 0xD800..0xDBFF.
--    * The low ten bits (also in the range 0..0x03FF) are added to 0xDC00 to give the second 16-bit code unit
--      or low surrogate, which will be in the range 0xDC00..0xDFFF.

next :: UArray Word16 -> Offset Word16 -> (# Char, Offset Word16 #)
next ba ofs@(Offset n) =
    if h < 0xd800
        then (# toChar hh, ofs+Offset 1 #)
        else if h >= 0xe000
            then (# toChar hh, ofs+Offset 1 #)
            else
                -- CHECK vector is not out of bound
                let cont = Vec.unsafeIndex ba (n + 1)
                 in if cont >= 0xdc00 && cont < 0xe000
                        then let !(W32# w) = ((to32 h .&. 0x3ff) `shiftL` 10)
                                          .|. (to32 cont .&. 0x3ff)
                              in (# toChar w, ofs + Offset 2 #)
                        else error "invalid continuation"
  where
    !h@(W16# hh) = Vec.unsafeIndex ba n

    to32 :: Word16 -> Word32
    to32 (W16# w) = W32# w

    toChar :: Word# -> Char
    toChar w = C# (chr# (word2Int# w))

write :: PrimMonad prim => MUArray Word16 (PrimState prim) -> Offset Word16 -> Char -> prim (Offset Word16)
write mba idx@(Offset i) c
    | c < toEnum 0xd800   = unsafeWrite mba i (w16 c) >> return (idx + Offset 1)
    | c > toEnum 0x10000  = let (w1, w2) = wHigh c in unsafeWrite mba i w1 >> unsafeWrite mba (i+1) w2 >> return (idx + Offset 2)
    | c > toEnum 0x10ffff = error "invalid codepoint"
    | c >= toEnum 0xe000  = unsafeWrite mba i (w16 c) >> return (idx + Offset 1)
    | otherwise           = error "cannot "
  where
    w16 :: Char -> Word16
    w16 (C# ch) = W16# (int2Word# (ord# ch))

    to16 :: Word32 -> Word16
    to16 = Prelude.fromIntegral

    wHigh :: Char -> (Word16, Word16)
    wHigh (C# ch) =
        let v = W32# (minusWord# (int2Word# (ord# ch)) 0x10000##)
         in (0xdc00 .|. to16 (v `shiftR` 10), 0xd800 .|. to16 (v .&. 0x3ff))
