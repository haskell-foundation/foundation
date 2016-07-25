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
import Core.Array.Unboxed.Mutable (MUArray)
import GHC.Prim
import GHC.Word
import GHC.Types
import Core.Number
import Data.Bits

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
    to32 = undefined

    toChar :: Word# -> Char
    toChar w = C# (chr# (word2Int# w))

write :: PrimMonad prim => MUArray Word16 (PrimState prim) -> Offset Word16 -> Char -> prim (Offset Word16)
write mba idx@(Offset i) c
    | c < toEnum 0xd800  = undefined
    | c > toEnum 0x10000 = undefined
    | c >= toEnum 0xe000 = undefined
    | otherwise          = undefined
