-- |
-- Module      : Core.String.Encoding.ISO_8859_1
-- License     : BSD-style
-- Maintainer  : Foundation
-- Stability   : experimental
-- Portability : portable
--

{-# LANGUAGE MagicHash #-}

module Core.String.Encoding.ISO_8859_1
    ( ISO_8859_1(..)
    , ISO_8859_1_Invalid(..)
    ) where

import Core.Internal.Base
import Core.Internal.Types
import Core.Number
import Core.Primitive.Monad

import GHC.Prim
import GHC.Word
import GHC.Types
import Core.Array.Unboxed.Builder

import Core.String.Encoding.Encoding

-- offset of size one
aone :: Offset Word8
aone = Offset 1

data ISO_8859_1_Invalid
    = NotISO_8859_1 Char
  deriving (Typeable, Show, Eq)
instance Exception ISO_8859_1_Invalid

data ISO_8859_1 = ISO_8859_1

instance Encoding ISO_8859_1 where
    type Unit ISO_8859_1 = Word8
    type Error ISO_8859_1 a = Either ISO_8859_1_Invalid a
    next _ = next_
    write _ = write_

next_ :: (Offset Word8 -> Word8)
      -> Offset Word8
      -> Either ISO_8859_1_Invalid (Char, Offset Word8)
next_ getter off = Right (toChar w, off + aone)
  where
    !(W8# w) = getter off
    toChar :: Word# -> Char
    toChar a = C# (chr# (word2Int# a))

write_ :: (PrimMonad st, Monad st)
       => Char
       -> ArrayBuilder Word8 st ()
write_ c@(C# ch)
    | c <  toEnum 0x80 = appendTy (W8# x)
    | c <= toEnum 0xFF = do
        appendTy $ W8# (or# (uncheckedShiftRL# x 6#) 0xC0##)
        appendTy $ W8# (toContinuation x)
    | otherwise       = throw $ NotISO_8859_1 c
  where
    x :: Word#
    !x = int2Word# (ord# ch)

    toContinuation :: Word# -> Word#
    toContinuation w = or# (and# w 0x3f##) 0x80##
