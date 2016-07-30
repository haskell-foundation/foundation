-- |
-- Module      : Core.String.Encoding.ISO_8859_1
-- License     : BSD-style
-- Maintainer  : Foundation
-- Stability   : experimental
-- Portability : portable
--

{-# LANGUAGE MagicHash #-}

module Core.String.Encoding.ISO_8859_1
    ( next
    , write
    ) where

import Core.Internal.Base
import Core.Internal.Types
import Core.Number
import Core.Primitive.Monad

import GHC.Prim
import GHC.Word
import GHC.Types
import Core.Array.Unboxed.Builder

-- offset of size one
aone :: Offset Word8
aone = Offset 1

data ISO_8859_1_Invalid
    = NotISO_8859_1 Char
  deriving (Typeable, Show, Eq)
instance Exception ISO_8859_1_Invalid

-- | consume an Ascii7 char and return the Unicode point and the position
-- of the next possible Ascii7 char
--
next :: (Offset Word8 -> Word8)
          -- ^ method to access a given byte
     -> Offset Word8
          -- ^ index of the byte
     -> Either ISO_8859_1_Invalid (Char, Offset Word8)
          -- ^ either successfully validated the ASCII char and returned the
          -- next index or fail with an error
next getter off = Right (toChar w, off + aone)
  where
    !(W8# w) = getter off
    toChar :: Word# -> Char
    toChar a = C# (chr# (word2Int# a))

-- Write ascii char
--
-- > build 64 $ sequence_ write "this is a simple list of char..."
--
write :: (PrimMonad st, Monad st)
      => Char
      -> ArrayBuilder Word8 st ()
write c@(C# ch)
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
