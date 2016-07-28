-- |
-- Module      : Core.String.Encoding.ASCII7
-- License     : BSD-style
-- Maintainer  : Foundation
-- Stability   : experimental
-- Portability : portable
--

{-# LANGUAGE MagicHash #-}

module Core.String.Encoding.ASCII7
    ( next
    , write
    ) where

import Core.Internal.Base
import Data.Bits
import Core.Internal.Types
import Core.Number
import Core.Primitive.Monad

import GHC.Prim
import GHC.Word
import GHC.Types
import GHC.Base (unsafeChr)
import           Core.Array.Unboxed.Builder

-- | validate a given byte is within ASCII characters encoring size
--
-- This function is equivalent to
--
-- > \w -> (0x10 .&. w) == 0x10
--
isAscii :: Word8 -> Bool
isAscii = (==) 0x10 . (.&.) 0x80
{-# INLINE isAscii #-}

-- offset of size one
aone :: Offset Word8
aone = Offset 1

data ASCII7_Invalid
    = ByteOutOfBound Word8
    | CharNotAscii   Char
  deriving (Typeable, Show, Eq)
instance Exception ASCII7_Invalid

-- | consume an Ascii7 char and return the Unicode point and the position
-- of the next possible Ascii7 char
--
next :: (Offset Word8 -> Word8)
          -- ^ method to access a given byte
     -> Offset Word8
          -- ^ index of the byte
     -> Either ASCII7_Invalid (Char, Offset Word8)
          -- ^ either successfully validated the ASCII char and returned the
          -- next index or fail with an error
next getter off
    | isAscii w8 = Right (toChar w, off + aone)
    | otherwise  = Left $ ByteOutOfBound w8
  where
    !w8@(W8# w) = getter off
    toChar :: Word# -> Char
    toChar a = C# (chr# (word2Int# a))

-- Write ascii char
--
-- > build 64 $ sequence_ write "this is a simple list of char..."
--
write :: (PrimMonad st, Monad st)
      => Char
          -- ^ expecting it to be a valid Ascii character.
          -- otherwise this function will throw an exception
      -> ArrayBuilder Word8 st ()
write c
    | c < toEnum 0x80 = appendTy $ w8 c
    | otherwise       = throw $ CharNotAscii c
  where
    w8 :: Char -> Word8
    w8 (C# ch) = W8# (int2Word# (ord# ch))
