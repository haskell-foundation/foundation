-- |
-- Module      : Foundation.String.Encoding.ISO_8859_1
-- License     : BSD-style
-- Maintainer  : Foundation
-- Stability   : experimental
-- Portability : portable
--

{-# LANGUAGE MagicHash #-}

module Foundation.String.Encoding.ISO_8859_1
    ( ISO_8859_1(..)
    , ISO_8859_1_Invalid(..)
    ) where

import Foundation.Internal.Base
import Foundation.Internal.Types
import Foundation.Number
import Foundation.Primitive.Monad

import GHC.Prim
import GHC.Word
import GHC.Types
import Foundation.Array.Unboxed.Builder

import Foundation.String.Encoding.Encoding

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
    type Error ISO_8859_1 = ISO_8859_1_Invalid
    encodingNext  _ = next
    encodingWrite _ = write

next :: (Offset Word8 -> Word8)
     -> Offset Word8
     -> Either ISO_8859_1_Invalid (Char, Offset Word8)
next getter off = Right (toChar w, off + aone)
  where
    !(W8# w) = getter off
    toChar :: Word# -> Char
    toChar a = C# (chr# (word2Int# a))

write :: (PrimMonad st, Monad st)
      => Char
      -> ArrayBuilder Word8 st ()
write c@(C# ch)
    | c <= toEnum 0xFF = appendTy (W8# x)
    | otherwise       = throw $ NotISO_8859_1 c
  where
    x :: Word#
    !x = int2Word# (ord# ch)
