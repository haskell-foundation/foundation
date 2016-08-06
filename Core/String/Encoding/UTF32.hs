-- |
-- Module      : Core.String.Encoding.UTF32
-- License     : BSD-style
-- Maintainer  : Foundation
-- Stability   : experimental
-- Portability : portable
--
{-# LANGUAGE MagicHash #-}
module Core.String.Encoding.UTF32
    ( UTF32(..)
    ) where

import Core.Internal.Base
import Core.Internal.Types
import Core.Primitive.Monad
import GHC.Prim
import GHC.Word
import GHC.Types
import Core.Number
import Core.Array.Unboxed.Builder

import Core.String.Encoding.Encoding

data UTF32 = UTF32

instance Encoding UTF32 where
    type Unit UTF32 = Word32
    type Error UTF32 a = a
    encodingNext  _ = next
    encodingWrite _ = write

next :: (Offset Word32 -> Word32)
     -> Offset Word32
     -> (Char, Offset Word32)
next getter off = (char, off + Offset 1)
  where
    !(W32# hh) = getter off
    char :: Char
    char = C# (chr# (word2Int# hh))

write :: (PrimMonad st, Monad st)
      => Char
      -> ArrayBuilder Word32 st ()
write c = appendTy w32
  where
    !(C# ch) = c
    w32 :: Word32
    w32 = W32# (int2Word# (ord# ch))
