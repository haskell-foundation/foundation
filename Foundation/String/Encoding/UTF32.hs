-- |
-- Module      : Foundation.String.Encoding.UTF32
-- License     : BSD-style
-- Maintainer  : Foundation
-- Stability   : experimental
-- Portability : portable
--
{-# LANGUAGE MagicHash #-}
module Foundation.String.Encoding.UTF32
    ( UTF32(..)
    , UTF32_Invalid
    ) where

import Foundation.Internal.Base
import Foundation.Internal.Types
import Foundation.Primitive.Monad
import GHC.Prim
import GHC.Word
import GHC.Types
import Foundation.Numerical
import Foundation.Array.Unboxed
import Foundation.Array.Unboxed.Mutable (MUArray)
import Foundation.Collection.Buildable

import Foundation.String.Encoding.Encoding

data UTF32 = UTF32

data UTF32_Invalid = UTF32_Invalid
  deriving (Typeable, Show, Eq, Ord, Enum, Bounded)
instance Exception UTF32_Invalid

instance Encoding UTF32 where
    type Unit UTF32 = Word32
    type Error UTF32 = UTF32_Invalid
    encodingNext  _ = next
    encodingWrite _ = write

next :: (Offset Word32 -> Word32)
     -> Offset Word32
     -> Either UTF32_Invalid (Char, Offset Word32)
next getter off = Right (char, off + Offset 1)
  where
    !(W32# hh) = getter off
    char :: Char
    char = C# (chr# (word2Int# hh))

write :: (PrimMonad st, Monad st)
      => Char
      -> Builder (UArray Word32) (MUArray Word32) Word32 st ()
write c = append w32
  where
    !(C# ch) = c
    w32 :: Word32
    w32 = W32# (int2Word# (ord# ch))
