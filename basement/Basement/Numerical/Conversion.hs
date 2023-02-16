{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MagicHash             #-}
module Basement.Numerical.Conversion
    ( intToInt64
    , int64ToInt
    , intToWord
    , wordToWord64
    , word64ToWord
    , Word32x2(..)
    , word64ToWord32s
    , wordToChar
    , wordToInt
    , word64ToWord#
    , charToInt
    , int64ToWord64
    , word64ToInt64
    ) where

#include "MachDeps.h"

import GHC.Types
import GHC.Prim hiding (word64ToWord#)
import qualified GHC.Prim
import GHC.Int
import GHC.Word
import Basement.Compat.Primitive

#if WORD_SIZE_IN_BITS < 64
#if __GLASGOW_HASKELL__ >= 904
import GHC.Exts
#else
import GHC.IntWord64
#endif
#endif

intToInt64 :: Int -> Int64
#if WORD_SIZE_IN_BITS == 64
#if __GLASGOW_HASKELL__ >= 904
intToInt64 (I# i) = I64# (intToInt64# i)
#else
intToInt64 (I# i) = I64# i
#endif
#else
intToInt64 (I# i) = I64# (intToInt64# i)
#endif

int64ToInt :: Int64 -> Int
#if WORD_SIZE_IN_BITS == 64
#if __GLASGOW_HASKELL__ >= 904
int64ToInt (I64# i) = I# (int64ToInt# i)
#else
int64ToInt (I64# i) = I# i
#endif
#else
int64ToInt (I64# i) = I# (int64ToInt# i)
#endif

wordToWord64 :: Word -> Word64
#if WORD_SIZE_IN_BITS == 64
#if __GLASGOW_HASKELL__ >= 904
wordToWord64 (W# i) = W64# (wordToWord64# i)
#else
wordToWord64 (W# i) = W64# i
#endif
#else
wordToWord64 (W# i) = W64# (wordToWord64# i)
#endif

word64ToWord :: Word64 -> Word
#if WORD_SIZE_IN_BITS == 64
#if __GLASGOW_HASKELL__ >= 904
word64ToWord (W64# i) = W# (GHC.Prim.word64ToWord# i)
#else
word64ToWord (W64# i) = W# i
#endif
#else
word64ToWord (W64# i) = W# (word64ToWord# i)
#endif

word64ToInt64 :: Word64 -> Int64
#if WORD_SIZE_IN_BITS == 64
#if __GLASGOW_HASKELL__ >= 904
word64ToInt64 (W64# i) = I64# (word64ToInt64# i)
#else
word64ToInt64 (W64# i) = I64# (word2Int# i)
#endif
#else
word64ToInt64 (W64# i) = I64# (word64ToInt64# i)
#endif

int64ToWord64 :: Int64 -> Word64
#if WORD_SIZE_IN_BITS == 64
#if __GLASGOW_HASKELL__ >= 904
int64ToWord64 (I64# i) = W64# (int64ToWord64# i)
#else
int64ToWord64 (I64# i) = W64# (int2Word# i)
#endif
#else
int64ToWord64 (I64# i) = W64# (int64ToWord64# i)
#endif

#if WORD_SIZE_IN_BITS == 64
#if __GLASGOW_HASKELL__ >= 904
word64ToWord# :: Word64# -> Word#
word64ToWord# i = word64ToWord# i
#else
word64ToWord# :: Word# -> Word#
word64ToWord# i = i
#endif
{-# INLINE word64ToWord# #-}
#endif

#if WORD_SIZE_IN_BITS < 64
word64ToWord32# :: Word64# -> Word32#
word64ToWord32# i = wordToWord32# (word64ToWord# i)
{-# INLINE word64ToWord32# #-}
#endif

-- | 2 Word32s
data Word32x2 = Word32x2 {-# UNPACK #-} !Word32
                         {-# UNPACK #-} !Word32

#if WORD_SIZE_IN_BITS == 64
word64ToWord32s :: Word64 -> Word32x2
#if __GLASGOW_HASKELL__ >= 904
word64ToWord32s (W64# w64) = Word32x2 (W32# (wordToWord32# (uncheckedShiftRL# (GHC.Prim.word64ToWord# w64 ) 32#))) (W32# (wordToWord32# (GHC.Prim.word64ToWord# w64)))
#else
word64ToWord32s (W64# w64) = Word32x2 (W32# (wordToWord32# (uncheckedShiftRL# w64 32#))) (W32# (wordToWord32# w64))
#endif
#else
#if __GLASGOW_HASKELL__ >= 904
word64ToWord32s :: Word64 -> Word32x2
word64ToWord32s (W64# w64) = Word32x2 (W32# (word64ToWord32# (uncheckedShiftRL64# w64 32#))) (W32# (word64ToWord32# w64))
#else
word64ToWord32s :: Word64 -> Word32x2
word64ToWord32s (W64# w64) = Word32x2 (W32# (word64ToWord# (uncheckedShiftRL64# w64 32#))) (W32# (word64ToWord# w64))
#endif
#endif

wordToChar :: Word -> Char
wordToChar (W# word) = C# (chr# (word2Int# word))

wordToInt :: Word -> Int
wordToInt (W# word) = I# (word2Int# word)

intToWord :: Int -> Word
intToWord (I# i) = W# (int2Word# i)

charToInt :: Char -> Int
charToInt (C# x) = I# (ord# x)
