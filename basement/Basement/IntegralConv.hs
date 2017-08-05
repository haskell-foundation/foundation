{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnboxedTuples         #-}
module Basement.IntegralConv
    ( IntegralDownsize(..)
    , IntegralUpsize(..)
    , IntegralCast(..)
    , intToInt64
    , int64ToInt
    , wordToWord64
    , word64ToWord32s
    , word64ToWord
    , wordToChar
    , wordToInt
    , charToInt
    ) where

#include "MachDeps.h"

import GHC.Types
import GHC.Prim
import GHC.Int
import GHC.Word
import Prelude (Integer, fromIntegral)
import Basement.Compat.Base
import Basement.Compat.Natural

#if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64
#endif

-- | Downsize an integral value
class IntegralDownsize a b where
    integralDownsize :: a -> b
    default integralDownsize :: a ~ b => a -> b
    integralDownsize = id

    integralDownsizeCheck :: a -> Maybe b

-- | Upsize an integral value
--
-- The destination type 'b' size need to be greater or equal
-- than the size type of 'a'
class IntegralUpsize a b where
    integralUpsize      :: a -> b

-- | Cast an integral value to another value
-- that have the same representional size
class IntegralCast a b where
    integralCast :: a -> b
    default integralCast :: a ~ b => a -> b
    integralCast = id

integralDownsizeBounded :: forall a b . (Ord a, Bounded b, IntegralDownsize a b, IntegralUpsize b a)
                        => (a -> b)
                        -> a
                        -> Maybe b
integralDownsizeBounded aToB x
    | x < integralUpsize (minBound :: b) && x > integralUpsize (maxBound :: b) = Nothing
    | otherwise                                                                = Just (aToB x)

instance IntegralUpsize Int8 Int16 where
    integralUpsize (I8# i) = I16# i
instance IntegralUpsize Int8 Int32 where
    integralUpsize (I8# i) = I32# i
instance IntegralUpsize Int8 Int64 where
    integralUpsize (I8# i) = intToInt64 (I# i)
instance IntegralUpsize Int8 Int where
    integralUpsize (I8# i) = I# i
instance IntegralUpsize Int8 Integer where
    integralUpsize = fromIntegral

instance IntegralUpsize Int16 Int32 where
    integralUpsize (I16# i) = I32# i
instance IntegralUpsize Int16 Int64 where
    integralUpsize (I16# i) = intToInt64 (I# i)
instance IntegralUpsize Int16 Int where
    integralUpsize (I16# i) = I# i
instance IntegralUpsize Int16 Integer where
    integralUpsize = fromIntegral

instance IntegralUpsize Int32 Int64 where
    integralUpsize (I32# i) = intToInt64 (I# i)
instance IntegralUpsize Int32 Int where
    integralUpsize (I32# i) = I# i
instance IntegralUpsize Int32 Integer where
    integralUpsize = fromIntegral

instance IntegralUpsize Int Integer where
    integralUpsize = fromIntegral
instance IntegralUpsize Int Int64 where
    integralUpsize = intToInt64

instance IntegralUpsize Int64 Integer where
    integralUpsize = fromIntegral

instance IntegralUpsize Word8 Word16 where
    integralUpsize (W8# i) = W16# i
instance IntegralUpsize Word8 Word32 where
    integralUpsize (W8# i) = W32# i
instance IntegralUpsize Word8 Word64 where
    integralUpsize (W8# i) = wordToWord64 (W# i)
instance IntegralUpsize Word8 Word where
    integralUpsize (W8# i) = W# i
instance IntegralUpsize Word8 Int16 where
    integralUpsize (W8# w) = I16# (word2Int# w)
instance IntegralUpsize Word8 Int32 where
    integralUpsize (W8# w) = I32# (word2Int# w)
instance IntegralUpsize Word8 Int64 where
    integralUpsize (W8# w) = intToInt64 (I# (word2Int# w))
instance IntegralUpsize Word8 Int where
    integralUpsize (W8# w) = I# (word2Int# w)
instance IntegralUpsize Word8 Integer where
    integralUpsize = fromIntegral
instance IntegralUpsize Word8 Natural where
    integralUpsize = fromIntegral

instance IntegralUpsize Word16 Word32 where
    integralUpsize (W16# i) = W32# i
instance IntegralUpsize Word16 Word64 where
    integralUpsize (W16# i) = wordToWord64 (W# i)
instance IntegralUpsize Word16 Word where
    integralUpsize (W16# i) = W# i
instance IntegralUpsize Word16 Integer where
    integralUpsize = fromIntegral
instance IntegralUpsize Word16 Natural where
    integralUpsize = fromIntegral

instance IntegralUpsize Word32 Word64 where
    integralUpsize (W32# i) = wordToWord64 (W# i)
instance IntegralUpsize Word32 Word where
    integralUpsize (W32# i) = W# i
instance IntegralUpsize Word32 Integer where
    integralUpsize = fromIntegral
instance IntegralUpsize Word32 Natural where
    integralUpsize = fromIntegral

instance IntegralUpsize Word Integer where
    integralUpsize = fromIntegral
instance IntegralUpsize Word Natural where
    integralUpsize = fromIntegral
instance IntegralUpsize Word Word64 where
    integralUpsize = wordToWord64

instance IntegralUpsize Word64 Integer where
    integralUpsize = fromIntegral
instance IntegralUpsize Word64 Natural where
    integralUpsize = fromIntegral

instance IntegralUpsize Natural Integer where
    integralUpsize = fromIntegral

instance IntegralDownsize Int Int8 where
    integralDownsize      (I# i) = I8# (narrow8Int# i)
    integralDownsizeCheck = integralDownsizeBounded integralDownsize
instance IntegralDownsize Int Int16 where
    integralDownsize      (I# i) = I16# (narrow16Int# i)
    integralDownsizeCheck = integralDownsizeBounded integralDownsize
instance IntegralDownsize Int Int32 where
    integralDownsize      (I# i) = I32# (narrow32Int# i)
    integralDownsizeCheck = integralDownsizeBounded integralDownsize

instance IntegralDownsize Int64 Int8 where
    integralDownsize      i = integralDownsize (int64ToInt i)
    integralDownsizeCheck = integralDownsizeBounded integralDownsize
instance IntegralDownsize Int64 Int16 where
    integralDownsize      i = integralDownsize (int64ToInt i)
    integralDownsizeCheck = integralDownsizeBounded integralDownsize
instance IntegralDownsize Int64 Int32 where
    integralDownsize      i = integralDownsize (int64ToInt i)
    integralDownsizeCheck = integralDownsizeBounded integralDownsize
instance IntegralDownsize Int64 Int where
    integralDownsize      i = int64ToInt i
    integralDownsizeCheck = integralDownsizeBounded integralDownsize

instance IntegralDownsize Word64 Word8 where
    integralDownsize      (W64# i) = W8# (narrow8Word# (word64ToWord# i))
    integralDownsizeCheck = integralDownsizeBounded integralDownsize
instance IntegralDownsize Word64 Word16 where
    integralDownsize      (W64# i) = W16# (narrow16Word# (word64ToWord# i))
    integralDownsizeCheck = integralDownsizeBounded integralDownsize
instance IntegralDownsize Word64 Word32 where
    integralDownsize      (W64# i) = W32# (narrow32Word# (word64ToWord# i))
    integralDownsizeCheck = integralDownsizeBounded integralDownsize

instance IntegralDownsize Word Word8 where
    integralDownsize (W# w) = W8# (narrow8Word# w)
    integralDownsizeCheck = integralDownsizeBounded integralDownsize
instance IntegralDownsize Word Word16 where
    integralDownsize (W# w) = W16# (narrow16Word# w)
    integralDownsizeCheck = integralDownsizeBounded integralDownsize
instance IntegralDownsize Word Word32 where
    integralDownsize (W# w) = W32# (narrow32Word# w)
    integralDownsizeCheck = integralDownsizeBounded integralDownsize

instance IntegralDownsize Word32 Word8 where
    integralDownsize      (W32# i) = W8# (narrow8Word# i)
    integralDownsizeCheck = integralDownsizeBounded integralDownsize
instance IntegralDownsize Word32 Word16 where
    integralDownsize      (W32# i) = W16# (narrow16Word# i)
    integralDownsizeCheck = integralDownsizeBounded integralDownsize

instance IntegralDownsize Word16 Word8 where
    integralDownsize      (W16# i) = W8# (narrow8Word# i)
    integralDownsizeCheck = integralDownsizeBounded integralDownsize

instance IntegralDownsize Integer Int8 where
    integralDownsize = fromIntegral
    integralDownsizeCheck = integralDownsizeBounded integralDownsize
instance IntegralDownsize Integer Int16 where
    integralDownsize = fromIntegral
    integralDownsizeCheck = integralDownsizeBounded integralDownsize
instance IntegralDownsize Integer Int32 where
    integralDownsize = fromIntegral
    integralDownsizeCheck = integralDownsizeBounded integralDownsize
instance IntegralDownsize Integer Int64 where
    integralDownsize = fromIntegral
    integralDownsizeCheck = integralDownsizeBounded integralDownsize

instance IntegralDownsize Integer Word8 where
    integralDownsize = fromIntegral
    integralDownsizeCheck = integralDownsizeBounded integralDownsize
instance IntegralDownsize Integer Word16 where
    integralDownsize = fromIntegral
    integralDownsizeCheck = integralDownsizeBounded integralDownsize
instance IntegralDownsize Integer Word32 where
    integralDownsize = fromIntegral
    integralDownsizeCheck = integralDownsizeBounded integralDownsize
instance IntegralDownsize Integer Word64 where
    integralDownsize = fromIntegral
    integralDownsizeCheck = integralDownsizeBounded integralDownsize
instance IntegralDownsize Integer Natural where
    integralDownsize i
        | i >= 0    = fromIntegral i
        | otherwise = 0
    integralDownsizeCheck i
        | i >= 0    = Just (fromIntegral i)
        | otherwise = Nothing

instance IntegralDownsize Natural Word8 where
    integralDownsize = fromIntegral
    integralDownsizeCheck = integralDownsizeBounded integralDownsize
instance IntegralDownsize Natural Word16 where
    integralDownsize = fromIntegral
    integralDownsizeCheck = integralDownsizeBounded integralDownsize
instance IntegralDownsize Natural Word32 where
    integralDownsize = fromIntegral
    integralDownsizeCheck = integralDownsizeBounded integralDownsize
instance IntegralDownsize Natural Word64 where
    integralDownsize = fromIntegral
    integralDownsizeCheck = integralDownsizeBounded integralDownsize

instance IntegralCast Word Int where
    integralCast (W# w) = I# (word2Int# w)
instance IntegralCast Int Word where
    integralCast (I# i) = W# (int2Word# i)
instance IntegralCast Word64 Int64 where
#if WORD_SIZE_IN_BITS == 64
    integralCast (W64# i) = I64# (word2Int# i)
#else
    integralCast (W64# i) = I64# (word64ToInt64# i)
#endif
instance IntegralCast Int64 Word64 where
#if WORD_SIZE_IN_BITS == 64
    integralCast (I64# i) = W64# (int2Word# i)
#else
    integralCast (I64# i) = W64# (int64ToWord64# i)
#endif

instance IntegralCast Int8 Word8 where
    integralCast (I8# i) = W8# (narrow8Word# (int2Word# i))

instance IntegralCast Int16 Word16 where
    integralCast (I16# i) = W16# (narrow16Word# (int2Word# i))

instance IntegralCast Int32 Word32 where
    integralCast (I32# i) = W32# (narrow32Word# (int2Word# i))

instance IntegralCast Word8 Int8 where
    integralCast (W8# i) = I8# (narrow8Int# (word2Int# i))

instance IntegralCast Word16 Int16 where
    integralCast (W16# i) = I16# (narrow16Int# (word2Int# i))

instance IntegralCast Word32 Int32 where
    integralCast (W32# i) = I32# (narrow32Int# (word2Int# i))

intToInt64 :: Int -> Int64
#if WORD_SIZE_IN_BITS == 64
intToInt64 (I# i) = I64# i
#else
intToInt64 (I# i) = I64# (intToInt64# i)
#endif

int64ToInt :: Int64 -> Int
#if WORD_SIZE_IN_BITS == 64
int64ToInt (I64# i) = I# i
#else
int64ToInt (I64# i) = I# (int64ToInt# i)
#endif

wordToWord64 :: Word -> Word64
#if WORD_SIZE_IN_BITS == 64
wordToWord64 (W# i) = W64# i
#else
wordToWord64 (W# i) = W64# (wordToWord64# i)
#endif

word64ToWord :: Word64 -> Word
#if WORD_SIZE_IN_BITS == 64
word64ToWord (W64# i) = W# i
#else
word64ToWord (W64# i) = W# (word64ToWord# i)
#endif

#if WORD_SIZE_IN_BITS == 64
word64ToWord# :: Word# -> Word#
word64ToWord# i = i
{-# INLINE word64ToWord# #-}
#endif

#if WORD_SIZE_IN_BITS == 64
word64ToWord32s :: Word64 -> (# Word32, Word32 #)
word64ToWord32s (W64# w64) = (# W32# (uncheckedShiftRL# w64 32#), W32# (narrow32Word# w64) #)
#else
word64ToWord32s :: Word64 -> (# Word32, Word32 #)
word64ToWord32s (W64# w64) = (# W32# (word64ToWord# (uncheckedShiftRL64# w64 32#)), W32# (word64ToWord# w64) #)
#endif

wordToChar :: Word -> Char
wordToChar (W# word) = C# (chr# (word2Int# word))

wordToInt :: Word -> Int
wordToInt (W# word) = I# (word2Int# word)

charToInt :: Char -> Int
charToInt (C# x) = I# (ord# x)
