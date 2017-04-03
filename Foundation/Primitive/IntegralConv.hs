{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Foundation.Primitive.IntegralConv
    ( IntegralDownsize(..)
    , IntegralUpsize(..)
    , IntegralCast(..)
    , intToInt64
    , wordToWord64
    ) where

#include "MachDeps.h"

import GHC.Types
import GHC.Prim
import GHC.Int
import GHC.Word
import Prelude (Integer, fromIntegral)
import Foundation.Internal.Base
import Foundation.Internal.Natural

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

instance IntegralUpsize Word64 Integer where
    integralUpsize = fromIntegral
instance IntegralUpsize Word64 Natural where
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

instance IntegralCast Word Int where
    integralCast (W# w) = I# (word2Int# w)
instance IntegralCast Int Word where
    integralCast (I# i) = W# (int2Word# i)
instance IntegralCast Word64 Int64 where
#if WORD_SIZE_IN_BITS == 64
    integralCast (W64# i) = I64# (word2Int# i)
#else
    integralCast (W64# i) = I64# (int64ToWord64# i)
#endif
instance IntegralCast Int64 Word64 where
#if WORD_SIZE_IN_BITS == 64
    integralCast (I64# i) = W64# (int2Word# i)
#else
    integralCast (I64# i) = W64# (word64ToInt64# i)
#endif

-- missing word8, word16, word32, word64
-- instance IntegralCast Word8 Int8 where
-- instance IntegralCast Int8 Word8 where

intToInt64 :: Int -> Int64
#if WORD_SIZE_IN_BITS == 64
intToInt64 (I# i) = I64# i
#else
intToInt64 (I# i) = I64# (intToInt64# i)
#endif

wordToWord64 :: Word -> Word64
#if WORD_SIZE_IN_BITS == 64
wordToWord64 (W# i) = W64# i
#else
wordToWord64 (W# i) = W64# (wordToWord64# i)
#endif
