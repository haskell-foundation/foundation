{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeOperators         #-}
module Basement.IntegralConv
    ( IntegralDownsize(..)
    , IntegralUpsize(..)
    , intToInt64
    , int64ToInt
    , wordToWord64
    , word64ToWord32s
    , Word32x2(..)
    , word64ToWord
    , wordToChar
    , wordToInt
    , charToInt
    ) where

import GHC.Types
import GHC.Prim hiding (word64ToWord#)
import qualified GHC.Prim
import GHC.Int
import GHC.Word
import Prelude (Integer, fromIntegral)
import Basement.Compat.Base
import Basement.Compat.Natural
import Basement.Compat.Primitive
import Basement.Numerical.Number
import Basement.Numerical.Conversion

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

integralDownsizeBounded :: forall a b . (Ord a, Bounded b, IntegralDownsize a b, IntegralUpsize b a)
                        => (a -> b)
                        -> a
                        -> Maybe b
integralDownsizeBounded aToB x
    | x < integralUpsize (minBound :: b) && x > integralUpsize (maxBound :: b) = Nothing
    | otherwise                                                                = Just (aToB x)

instance IsIntegral a => IntegralUpsize a Integer where
    integralUpsize = toInteger
instance IsNatural a => IntegralUpsize a Natural where
    integralUpsize = toNatural

instance IntegralUpsize Int8 Int16 where
    integralUpsize (I8# i) = I16# (int8ToInt16# i)
instance IntegralUpsize Int8 Int32 where
    integralUpsize (I8# i) = I32# (int8ToInt32# i)
instance IntegralUpsize Int8 Int64 where
    integralUpsize (I8# i) = intToInt64 (I# (int8ToInt# i))
instance IntegralUpsize Int8 Int where
    integralUpsize (I8# i) = I# (int8ToInt# i)

instance IntegralUpsize Int16 Int32 where
    integralUpsize (I16# i) = I32# (int16ToInt32# i)
instance IntegralUpsize Int16 Int64 where
    integralUpsize (I16# i) = intToInt64 (I# (int16ToInt# i))
instance IntegralUpsize Int16 Int where
    integralUpsize (I16# i) = I# (int16ToInt# i)

instance IntegralUpsize Int32 Int64 where
    integralUpsize (I32# i) = intToInt64 (I# (int32ToInt# i))
instance IntegralUpsize Int32 Int where
    integralUpsize (I32# i) = I# (int32ToInt# i)

instance IntegralUpsize Int Int64 where
    integralUpsize = intToInt64

instance IntegralUpsize Word8 Word16 where
    integralUpsize (W8# i) = W16# (word8ToWord16# i)
instance IntegralUpsize Word8 Word32 where
    integralUpsize (W8# i) = W32# (word8ToWord32# i)
instance IntegralUpsize Word8 Word64 where
    integralUpsize (W8# i) = wordToWord64 (W# (word8ToWord# i))
instance IntegralUpsize Word8 Word where
    integralUpsize (W8# i) = W# (word8ToWord# i)
instance IntegralUpsize Word8 Int16 where
    integralUpsize (W8# w) = I16# (word8ToInt16# w)
instance IntegralUpsize Word8 Int32 where
    integralUpsize (W8# w) = I32# (word8ToInt32# w)
instance IntegralUpsize Word8 Int64 where
    integralUpsize (W8# w) = intToInt64 (I# (word2Int# (word8ToWord# w)))
instance IntegralUpsize Word8 Int where
    integralUpsize (W8# w) = I# (word2Int# (word8ToWord# w))

instance IntegralUpsize Word16 Word32 where
    integralUpsize (W16# i) = W32# (word16ToWord32# i)
instance IntegralUpsize Word16 Word64 where
    integralUpsize (W16# i) = wordToWord64 (W# (word16ToWord# i))
instance IntegralUpsize Word16 Word where
    integralUpsize (W16# i) = W# (word16ToWord# i)

instance IntegralUpsize Word32 Word64 where
    integralUpsize (W32# i) = wordToWord64 (W# (word32ToWord# i))
instance IntegralUpsize Word32 Word where
    integralUpsize (W32# i) = W# (word32ToWord# i)

instance IntegralUpsize Word Word64 where
    integralUpsize = wordToWord64

instance IntegralDownsize Int Int8 where
    integralDownsize      (I# i) = I8# (intToInt8# i)
    integralDownsizeCheck = integralDownsizeBounded integralDownsize
instance IntegralDownsize Int Int16 where
    integralDownsize      (I# i) = I16# (intToInt16# i)
    integralDownsizeCheck = integralDownsizeBounded integralDownsize
instance IntegralDownsize Int Int32 where
    integralDownsize      (I# i) = I32# (intToInt32# i)
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
#if __GLASGOW_HASKELL__ >= 904
    integralDownsize      (W64# i) = W8# (wordToWord8# (GHC.Prim.word64ToWord# i))
#else
    integralDownsize      (W64# i) = W8# (wordToWord8# (word64ToWord# i))
#endif
    integralDownsizeCheck = integralDownsizeBounded integralDownsize
instance IntegralDownsize Word64 Word16 where
#if __GLASGOW_HASKELL__ >= 904
    integralDownsize      (W64# i) = W16# (wordToWord16# (GHC.Prim.word64ToWord# i))
#else
    integralDownsize      (W64# i) = W16# (wordToWord16# (word64ToWord# i))
#endif
    integralDownsizeCheck = integralDownsizeBounded integralDownsize
instance IntegralDownsize Word64 Word32 where
#if __GLASGOW_HASKELL__ >= 904
    integralDownsize      (W64# i) = W32# (wordToWord32# (GHC.Prim.word64ToWord# i))
#else
    integralDownsize      (W64# i) = W32# (wordToWord32# (word64ToWord# i))
#endif
    integralDownsizeCheck = integralDownsizeBounded integralDownsize

instance IntegralDownsize Word Word8 where
    integralDownsize (W# w) = W8# (wordToWord8# w)
    integralDownsizeCheck = integralDownsizeBounded integralDownsize
instance IntegralDownsize Word Word16 where
    integralDownsize (W# w) = W16# (wordToWord16# w)
    integralDownsizeCheck = integralDownsizeBounded integralDownsize
instance IntegralDownsize Word Word32 where
    integralDownsize (W# w) = W32# (wordToWord32# w)
    integralDownsizeCheck = integralDownsizeBounded integralDownsize

instance IntegralDownsize Word32 Word8 where
    integralDownsize      (W32# i) = W8# (word32ToWord8# i)
    integralDownsizeCheck = integralDownsizeBounded integralDownsize
instance IntegralDownsize Word32 Word16 where
    integralDownsize      (W32# i) = W16# (word32ToWord16# i)
    integralDownsizeCheck = integralDownsizeBounded integralDownsize

instance IntegralDownsize Word16 Word8 where
    integralDownsize      (W16# i) = W8# (word16ToWord8# i)
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
