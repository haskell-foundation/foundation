{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Foundation.Primitive.IntegralConv
    ( IntegralDownsize(..)
    , IntegralUpsize(..)
    , IntegralCast(..)
    ) where

#include "MachDeps.h"

import GHC.Types
import GHC.Int
import GHC.Word
import Foundation.Internal.Base

-- | Downsize an integral value
class IntegralDownsize a b where
    integralDownsize      :: a -> b
    default integralDownsize :: a ~ b => a -> b
    integralDownsize = id

    integralDownsizeCheck :: a -> Maybe b
    default integralDownsizeCheck :: a ~ b => a -> Maybe b
    integralDownsizeCheck = Just

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

instance IntegralUpsize Int8 Int16 where
    integralUpsize (I8# i) = I16# i
instance IntegralUpsize Int8 Int32 where
    integralUpsize (I8# i) = I32# i
instance IntegralUpsize Int8 Int64 where
    integralUpsize (I8# i) = integralUpsize (I# i)
instance IntegralUpsize Int8 Int where
    integralUpsize (I8# i) = I# i

instance IntegralUpsize Int16 Int32 where
    integralUpsize (I16# i) = I32# i
instance IntegralUpsize Int16 Int64 where
    integralUpsize (I16# i) = integralUpsize (I# i)
instance IntegralUpsize Int16 Int where
    integralUpsize (I16# i) = I# i

instance IntegralUpsize Int32 Int64 where
    integralUpsize (I32# i) = integralUpsize (I# i)
instance IntegralUpsize Int32 Int where
    integralUpsize (I32# i) = I# i

instance IntegralUpsize Int Int64 where
#if WORD_SIZE_IN_BITS == 64
    integralUpsize (I# i) = I64# i
#else
    integralUpsize (I# i) = I64# (intToInt64# i)
#endif

instance IntegralUpsize Word8 Word16 where
    integralUpsize (W8# i) = W16# i
instance IntegralUpsize Word8 Word32 where
    integralUpsize (W8# i) = W32# i
instance IntegralUpsize Word8 Word64 where
    integralUpsize (W8# i) = integralUpsize (W# i)
instance IntegralUpsize Word8 Word where
    integralUpsize (W8# i) = W# i

instance IntegralUpsize Word16 Word32 where
    integralUpsize (W16# i) = W32# i
instance IntegralUpsize Word16 Word64 where
    integralUpsize (W16# i) = integralUpsize (W# i)
instance IntegralUpsize Word16 Word where
    integralUpsize (W16# i) = W# i

instance IntegralUpsize Word32 Word64 where
    integralUpsize (W32# i) = integralUpsize (W# i)
instance IntegralUpsize Word32 Word where
    integralUpsize (W32# i) = W# i

instance IntegralUpsize Word Word64 where
#if WORD_SIZE_IN_BITS == 64
    integralUpsize (W# i) = W64# i
#else
    integralUpsize (W# i) = W64# (wordToWord64# i)
#endif
