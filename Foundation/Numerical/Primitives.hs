{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
module Foundation.Numerical.Primitives
    ( intToWord
    , integralConvert
    ) where

import GHC.Types
import GHC.Prim
import GHC.Word
import GHC.Int
import qualified Prelude

intToWord :: Int -> Word
intToWord (I# i) = W# (int2Word# i)
{-# INLINE intToWord #-}

class IntegralConvert a b where
    -- lossless integral convertion
    integralConvert :: a -> b

instance IntegralConvert Int8 Word8 where
    integralConvert (I8# i) = W8# (int2Word# i)
instance IntegralConvert Int16 Word16 where
    integralConvert (I16# i) = W16# (int2Word# i)
instance IntegralConvert Int32 Word32 where
    integralConvert (I32# i) = W32# (int2Word# i)
instance IntegralConvert Int64 Word64 where
    integralConvert i = Prelude.fromIntegral i
