{-# LANGUAGE TypeSynonymInstances #-}
module Foundation.Numerical.Floating
    ( FloatingPoint(..)
    ) where

import           Foundation.Internal.Base
import           Foundation.Internal.Proxy
import qualified Prelude

-- | IEEE754 Floating Point
class FloatingPoint a where
    floatRadix  :: Proxy a -> Integer
    floatDigits :: Proxy a -> Int
    floatRange  :: Proxy a -> (Int, Int)
    floatDecode :: a -> (Integer, Int)
    floatEncode :: Integer -> Int -> a

instance FloatingPoint FP32 where
    floatRadix _ = Prelude.floatRadix (0.0 :: FP32)
    floatDigits _ = Prelude.floatDigits (0.0 :: FP32)
    floatRange _ = Prelude.floatRange (0.0 :: FP32)
    floatDecode = Prelude.decodeFloat
    floatEncode = Prelude.encodeFloat

instance FloatingPoint FP64 where
    floatRadix _ = Prelude.floatRadix (0.0 :: FP64)
    floatDigits _ = Prelude.floatDigits (0.0 :: FP64)
    floatRange _ = Prelude.floatRange (0.0 :: FP64)
    floatDecode = Prelude.decodeFloat
    floatEncode = Prelude.encodeFloat
