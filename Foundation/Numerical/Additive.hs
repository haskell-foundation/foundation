{-# LANGUAGE DefaultSignatures #-}
module Foundation.Numerical.Additive
    ( Additive(..)
    ) where

import           Foundation.Internal.Base
import           Foundation.Internal.Natural
import           Foundation.Numerical.Number
import qualified Prelude

-- | Represent class of things that can be added together,
-- contains a neutral element and is commutative.
--
-- > x + azero = x
-- > azero + x = x
-- > x + y = y + x
--
class Additive a where
    {-# MINIMAL azero, (+) #-}
    azero :: a           -- the identity element over addition
    (+)   :: a -> a -> a -- the addition

    scale :: IsNatural n => n -> a -> a -- scale: repeated addition
    --default scale :: (Prelude.Num n, IsNatural n) => n -> a -> a
    scale 0 _ = azero
    scale 1 a = a
    scale 2 a = a + a
    scale n a = a + scale (pred n) a -- TODO optimise. define by group of 2.

infixl 6 +

instance Additive Integer where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive Int where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive Int8 where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive Int16 where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive Int32 where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive Int64 where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive Word where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive Natural where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive Word8 where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive Word16 where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive Word32 where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive Word64 where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive Prelude.Float where
    azero = 0.0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive Prelude.Double where
    azero = 0.0
    (+) = (Prelude.+)
    scale = scaleNum

scaleNum :: (Prelude.Num a, IsNatural n) => n -> a -> a
scaleNum n a = (Prelude.fromIntegral $ toNatural n) Prelude.* a
