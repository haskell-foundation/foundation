{-# LANGUAGE CPP               #-}
{-# LANGUAGE MagicHash         #-}
module Foundation.Numerical.Additive
    ( Additive(..)
    ) where

#include "MachDeps.h"

import           Foundation.Internal.Base
import           Foundation.Internal.Natural
import           Foundation.Numerical.Number
import qualified Prelude
import           GHC.Types
import           GHC.Prim
import           GHC.Int
import           GHC.Word
import           Foreign.C.Types

#if WORD_SIZE_IN_BITS < 64
import           GHC.IntWord64
#endif

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
    (I# a) + (I# b) = I# (a +# b)
    scale = scaleNum
instance Additive Int8 where
    azero = 0
    (I8# a) + (I8# b) = I8# (narrow8Int# (a +# b))
    scale = scaleNum
instance Additive Int16 where
    azero = 0
    (I16# a) + (I16# b) = I16# (narrow16Int# (a +# b))
    scale = scaleNum
instance Additive Int32 where
    azero = 0
    (I32# a) + (I32# b) = I32# (narrow32Int# (a +# b))
    scale = scaleNum
instance Additive Int64 where
    azero = 0
#if WORD_SIZE_IN_BITS == 64
    (I64# a) + (I64# b) = I64# (a +# b)
#else
    (I64# a) + (I64# b) = I64# (a `plusInt64#` b)
#endif
    scale = scaleNum
instance Additive Word where
    azero = 0
    (W# a) + (W# b) = W# (a `plusWord#` b)
    scale = scaleNum
instance Additive Natural where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive Word8 where
    azero = 0
    (W8# a) + (W8# b) = W8# (narrow8Word# (a `plusWord#` b))
    scale = scaleNum
instance Additive Word16 where
    azero = 0
    (W16# a) + (W16# b) = W16# (narrow16Word# (a `plusWord#` b))
    scale = scaleNum
instance Additive Word32 where
    azero = 0
    (W32# a) + (W32# b) = W32# (narrow32Word# (a `plusWord#` b))
    scale = scaleNum
instance Additive Word64 where
    azero = 0
#if WORD_SIZE_IN_BITS == 64
    (W64# a) + (W64# b) = W64# (a `plusWord#` b)
#else
    (W64# a) + (W64# b) = W64# (int64ToWord64# (word64ToInt64# a `plusInt64#` word64ToInt64# b))
#endif
    scale = scaleNum
instance Additive Prelude.Float where
    azero = 0.0
    (F# a) + (F# b) = F# (a `plusFloat#` b)
    scale = scaleNum
instance Additive Prelude.Double where
    azero = 0.0
    (D# a) + (D# b) = D# (a +## b)
    scale = scaleNum
instance Additive CSize where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum

scaleNum :: (Prelude.Num a, IsNatural n) => n -> a -> a
scaleNum n a = (Prelude.fromIntegral $ toNatural n) Prelude.* a
