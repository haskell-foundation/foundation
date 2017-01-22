module Foundation.Numerical.Number
    ( IsIntegral(..)
    , IsNatural(..)
    ) where

import           Foundation.Internal.Base
import           Foundation.Internal.Natural
import qualified Prelude
import           Foreign.C.Types
import           System.Posix.Types

-- | Number literals, convertible through the generic Integer type.
--
-- all number are Enum'erable, meaning that you can move to
-- next element
class (Enum a, Eq a, Ord a, Integral a) => IsIntegral a where
    {-# MINIMAL toInteger #-}
    toInteger :: a -> Integer

-- | Non Negative Number literals, convertible through the generic Natural type
class (Enum a, Eq a, Ord a, Integral a, IsIntegral a) => IsNatural a where
    {-# MINIMAL toNatural #-}
    toNatural :: a -> Natural

instance IsIntegral Integer where
    toInteger i = i
instance IsIntegral Int where
    toInteger i = Prelude.toInteger i
instance IsIntegral Int8 where
    toInteger i = Prelude.toInteger i
instance IsIntegral Int16 where
    toInteger i = Prelude.toInteger i
instance IsIntegral Int32 where
    toInteger i = Prelude.toInteger i
instance IsIntegral Int64 where
    toInteger i = Prelude.toInteger i
instance IsIntegral Natural where
    toInteger i = Prelude.toInteger i
instance IsIntegral Word where
    toInteger i = Prelude.toInteger i
instance IsIntegral Word8 where
    toInteger i = Prelude.toInteger i
instance IsIntegral Word16 where
    toInteger i = Prelude.toInteger i
instance IsIntegral Word32 where
    toInteger i = Prelude.toInteger i
instance IsIntegral Word64 where
    toInteger i = Prelude.toInteger i
instance IsIntegral CSize where
    toInteger i = Prelude.toInteger i

instance IsNatural Natural where
    toNatural i = i
instance IsNatural Word where
    toNatural i = Prelude.fromIntegral i
instance IsNatural Word8 where
    toNatural i = Prelude.fromIntegral i
instance IsNatural Word16 where
    toNatural i = Prelude.fromIntegral i
instance IsNatural Word32 where
    toNatural i = Prelude.fromIntegral i
instance IsNatural Word64 where
    toNatural i = Prelude.fromIntegral i
instance IsNatural CSize where
    toNatural i = Prelude.fromIntegral i
