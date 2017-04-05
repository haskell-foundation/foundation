module Foundation.Numerical.Subtractive
    ( Subtractive(..)
    ) where

import           Foundation.Internal.Base
import           Foundation.Internal.Natural
import           Foundation.Primitive.IntegralConv
import qualified Prelude

-- | Represent class of things that can be subtracted.
--
--
-- Note that the result is not necessary of the same type
-- as the operand depending on the actual type.
--
-- For example:
--
-- > (-) :: Int -> Int -> Int
-- > (-) :: DateTime -> DateTime -> Seconds
-- > (-) :: Ptr a -> Ptr a -> PtrDiff
-- > (-) :: Natural -> Natural -> Maybe Natural
class Subtractive a where
    type Difference a
    (-) :: a -> a -> Difference a

infixl 6 -

instance Subtractive Integer where
    type Difference Integer = Integer
    (-) = (Prelude.-)
instance Subtractive Int where
    type Difference Int = Int
    (-) = (Prelude.-)
instance Subtractive Int8 where
    type Difference Int8 = Int8
    (-) = (Prelude.-)
instance Subtractive Int16 where
    type Difference Int16 = Int16
    (-) = (Prelude.-)
instance Subtractive Int32 where
    type Difference Int32 = Int32
    (-) = (Prelude.-)
instance Subtractive Int64 where
    type Difference Int64 = Int64
    (-) = (Prelude.-)
instance Subtractive Natural where
    type Difference Natural = Maybe Natural
    (-) a b
        | b > a     = Nothing
        | otherwise = Just (a Prelude.- b)
instance Subtractive Word where
    type Difference Word = Word
    (-) = (Prelude.-)
instance Subtractive Word8 where
    type Difference Word8 = Word8
    (-) = (Prelude.-)
instance Subtractive Word16 where
    type Difference Word16 = Word16
    (-) = (Prelude.-)
instance Subtractive Word32 where
    type Difference Word32 = Word32
    (-) = (Prelude.-)
instance Subtractive Word64 where
    type Difference Word64 = Word64
    (-) = (Prelude.-)
instance Subtractive Prelude.Float where
    type Difference Prelude.Float = Prelude.Float
    (-) = (Prelude.-)
instance Subtractive Prelude.Double where
    type Difference Prelude.Double = Prelude.Double
    (-) = (Prelude.-)
instance Subtractive Prelude.Char where
    type Difference Prelude.Char = Prelude.Int
    (-) a b = (Prelude.-) (charToInt a) (charToInt b)
