{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Foundation.Primitive.Numerical.Multiplicative
    ( Multiplicative(..)
    , IDivisible(..)
    , Divisible(..)
    , recip
    ) where

import           Foundation.Internal.Base
import           Foundation.Internal.Natural
import           Foundation.Primitive.Numerical.Number
import           Foundation.Primitive.Numerical.Additive
import qualified Prelude

-- | Represent class of things that can be multiplied together
--
-- > x * midentity = x
-- > midentity * x = x
class Multiplicative a where
    {-# MINIMAL midentity, (*) #-}
    -- | Identity element over multiplication
    midentity :: a

    -- | Multiplication of 2 elements that result in another element
    (*) :: a -> a -> a

    -- | Raise to power, repeated multiplication
    -- e.g.
    -- > a ^ 2 = a * a
    -- > a ^ 10 = (a ^ 5) * (a ^ 5) ..
    --(^) :: (IsNatural n) => a -> n -> a
    (^) :: (IsNatural n, IDivisible n) => a -> n -> a
    -- default (^) :: (IDivisible n, IsNatural n, Multiplicative a) => a -> n -> a
    (^) = power

-- | Represent types that supports an euclidian division
--
-- > (x ‘div‘ y) * y + (x ‘mod‘ y) == x
class (Additive a, Multiplicative a) => IDivisible a where
    {-# MINIMAL (div, mod) | divMod #-}
    div :: a -> a -> a
    div a b = fst $ divMod a b
    mod :: a -> a -> a
    mod a b = snd $ divMod a b
    divMod :: a -> a -> (a, a)
    divMod a b = (div a b, mod a b)

-- | Support for division between same types
--
-- This is likely to change to represent specific mathematic divisions
class Multiplicative a => Divisible a where
    {-# MINIMAL (/) #-}
    (/) :: a -> a -> a

infixl 7  *, /
infixr 8  ^

instance Multiplicative Integer where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative Int where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative Int8 where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative Int16 where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative Int32 where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative Int64 where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative Natural where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative Word where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative Word8 where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative Word16 where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative Word32 where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative Word64 where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative Prelude.Float where
    midentity = 1.0
    (*) = (Prelude.*)
instance Multiplicative Prelude.Double where
    midentity = 1.0
    (*) = (Prelude.*)
instance Multiplicative Prelude.Rational where
    midentity = 1.0
    (*) = (Prelude.*)

instance IDivisible Integer where
    div = Prelude.div
    mod = Prelude.mod
instance IDivisible Int where
    div = Prelude.div
    mod = Prelude.mod
instance IDivisible Int8 where
    div = Prelude.div
    mod = Prelude.mod
instance IDivisible Int16 where
    div = Prelude.div
    mod = Prelude.mod
instance IDivisible Int32 where
    div = Prelude.div
    mod = Prelude.mod
instance IDivisible Int64 where
    div = Prelude.div
    mod = Prelude.mod
instance IDivisible Natural where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible Word where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible Word8 where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible Word16 where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible Word32 where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible Word64 where
    div = Prelude.quot
    mod = Prelude.rem

instance Divisible Prelude.Rational where
    (/) = (Prelude./)
instance Divisible Float where
    (/) = (Prelude./)
instance Divisible Double where
    (/) = (Prelude./)

recip :: Divisible a => a -> a
recip x = midentity / x

power :: (IsNatural n, IDivisible n, Multiplicative a) => a -> n -> a
power a n
    | n == 0    = midentity
    | otherwise = squaring midentity a n
  where
    squaring y x i
        | i == 0    = y
        | i == 1    = x * y
        | even i    = squaring y (x*x) (i`div`2)
        | otherwise = squaring (x*y) (x*x) (pred i`div` 2)

even :: (IDivisible n, IsIntegral n) => n -> Bool
even n = (n `mod` 2) == 0
