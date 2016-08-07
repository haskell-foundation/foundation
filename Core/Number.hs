-- |
-- Module      : Core.Number
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- Compared to the Haskell hierarchy of number classes
-- this provide a more flexible approach that is closer to the
-- mathematical foundation (group, field, etc)
--
-- This try to only provide one feature per class, at the expense of
-- the number of classes.
--
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
module Core.Number
    ( Number(..)
    , fromInteger
    , Signed(..)
    , Additive(..)
    , Multiplicative(..)
    , Subtractive(..)
    , Divisible(..)
    , Sign(..)
    ) where

import           Core.Internal.Base
import qualified Prelude

infixl 7  *
infixl 6  +, -
infixr 8  ^

-- | Sign of a signed number
data Sign = Negative | Zero | Positive
    deriving (Eq)

orderingToSign :: Ordering -> Sign
orderingToSign EQ = Zero
orderingToSign GT = Negative
orderingToSign LT = Positive

-- | Number literals, convertible through the generic Integer type.
--
-- all number are Enum'erable, meaning that you can move to
-- next element
class (Eq a, Ord a, Prelude.Num a, Enum a, Additive a, Subtractive a, Difference a ~ a, Multiplicative a, Divisible a) => Number a where
    {-# MINIMAL toInteger #-}
    --fromInteger  :: Integer -> a
    toInteger    :: a -> Integer

-- | convert an Integer to a type having the Number constraint
fromInteger :: Number a => Integer -> a
fromInteger = Prelude.fromInteger

-- | Number literals that can be negative
class Number a => Signed a where
    {-# MINIMAL abs, signum #-}
    abs    :: a -> a
    signum :: a -> Sign

-- | Represent class of things that can be added together,
-- contains a neutral element and is commutative.
--
-- * x + azero = x
-- * azero + x = x
-- * x + y = y + x
--
class Additive a where
    {-# MINIMAL azero, (+) #-}
    azero :: a           -- the identity element over addition
    (+)   :: a -> a -> a -- the addition

    scale :: Number n => n -> a -> a -- scale: repeated addition
    default scale :: Number n => n -> a -> a
    scale 0 _ = azero
    scale 1 a = a
    scale 2 a = a + a
    scale n a
        | n < 0     = error "cannot scale by negative number"
        | otherwise = a + scale (pred n) a -- TODO optimise. define by group of 2.

-- | Represent class of things that can be multiplied together
--
-- * x * midentity = x
-- * midentity * x = x
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
    (^) :: Number n => a -> n -> a
    (^) = power

-- | Represent class of things that can be subtracted.
--
-- Note that the result is not necessary of the same type
-- as the operand depending on the actual type.
--
-- For example:
-- e.g. (-) :: Int -> Int -> Int
--      (-) :: DateTime -> DateTime -> Seconds
--      (-) :: Ptr a -> Ptr a -> PtrDiff
class Subtractive a where
    type Difference a
    (-) :: a -> a -> Difference a

-- | Represent class of things that can be divided
--
-- (x ‘div‘  y) * y + (x ‘mod‘ y) == x
class Multiplicative a => Divisible a where
    {-# MINIMAL (div, mod) | divMod #-}
    div :: a -> a -> a
    div a b = fst $ divMod a b
    mod :: a -> a -> a
    mod a b = snd $ divMod a b
    divMod :: a -> a -> (a, a)
    divMod a b = (div a b, mod a b)

instance Number Integer where
    toInteger i = i
instance Number Int where
    toInteger i = Prelude.fromIntegral i
instance Number Int8 where
    toInteger i = Prelude.fromIntegral i
instance Number Int16 where
    toInteger i = Prelude.fromIntegral i
instance Number Int32 where
    toInteger i = Prelude.fromIntegral i
instance Number Int64 where
    toInteger i = Prelude.fromIntegral i

instance Signed Integer where
    abs = Prelude.abs
    signum = orderingToSign . compare 0
instance Signed Int where
    abs = Prelude.abs
    signum = orderingToSign . compare 0
instance Signed Int8 where
    abs = Prelude.abs
    signum = orderingToSign . compare 0
instance Signed Int16 where
    abs = Prelude.abs
    signum = orderingToSign . compare 0
instance Signed Int32 where
    abs = Prelude.abs
    signum = orderingToSign . compare 0
instance Signed Int64 where
    abs = Prelude.abs
    signum = orderingToSign . compare 0

instance Number Word where
    toInteger i = Prelude.fromIntegral i
instance Number Word8 where
    toInteger i = Prelude.fromIntegral i
instance Number Word16 where
    toInteger i = Prelude.fromIntegral i
instance Number Word32 where
    toInteger i = Prelude.fromIntegral i
instance Number Word64 where
    toInteger i = Prelude.fromIntegral i

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

scaleNum :: (Prelude.Num a, Number n) => n -> a -> a
scaleNum n a = (Prelude.fromIntegral $ toInteger n) Prelude.* a

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

instance Multiplicative Integer where
    midentity = 1
    (*) = (Prelude.*)
    (^) = power
instance Multiplicative Int where
    midentity = 1
    (*) = (Prelude.*)
    (^) = power
instance Multiplicative Int8 where
    midentity = 1
    (*) = (Prelude.*)
    (^) = power
instance Multiplicative Int16 where
    midentity = 1
    (*) = (Prelude.*)
    (^) = power
instance Multiplicative Int32 where
    midentity = 1
    (*) = (Prelude.*)
    (^) = power
instance Multiplicative Int64 where
    midentity = 1
    (*) = (Prelude.*)
    (^) = power
instance Multiplicative Word where
    midentity = 1
    (*) = (Prelude.*)
    (^) = power
instance Multiplicative Word8 where
    midentity = 1
    (*) = (Prelude.*)
    (^) = power
instance Multiplicative Word16 where
    midentity = 1
    (*) = (Prelude.*)
    (^) = power
instance Multiplicative Word32 where
    midentity = 1
    (*) = (Prelude.*)
    (^) = power
instance Multiplicative Word64 where
    midentity = 1
    (*) = (Prelude.*)
    (^) = power

power :: (Number n, Multiplicative a) => a -> n -> a
power a n
    | n < 0     = error "(^): cannot use negative exponent"
    | n == 0    = midentity
    | otherwise = squaring midentity a n
  where
    squaring y x i
        | i == 0    = y
        | i == 1    = x * y
        | even i    = squaring y (x*x) (i`div`2)
        | otherwise = squaring (x*y) (x*x) (pred i`div` 2)

--odd n = (n `mod` 2) /= 0
even :: Number n => n -> Bool
even n = (n `mod` 2) == 0

instance Divisible Integer where
    div = Prelude.div
    mod = Prelude.mod
instance Divisible Int where
    div = Prelude.div
    mod = Prelude.mod
instance Divisible Int8 where
    div = Prelude.div
    mod = Prelude.mod
instance Divisible Int16 where
    div = Prelude.div
    mod = Prelude.mod
instance Divisible Int32 where
    div = Prelude.div
    mod = Prelude.mod
instance Divisible Int64 where
    div = Prelude.div
    mod = Prelude.mod
instance Divisible Word where
    div = Prelude.quot
    mod = Prelude.rem
instance Divisible Word8 where
    div = Prelude.quot
    mod = Prelude.rem
instance Divisible Word16 where
    div = Prelude.quot
    mod = Prelude.rem
instance Divisible Word32 where
    div = Prelude.quot
    mod = Prelude.rem
instance Divisible Word64 where
    div = Prelude.quot
    mod = Prelude.rem

-- {-# RULES scaleNum = * #-}

--numUpSize :: a -> b
--numUpSize
-----------------------------------
{- haskell numerical classes:
 -
Prelude> :i Fractional
class Num a => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a
  	-- Defined in ‘GHC.Real’
instance Fractional Float -- Defined in ‘GHC.Float’
instance Fractional Double -- Defined in ‘GHC.Float’

Prelude> :i Integral
class (Real a, Enum a) => Integral a where
  quot :: a -> a -> a
  rem :: a -> a -> a
  div :: a -> a -> a
  mod :: a -> a -> a
  quotRem :: a -> a -> (a, a)
  divMod :: a -> a -> (a, a)
  toInteger :: a -> Integer
  	-- Defined in ‘GHC.Real’
instance Integral Word -- Defined in ‘GHC.Real’
instance Integral Integer -- Defined in ‘GHC.Real’
instance Integral Int -- Defined in ‘GHC.Real’

Prelude> :i Real
class (Num a, Ord a) => Real a where
  toRational :: a -> Rational
  	-- Defined in ‘GHC.Real’
instance Real Word -- Defined in ‘GHC.Real’
instance Real Integer -- Defined in ‘GHC.Real’
instance Real Int -- Defined in ‘GHC.Real’
instance Real Float -- Defined in ‘GHC.Float’
instance Real Double -- Defined in ‘GHC.Float’
-}
