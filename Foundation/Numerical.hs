-- |
-- Module      : Foundation.Numerical
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
module Foundation.Numerical
    ( IsIntegral(..)
    , IsNatural(..)
    , Signed(..)
    , Additive(..)
    , Subtractive(..)
    , Multiplicative(..)
    , IDivisible(..)
    , Sign(..)
    ) where

import           Foundation.Internal.Base
import           Foundation.Numerical.Number
import           Foundation.Numerical.Additive
import           Foundation.Numerical.Subtractive
import           Foundation.Numerical.Multiplicative
import qualified Prelude

-- | Sign of a signed number
data Sign = SignNegative | SignZero | SignPositive
    deriving (Eq)

orderingToSign :: Ordering -> Sign
orderingToSign EQ = SignZero
orderingToSign GT = SignNegative
orderingToSign LT = SignPositive

-- | types that have sign and can be made absolute
class Signed a where
    {-# MINIMAL abs, signum #-}
    abs    :: a -> a
    signum :: a -> Sign

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

--odd n = (n `mod` 2) /= 0
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
