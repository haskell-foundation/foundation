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
    , Divisible(..)
    , Sign(..)
    , recip
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
