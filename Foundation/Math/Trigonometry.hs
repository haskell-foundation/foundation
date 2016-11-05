{-# LANGUAGE TypeSynonymInstances #-}
module Foundation.Math.Trigonometry
    ( Trigonometry(..)
    ) where

import           Foundation.Internal.Base
import           Foundation.Numerical
import qualified Prelude

class Trigonometry a where
    pi    :: a
    sin   :: a -> a
    cos   :: a -> a
    tan   :: a -> a
    asin  :: a -> a
    acos  :: a -> a
    atan  :: a -> a
    sinh  :: a -> a
    cosh  :: a -> a
    tanh  :: a -> a
    asinh :: a -> a
    acosh :: a -> a
    atanh :: a -> a

instance Trigonometry FP32 where
    pi = Prelude.pi
    sin = Prelude.sin
    cos = Prelude.cos
    tan = Prelude.tan
    asin = Prelude.asin
    acos = Prelude.acos
    atan = Prelude.atan
    sinh = Prelude.sinh
    cosh = Prelude.cosh
    tanh = Prelude.tanh
    asinh = Prelude.asinh
    acosh = Prelude.acosh
    atanh = Prelude.atanh

instance Trigonometry FP64 where
    pi = Prelude.pi
    sin = Prelude.sin
    cos = Prelude.cos
    tan = Prelude.tan
    asin = Prelude.asin
    acos = Prelude.acos
    atan = Prelude.atan
    sinh = Prelude.sinh
    cosh = Prelude.cosh
    tanh = Prelude.tanh
    asinh = Prelude.asinh
    acosh = Prelude.acosh
    atanh = Prelude.atanh
