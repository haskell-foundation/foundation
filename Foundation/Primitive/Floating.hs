module Foundation.Primitive.Floating
    ( integerToDouble
    , naturalToDouble
    , doubleExponant
    , integerToFloat
    , naturalToFloat
    ) where

import           GHC.Types
import           Foundation.Internal.Base
import           Foundation.Internal.Natural
import qualified Prelude (fromInteger, toInteger, (^^))

integerToDouble :: Integer -> Double
integerToDouble = Prelude.fromInteger
-- this depends on integer-gmp
--integerToDouble i = D# (doubleFromInteger i)

naturalToDouble :: Natural -> Double
naturalToDouble = integerToDouble . Prelude.toInteger

doubleExponant :: Double -> Int -> Double
doubleExponant = (Prelude.^^)

integerToFloat :: Integer -> Float
integerToFloat = Prelude.fromInteger

naturalToFloat :: Natural -> Float
naturalToFloat = integerToFloat . Prelude.toInteger
