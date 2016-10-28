-- |
-- Module      : Foundation.Internal.NumLiteral
-- License     : BSD-style
-- Maintainer  : Foundation
--
-- Literal support for Integral and Fractional
--
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Foundation.Internal.NumLiteral
    ( Integral(..)
    , Fractional(..)
    ) where

import           Prelude (Int, Word, Integer, Rational, Float, Double)
import           Data.Word (Word8, Word16, Word32, Word64)
import           Data.Int (Int8, Int16, Int32, Int64)
import qualified Prelude
import           Foundation.Internal.Natural

-- | Integral Literal support
-- 
-- e.g. 123 :: Integer
--      123 :: Word8
class Integral a where
    fromInteger :: Integer -> a

-- | Fractional Literal support
-- 
-- e.g. 1.2  :: Double
--      0.03 :: Float
class Fractional a where
    fromRational :: Rational -> a

instance Integral Integer where
    fromInteger a = a
instance Integral Natural where
    fromInteger a = Prelude.fromInteger a
instance Integral Int where
    fromInteger a = Prelude.fromInteger a
instance Integral Word where
    fromInteger a = Prelude.fromInteger a
instance Integral Word8 where
    fromInteger a = Prelude.fromInteger a
instance Integral Word16 where
    fromInteger a = Prelude.fromInteger a
instance Integral Word32 where
    fromInteger a = Prelude.fromInteger a
instance Integral Word64 where
    fromInteger a = Prelude.fromInteger a
instance Integral Int8 where
    fromInteger a = Prelude.fromInteger a
instance Integral Int16 where
    fromInteger a = Prelude.fromInteger a
instance Integral Int32 where
    fromInteger a = Prelude.fromInteger a
instance Integral Int64 where
    fromInteger a = Prelude.fromInteger a

instance Fractional Rational where
    fromRational a = Prelude.fromRational a
instance Fractional Float where
    fromRational a = Prelude.fromRational a
instance Fractional Double where
    fromRational a = Prelude.fromRational a
