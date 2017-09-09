{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Basement.Types.Word128
    ( Word128(..)
    , (+)
    , (-)
    , (*)
    , quot
    , rem
    , bitwiseAnd
    , bitwiseOr
    , bitwiseXor
    , fromNatural
    ) where

import           GHC.Prim
import           GHC.Word
import           GHC.Types
import qualified Prelude (fromInteger, show, Num(..), quot, rem)
import           Data.Bits hiding (complement)
import qualified Data.Bits as Bits
import           Data.Function (on)

import           Basement.Compat.Base
import           Basement.Compat.Natural
import           Basement.Compat.Primitive (bool#)
import           Basement.Numerical.Conversion
import           Basement.Numerical.Number

-- | 128 bits Word
data Word128 = Word128 {-# UNPACK #-} !Word64
                       {-# UNPACK #-} !Word64
    deriving (Eq)

instance Show Word128 where
    show w = Prelude.show (toNatural w)
instance Enum Word128 where
    toEnum i = Word128 0 $ int64ToWord64 (intToInt64 i)
    fromEnum (Word128 _ a0) = wordToInt (word64ToWord a0)
    succ (Word128 a1 a0)
        | a0 == maxBound = Word128 (succ a1) 0
        | otherwise      = Word128 a1        (succ a0)
    pred (Word128 a1 a0)
        | a0 == minBound = Word128 (pred a1) maxBound
        | otherwise      = Word128 a1        (pred a0)
instance Bounded Word128 where
    minBound = Word128 minBound minBound
    maxBound = Word128 maxBound maxBound
instance Ord Word128 where
    compare (Word128 a1 a0) (Word128 b1 b0) =
        case compare a1 b1 of
            EQ -> compare a0 b0
            r  -> r
    (<) (Word128 a1 a0) (Word128 b1 b0) =
        case compare a1 b1 of
            EQ -> a0 < b0
            r  -> r == LT
    (<=) (Word128 a1 a0) (Word128 b1 b0) =
        case compare a1 b1 of
            EQ -> a0 <= b0
            r  -> r == LT

instance Integral Word128 where
    fromInteger = literal
instance HasNegation Word128 where
    negate = complement

instance IsIntegral Word128 where
    toInteger (Word128 a1 a0) =
        (toInteger a1 `unsafeShiftL` 64) .|.
        toInteger a0
instance IsNatural Word128 where
    toNatural (Word128 a1 a0) =
        (toNatural a1 `unsafeShiftL` 64) .|.
        toNatural a0

instance Prelude.Num Word128 where
    (+) = (+)
    (-) = (-)
    (*) = (*)

-- | Add 2 Word128
(+) :: Word128 -> Word128 -> Word128
(+) (Word128 (W64# a1) (W64# a0)) (Word128 (W64# b1) (W64# b0)) = Word128 (W64# s1) (W64# s0)
  where
    !(# carry, s0 #) = plusWord2# a0 b0
    s1               = plusWord# (plusWord# a1 b1) carry

-- temporary available until native operation available
applyBiWordOnNatural :: (Natural -> Natural -> Natural)
                     -> Word128
                     -> Word128
                     -> Word128
applyBiWordOnNatural f = (fromNatural .) . (f `on` toNatural)

-- | Subtract 2 Word128
(-) :: Word128 -> Word128 -> Word128
(-) a b
    | a >= b    = applyBiWordOnNatural (Prelude.-) a b
    | otherwise = complement $ applyBiWordOnNatural (Prelude.-) b a

-- | Multiplication
(*) :: Word128 -> Word128 -> Word128
(*) = applyBiWordOnNatural (Prelude.*)

-- | Division
quot :: Word128 -> Word128 -> Word128
quot = applyBiWordOnNatural Prelude.quot

-- | Modulo
rem :: Word128 -> Word128 -> Word128
rem = applyBiWordOnNatural Prelude.rem

-- | Bitwise and
bitwiseAnd :: Word128 -> Word128 -> Word128
bitwiseAnd (Word128 a1 a0) (Word128 b1 b0) =
    Word128 (a1 .&. b1) (a0 .&. b0)

-- | Bitwise or
bitwiseOr :: Word128 -> Word128 -> Word128
bitwiseOr (Word128 a1 a0) (Word128 b1 b0) =
    Word128 (a1 .|. b1) (a0 .|. b0)

-- | Bitwise xor
bitwiseXor :: Word128 -> Word128 -> Word128
bitwiseXor (Word128 a1 a0) (Word128 b1 b0) =
    Word128 (a1 `xor` b1) (a0 `xor` b0)

-- | Bitwise complement
complement :: Word128 -> Word128
complement (Word128 a1 a0) = Word128 (Bits.complement a1) (Bits.complement a0)

literal :: Integer -> Word128
literal i = Word128
    (Prelude.fromInteger (i `unsafeShiftR` 64))
    (Prelude.fromInteger i)

fromNatural :: Natural -> Word128
fromNatural n = Word128
    (Prelude.fromInteger (naturalToInteger n `unsafeShiftR` 64))
    (Prelude.fromInteger $ naturalToInteger n)
