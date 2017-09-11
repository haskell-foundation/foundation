{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Basement.Types.Word256
    ( Word256(..)
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
import           Foreign.C
import           Foreign.Ptr
import           Foreign.Storable

import           Basement.Compat.Base
import           Basement.Compat.Natural
import           Basement.Compat.Primitive (bool#)
import           Basement.Numerical.Conversion
import           Basement.Numerical.Number

#include "MachDeps.h"

-- | 256 bits Word
data Word256 = Word256 {-# UNPACK #-} !Word64
                       {-# UNPACK #-} !Word64
                       {-# UNPACK #-} !Word64
                       {-# UNPACK #-} !Word64
    deriving (Eq)

instance Show Word256 where
    show w = Prelude.show (toNatural w)
instance Enum Word256 where
    toEnum i = Word256 0 0 0 $ int64ToWord64 (intToInt64 i)
    fromEnum (Word256 _ _ _ a0) = wordToInt (word64ToWord a0)
    succ (Word256 a3 a2 a1 a0)
        | a0 == maxBound =
            if a1 == maxBound
                then if a2 == maxBound
                        then Word256 (succ a3) 0 0 0
                        else Word256 a3 (succ a2) 0 0
                else Word256 a3 a2 (succ a1) 0
        | otherwise      = Word256 a3 a2 a1        (succ a0)
    pred (Word256 a3 a2 a1 a0)
        | a0 == minBound =
            if a1 == minBound
                then if a2 == minBound
                        then Word256 (pred a3) maxBound maxBound maxBound
                        else Word256 a3 (pred a2) maxBound maxBound
                else Word256 a3 a2 (pred a1) maxBound
        | otherwise      = Word256 a3 a2 a1        (pred a0)
instance Bounded Word256 where
    minBound = Word256 minBound minBound minBound minBound
    maxBound = Word256 maxBound maxBound maxBound maxBound
instance Ord Word256 where
    compare (Word256 a3 a2 a1 a0) (Word256 b3 b2 b1 b0) =
        compareEq a3 b3 $ compareEq a2 b2 $ compareEq a1 b1 $ compare a0 b0
      where compareEq x y next =
                case compare x y of
                    EQ -> next
                    r  -> r
    (<) (Word256 a3 a2 a1 a0) (Word256 b3 b2 b1 b0) =
        compareLt a3 b3 $ compareLt a2 b2 $ compareLt a1 b1 (a0 < b0)
      where compareLt x y next =
                case compare x y of
                    EQ -> next
                    r  -> r == LT
instance Storable Word256 where
    sizeOf _ = 32
    alignment _ = 32
    peek p = Word256 <$> peek (castPtr p            )
                     <*> peek (castPtr p `plusPtr` 8)
                     <*> peek (castPtr p `plusPtr` 16)
                     <*> peek (castPtr p `plusPtr` 24)
    poke p (Word256 a3 a2 a1 a0) = do
        poke (castPtr p             ) a3
        poke (castPtr p `plusPtr` 8 ) a2
        poke (castPtr p `plusPtr` 16) a1
        poke (castPtr p `plusPtr` 24) a0
                    
instance Integral Word256 where
    fromInteger = literal
instance HasNegation Word256 where
    negate = complement

instance IsIntegral Word256 where
    toInteger (Word256 a3 a2 a1 a0) =
        (toInteger a3 `Bits.unsafeShiftL` 192) Bits..|.
        (toInteger a2 `Bits.unsafeShiftL` 128) Bits..|.
        (toInteger a1 `Bits.unsafeShiftL` 64) Bits..|.
        toInteger a0
instance IsNatural Word256 where
    toNatural (Word256 a3 a2 a1 a0) =
        (toNatural a3 `Bits.unsafeShiftL` 192) Bits..|.
        (toNatural a2 `Bits.unsafeShiftL` 128) Bits..|.
        (toNatural a1 `Bits.unsafeShiftL` 64) Bits..|.
        toNatural a0

instance Prelude.Num Word256 where
    abs w = w
    signum w@(Word256 a3 a2 a1 a0)
        | a3 == 0 && a2 == 0 && a1 == 0 && a0 == 0 = w
        | otherwise                                = Word256 0 0 0 1
    fromInteger = literal
    (+) = (+)
    (-) = (-)
    (*) = (*)

-- | Add 2 Word256
(+) :: Word256 -> Word256 -> Word256
#if WORD_SIZE_IN_BITS < 64
(+) = applyBiWordOnNatural (Prelude.+)
#else
(+) (Word256 (W64# a3) (W64# a2) (W64# a1) (W64# a0))
    (Word256 (W64# b3) (W64# b2) (W64# b1) (W64# b0)) =
    Word256 (W64# s3) (W64# s2) (W64# s1) (W64# s0)
  where
    !(# c0, s0 #) = plusWord2# a0 b0
    !(# c1, s1 #) = plusWord3# a1 b1 c0
    !(# c2, s2 #) = plusWord3# a2 b2 c1
    !s3           = plusWord3NoCarry# a3 b3 c2

    plusWord3NoCarry# a b c = plusWord# (plusWord# a b) c
    plusWord3# a b c
        | bool# (eqWord# carry 0##) = plusWord2# x c
        | otherwise                 =
            case plusWord2# x c of
                (# carry2, x' #)
                    | bool# (eqWord# carry2 0##) -> (# carry, x' #)
                    | otherwise                  -> (# plusWord# carry carry2, x' #)
      where
        (# carry, x #) = plusWord2# a b
#endif

-- temporary available until native operation available
applyBiWordOnNatural :: (Natural -> Natural -> Natural)
                     -> Word256
                     -> Word256
                     -> Word256
applyBiWordOnNatural f = (fromNatural .) . (f `on` toNatural)

-- | Subtract 2 Word256
(-) :: Word256 -> Word256 -> Word256
(-) a b
    | a >= b    = applyBiWordOnNatural (Prelude.-) a b
    | otherwise = complement $ applyBiWordOnNatural (Prelude.-) b a

-- | Multiplication
(*) :: Word256 -> Word256 -> Word256
(*) = applyBiWordOnNatural (Prelude.*)

-- | Division
quot :: Word256 -> Word256 -> Word256
quot = applyBiWordOnNatural Prelude.quot

-- | Modulo
rem :: Word256 -> Word256 -> Word256
rem = applyBiWordOnNatural Prelude.rem

-- | Bitwise and
bitwiseAnd :: Word256 -> Word256 -> Word256
bitwiseAnd (Word256 a3 a2 a1 a0) (Word256 b3 b2 b1 b0) =
    Word256 (a3 Bits..&. b3) (a2 Bits..&. b2)  (a1 Bits..&. b1) (a0 Bits..&. b0)

-- | Bitwise or
bitwiseOr :: Word256 -> Word256 -> Word256
bitwiseOr (Word256 a3 a2 a1 a0) (Word256 b3 b2 b1 b0) =
    Word256 (a3 Bits..|. b3) (a2 Bits..|. b2)  (a1 Bits..|. b1) (a0 Bits..|. b0)

-- | Bitwise xor
bitwiseXor :: Word256 -> Word256 -> Word256
bitwiseXor (Word256 a3 a2 a1 a0) (Word256 b3 b2 b1 b0) =
    Word256 (a3 `Bits.xor` b3) (a2 `Bits.xor` b2)  (a1 `Bits.xor` b1) (a0 `Bits.xor` b0)

-- | Bitwise complement
complement :: Word256 -> Word256
complement (Word256 a3 a2 a1 a0) =
    Word256 (Bits.complement a3) (Bits.complement a2) (Bits.complement a1) (Bits.complement a0)

literal :: Integer -> Word256
literal i = Word256
    (Prelude.fromInteger (i `Bits.unsafeShiftR` 192))
    (Prelude.fromInteger (i `Bits.unsafeShiftR` 128))
    (Prelude.fromInteger (i `Bits.unsafeShiftR` 64))
    (Prelude.fromInteger i)

fromNatural :: Natural -> Word256
fromNatural n = Word256
    (Prelude.fromInteger (naturalToInteger n `Bits.unsafeShiftR` 192))
    (Prelude.fromInteger (naturalToInteger n `Bits.unsafeShiftR` 128))
    (Prelude.fromInteger (naturalToInteger n `Bits.unsafeShiftR` 64))
    (Prelude.fromInteger $ naturalToInteger n)
