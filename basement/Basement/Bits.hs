-- |
-- Module      : Basement.Bits
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
-- Stability   : experimental
-- Portability : portable
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NegativeLiterals #-}

#include "MachDeps.h"

module Basement.Bits
    ( BitOps(..)
    , FiniteBitsOps(..)
    , Bits
    , toBits
    , allOne
    ) where

import Basement.Compat.Base
import Basement.Compat.Natural
import Basement.Numerical.Additive
import Basement.Numerical.Subtractive
import Basement.Numerical.Multiplicative
import Basement.Types.OffsetSize
import Basement.Types.Word128 (Word128)
import qualified Basement.Types.Word128 as Word128
import Basement.Types.Word256 (Word256)
import qualified Basement.Types.Word256 as Word256
import Basement.IntegralConv (wordToInt)
import Basement.Nat

import qualified Prelude
import qualified Data.Bits as OldBits
import Data.Maybe (fromMaybe)
import Data.Proxy
import GHC.Base hiding ((.))
import GHC.Prim
import GHC.Types
import GHC.Word
import GHC.Int
import Basement.Compat.Primitive

#if WORD_SIZE_IN_BITS < 64
#if __GLASGOW_HASKELL__ >= 904
import GHC.Exts
#else
import GHC.IntWord64
#endif
#endif

-- | operation over finite bits
class FiniteBitsOps bits where
    -- | get the number of bits in the given object
    --
    numberOfBits :: bits -> CountOf Bool

    -- | rotate the given bit set.
    rotateL :: bits -> CountOf Bool -> bits
    -- | rotate the given bit set.
    rotateR :: bits -> CountOf Bool -> bits

    -- | count of number of bit set to 1 in the given bit set.
    popCount :: bits -> CountOf Bool

    -- | reverse all bits in the argument
    bitFlip   :: bits -> bits

    -- | count of the number of leading zeros
    countLeadingZeros :: bits -> CountOf Bool
    default countLeadingZeros :: BitOps bits => bits -> CountOf Bool
    countLeadingZeros n = loop stop azero
      where
        stop = numberOfBits n
        loop idx count
            | idx == azero = count
            | isBitSet n (sizeAsOffset idx) = count
            | otherwise = loop (fromMaybe azero (idx - 1)) (count + 1)

    -- | count of the number of trailing zeros
    countTrailingZeros :: bits -> CountOf Bool
    default countTrailingZeros :: BitOps bits => bits -> CountOf Bool
    countTrailingZeros n = loop azero
      where
        stop = numberOfBits n
        loop count
            | count == stop = count
            | isBitSet n (sizeAsOffset count) = count
            | otherwise = loop (count + 1)

-- | operation over bits
class BitOps bits where
    (.&.)     :: bits -> bits -> bits
    (.|.)     :: bits -> bits -> bits
    (.^.)     :: bits -> bits -> bits
    (.<<.)    :: bits -> CountOf Bool -> bits
    (.>>.)    :: bits -> CountOf Bool -> bits
    -- | construct a bit set with the bit at the given index set.
    bit       :: Offset Bool -> bits
    default bit :: Integral bits => Offset Bool -> bits
    bit n = 1 .<<. (offsetAsSize n)

    -- | test the bit at the given index is set
    isBitSet  :: bits -> Offset Bool -> Bool
    default isBitSet :: (Integral bits, Eq bits) => bits -> Offset Bool -> Bool
    isBitSet x n = x .&. (bit n) /= 0

    -- | set the bit at the given index
    setBit    :: bits -> Offset Bool -> bits
    default setBit :: Integral bits => bits -> Offset Bool -> bits
    setBit x n = x .|. (bit n)

    -- | clear the bit at the given index
    clearBit  :: bits -> Offset Bool -> bits
    default clearBit :: FiniteBitsOps bits => bits -> Offset Bool -> bits
    clearBit x n = x .&. (bitFlip (bit n))

infixl 8 .<<., .>>., `rotateL`, `rotateR`
infixl 7 .&.
infixl 6 .^.
infixl 5 .|.

-- | Bool set of 'n' bits.
--
newtype Bits (n :: Nat) = Bits { bitsToNatural :: Natural }
  deriving (Show, Eq, Ord, Typeable)

-- | convenient Type Constraint Alias fot 'Bits' functions
type SizeValid n = (KnownNat n, 1 <= n)

-- convert an 'Int' into a 'Natural'.
-- This functions is not meant to be exported
lift :: Int -> Natural
lift = Prelude.fromIntegral
{-# INLINABLE lift #-}

-- | convert the given 'Natural' into a 'Bits' of size 'n'
--
-- if bits that are not within the boundaries of the 'Bits n' will be truncated.
toBits :: SizeValid n => Natural -> Bits n
toBits nat = Bits nat .&. allOne

-- | construct a 'Bits' with all bits set.
--
-- this function is equivalet to 'maxBound'
allOne :: forall n . SizeValid n => Bits n
allOne = Bits (2 Prelude.^ n Prelude.- midentity)
  where
    n = natVal (Proxy @n)

instance SizeValid n => Enum (Bits n) where
    toEnum i | i < 0 && lift i > bitsToNatural maxi = error "Bits n not within bound"
             | otherwise                            = Bits (lift i)
      where maxi = allOne :: Bits n
    fromEnum (Bits n) = fromEnum n
instance SizeValid n => Bounded (Bits n) where
    minBound = azero
    maxBound = allOne
instance SizeValid n => Additive (Bits n) where
    azero = Bits 0
    (+) (Bits a) (Bits b) = toBits (a + b)
    scale n (Bits a) = toBits (scale n a)
instance SizeValid n => Subtractive (Bits n) where
    type Difference (Bits n) = Bits n
    (-) (Bits a) (Bits b) = maybe azero toBits (a - b)
instance SizeValid n => Multiplicative (Bits n) where
    midentity = Bits 1
    (*) (Bits a) (Bits b) = Bits (a Prelude.* b)
instance SizeValid n => IDivisible (Bits n) where
    div (Bits a) (Bits b) = Bits (a `Prelude.div` b)
    mod (Bits a) (Bits b) = Bits (a `Prelude.mod` b)
    divMod (Bits a) (Bits b) = let (q, r) = Prelude.divMod a b in (Bits q, Bits r)

instance SizeValid n => BitOps (Bits n) where
    (.&.)    (Bits a) (Bits b)    = Bits (a OldBits..&. b)
    (.|.)    (Bits a) (Bits b)    = Bits (a OldBits..|. b)
    (.^.)    (Bits a) (Bits b)    = Bits (a `OldBits.xor` b)
    (.<<.)   (Bits a) (CountOf w) = Bits (a `OldBits.shiftL` w)
    (.>>.)   (Bits a) (CountOf w) = Bits (a `OldBits.shiftR` w)
    bit               (Offset w)  = Bits (OldBits.bit w)
    isBitSet (Bits a) (Offset w)  = OldBits.testBit a w
    setBit   (Bits a) (Offset w)  = Bits (OldBits.setBit a w)
    clearBit (Bits a) (Offset w)  = Bits (OldBits.clearBit a w)
instance (SizeValid n, NatWithinBound (CountOf Bool) n) => FiniteBitsOps (Bits n) where
    bitFlip (Bits a) = Bits (OldBits.complement a)
    numberOfBits _ = natValCountOf (Proxy @n)
    rotateL a i = (a .<<. i) .|. (a .>>. d)
      where
        n = natValCountOf (Proxy :: Proxy n)
        d = fromMaybe (fromMaybe (error "impossible") (i - n)) (n - i)
    rotateR a i = (a .>>. i) .|. (a .<<. d)
      where
        n = natValCountOf (Proxy :: Proxy n)
        d = fromMaybe (fromMaybe (error "impossible") (i - n)) (n - i)
    popCount (Bits n) = CountOf (OldBits.popCount n)

-- Bool ------------------------------------------------------------------------

instance FiniteBitsOps Bool where
    numberOfBits _ = 1
    rotateL x _ = x
    rotateR x _ = x
    popCount True = 1
    popCount False = 0
    bitFlip  = not
    countLeadingZeros True  = 0
    countLeadingZeros False = 1
    countTrailingZeros True  = 0
    countTrailingZeros False = 1
instance BitOps Bool where
    (.&.) = (&&)
    (.|.) = (||)
    (.^.) = (/=)
    x .<<. 0 = x
    _ .<<. _ = False
    x .>>. 0 = x
    _ .>>. _ = False
    bit 0 = True
    bit _ = False
    isBitSet x 0 = x
    isBitSet _ _ = False
    setBit _ 0 = True
    setBit _ _ = False
    clearBit _ 0 = False
    clearBit x _ = x

-- Word8 ----------------------------------------------------------------------

instance FiniteBitsOps Word8 where
    numberOfBits _ = 8
    rotateL w (CountOf i) = w `OldBits.rotateL` i
    rotateR w (CountOf i) = w `OldBits.rotateR` i
    bitFlip = OldBits.complement
    popCount (W8# x#) = CountOf $ wordToInt (W# (popCnt8# (word8ToWord# x#)))
    countLeadingZeros (W8# w) = CountOf (wordToInt (W# (clz8# (word8ToWord# w))))
    countTrailingZeros (W8# w) = CountOf (wordToInt (W# (ctz8# (word8ToWord# w))))
instance BitOps Word8 where
    (.&.)    a b    = (a OldBits..&. b)
    (.|.)    a b    = (a OldBits..|. b)
    (.^.)    a b    = (a `OldBits.xor` b)
    (.<<.)   a (CountOf w) = (a `OldBits.shiftL` w)
    (.>>.)   a (CountOf w) = (a `OldBits.shiftR` w)

-- Word16 ---------------------------------------------------------------------

instance FiniteBitsOps Word16 where
    numberOfBits _ = 16
    rotateL w (CountOf i) = w `OldBits.rotateL` i
    rotateR w (CountOf i) = w `OldBits.rotateR` i
    bitFlip = OldBits.complement
    popCount (W16# x#) = CountOf $ wordToInt (W# (popCnt16# (word16ToWord# x#)))
    countLeadingZeros (W16# w#) = CountOf $ wordToInt (W# (clz16# (word16ToWord# w#)))
    countTrailingZeros (W16# w#) = CountOf $ wordToInt (W# (ctz16# (word16ToWord# w#)))
instance BitOps Word16 where
    (.&.)    a b    = (a OldBits..&. b)
    (.|.)    a b    = (a OldBits..|. b)
    (.^.)    a b    = (a `OldBits.xor` b)
    (.<<.)   a (CountOf w) = (a `OldBits.shiftL` w)
    (.>>.)   a (CountOf w) = (a `OldBits.shiftR` w)

-- Word32 ---------------------------------------------------------------------

instance FiniteBitsOps Word32 where
    numberOfBits _ = 32
    rotateL w (CountOf i) = w `OldBits.rotateL` i
    rotateR w (CountOf i) = w `OldBits.rotateR` i
    bitFlip = OldBits.complement
    popCount (W32# x#) = CountOf $ wordToInt (W# (popCnt32# (word32ToWord# x#)))
    countLeadingZeros (W32# w#) = CountOf $ wordToInt (W# (clz32# (word32ToWord# w#)))
    countTrailingZeros (W32# w#) = CountOf $ wordToInt (W# (ctz32# (word32ToWord# w#)))
instance BitOps Word32 where
    (.&.)    a b    = (a OldBits..&. b)
    (.|.)    a b    = (a OldBits..|. b)
    (.^.)    a b    = (a `OldBits.xor` b)
    (.<<.)   a (CountOf w) = (a `OldBits.shiftL` w)
    (.>>.)   a (CountOf w) = (a `OldBits.shiftR` w)

-- Word ---------------------------------------------------------------------

#if WORD_SIZE_IN_BITS == 64
instance FiniteBitsOps Word where
    numberOfBits _ = 64
    rotateL w (CountOf i) = w `OldBits.rotateL` i
    rotateR w (CountOf i) = w `OldBits.rotateR` i
    bitFlip = OldBits.complement
#if __GLASGOW_HASKELL__ >= 904
    popCount (W# x#) = CountOf $ wordToInt (W# (popCnt64# (wordToWord64# x#)))
    countLeadingZeros (W# w#) = CountOf $ wordToInt (W# (clz64# (wordToWord64# w#)))
    countTrailingZeros (W# w#) = CountOf $ wordToInt (W# (ctz64# (wordToWord64# w#)))
#else
    popCount (W# x#) = CountOf $ wordToInt (W# (popCnt64# x#))
    countLeadingZeros (W# w#) = CountOf $ wordToInt (W# (clz64# w#))
    countTrailingZeros (W# w#) = CountOf $ wordToInt (W# (ctz64# w#))
#endif
#else
instance FiniteBitsOps Word where
    numberOfBits _ = 32
    rotateL w (CountOf i) = w `OldBits.rotateL` i
    rotateR w (CountOf i) = w `OldBits.rotateR` i
    bitFlip = OldBits.complement
    popCount (W# x#) = CountOf $ wordToInt (W# (popCnt32# x#))
    countLeadingZeros (W# w#) = CountOf $ wordToInt (W# (clz32# w#))
    countTrailingZeros (W# w#) = CountOf $ wordToInt (W# (ctz32# w#))
#endif

instance BitOps Word where
    (.&.)    a b    = (a OldBits..&. b)
    (.|.)    a b    = (a OldBits..|. b)
    (.^.)    a b    = (a `OldBits.xor` b)
    (.<<.)   a (CountOf w) = (a `OldBits.shiftL` w)
    (.>>.)   a (CountOf w) = (a `OldBits.shiftR` w)

-- Word64 ---------------------------------------------------------------------

#if WORD_SIZE_IN_BITS == 64
instance FiniteBitsOps Word64 where
    numberOfBits _ = 64
    rotateL w (CountOf i) = w `OldBits.rotateL` i
    rotateR w (CountOf i) = w `OldBits.rotateR` i
    bitFlip = OldBits.complement
    popCount (W64# x#) = CountOf $ wordToInt (W# (popCnt64# x#))
    countLeadingZeros (W64# w#) = CountOf $ wordToInt (W# (clz64# w#))
    countTrailingZeros (W64# w#) = CountOf $ wordToInt (W# (ctz64# w#))
instance BitOps Word64 where
    (.&.)    a b    = (a OldBits..&. b)
    (.|.)    a b    = (a OldBits..|. b)
    (.^.)    a b    = (a `OldBits.xor` b)
    (.<<.)   a (CountOf w) = (a `OldBits.shiftL` w)
    (.>>.)   a (CountOf w) = (a `OldBits.shiftR` w)
#else
instance FiniteBitsOps Word64 where
    numberOfBits _ = 64
    rotateL w (CountOf i) = w `OldBits.rotateL` i
    rotateR w (CountOf i) = w `OldBits.rotateR` i
    bitFlip = OldBits.complement
    popCount (W64# x#) = CountOf $ wordToInt (W# (popCnt64# x#))
    countLeadingZeros (W64# w#) = CountOf $ wordToInt (W# (clz64# w#))
    countTrailingZeros (W64# w#) = CountOf $ wordToInt (W# (ctz64# w#))
instance BitOps Word64 where
    (.&.)    a b    = (a OldBits..&. b)
    (.|.)    a b    = (a OldBits..|. b)
    (.^.)    a b    = (a `OldBits.xor` b)
    (.<<.)   a (CountOf w) = (a `OldBits.shiftL` w)
    (.>>.)   a (CountOf w) = (a `OldBits.shiftR` w)
#endif

-- Word128 --------------------------------------------------------------------

instance FiniteBitsOps Word128 where
    numberOfBits _ = 128
    rotateL w (CountOf n) = Word128.rotateL w n
    rotateR w (CountOf n) = Word128.rotateR w n
    bitFlip = Word128.complement
    popCount = CountOf . Word128.popCount
instance BitOps Word128 where
    (.&.) = Word128.bitwiseAnd
    (.|.) = Word128.bitwiseOr
    (.^.) = Word128.bitwiseXor
    (.<<.) w (CountOf n) = Word128.shiftL w n
    (.>>.) w (CountOf n) = Word128.shiftR w n

-- Word256 --------------------------------------------------------------------

instance FiniteBitsOps Word256 where
    numberOfBits _ = 256
    rotateL w (CountOf n) = Word256.rotateL w n
    rotateR w (CountOf n) = Word256.rotateR w n
    bitFlip = Word256.complement
    popCount = CountOf . Word256.popCount
instance BitOps Word256 where
    (.&.) = Word256.bitwiseAnd
    (.|.) = Word256.bitwiseOr
    (.^.) = Word256.bitwiseXor
    (.<<.) w (CountOf n) = Word256.shiftL w n
    (.>>.) w (CountOf n) = Word256.shiftR w n

-- Int8 -----------------------------------------------------------------------
instance FiniteBitsOps Int8 where
    numberOfBits _ = 8
    rotateL w (CountOf i) = w `OldBits.rotateL` i
    rotateR w (CountOf i) = w `OldBits.rotateR` i
    bitFlip = OldBits.complement
    popCount (I8# x#) = CountOf $ wordToInt (W# (popCnt8# (int2Word# (int8ToInt# x#))))
    countLeadingZeros (I8# w#) = CountOf $ wordToInt (W# (clz8# (int2Word# (int8ToInt# w#))))
    countTrailingZeros (I8# w#) = CountOf $ wordToInt (W# (ctz8# (int2Word# (int8ToInt# w#))))
instance BitOps Int8 where
    (.&.)    a b    = (a OldBits..&. b)
    (.|.)    a b    = (a OldBits..|. b)
    (.^.)    a b    = (a `OldBits.xor` b)
    (.<<.)   a (CountOf w) = (a `OldBits.shiftL` w)
    (.>>.)   a (CountOf w) = (a `OldBits.shiftR` w)

-- Int16 ----------------------------------------------------------------------

instance FiniteBitsOps Int16 where
    numberOfBits _ = 16
    rotateL w (CountOf i) = w `OldBits.rotateL` i
    rotateR w (CountOf i) = w `OldBits.rotateR` i
    bitFlip = OldBits.complement
    popCount (I16# x#) = CountOf $ wordToInt (W# (popCnt16# (int2Word# (int16ToInt# x#))))
    countLeadingZeros (I16# w#) = CountOf $ wordToInt (W# (clz16# (int2Word# (int16ToInt# w#))))
    countTrailingZeros (I16# w#) = CountOf $ wordToInt (W# (ctz16# (int2Word# (int16ToInt# w#))))
instance BitOps Int16 where
    (.&.)    a b    = (a OldBits..&. b)
    (.|.)    a b    = (a OldBits..|. b)
    (.^.)    a b    = (a `OldBits.xor` b)
    (.<<.)   a (CountOf w) = (a `OldBits.shiftL` w)
    (.>>.)   a (CountOf w) = (a `OldBits.shiftR` w)

-- Int32 ----------------------------------------------------------------------

instance FiniteBitsOps Int32 where
    numberOfBits _ = 32
    rotateL w (CountOf i) = w `OldBits.rotateL` i
    rotateR w (CountOf i) = w `OldBits.rotateR` i
    bitFlip = OldBits.complement
    popCount (I32# x#) = CountOf $ wordToInt (W# (popCnt32# (int2Word# (int32ToInt# x#))))
    countLeadingZeros (I32# w#) = CountOf $ wordToInt (W# (clz32# (int2Word# (int32ToInt# w#))))
    countTrailingZeros (I32# w#) = CountOf $ wordToInt (W# (ctz32# (int2Word# (int32ToInt# w#))))
instance BitOps Int32 where
    (.&.)    a b    = (a OldBits..&. b)
    (.|.)    a b    = (a OldBits..|. b)
    (.^.)    a b    = (a `OldBits.xor` b)
    (.<<.)   a (CountOf w) = (a `OldBits.shiftL` w)
    (.>>.)   a (CountOf w) = (a `OldBits.shiftR` w)
-- Int64 ----------------------------------------------------------------------

#if WORD_SIZE_IN_BITS == 64
instance FiniteBitsOps Int64 where
    numberOfBits _ = 64
    rotateL w (CountOf i) = w `OldBits.rotateL` i
    rotateR w (CountOf i) = w `OldBits.rotateR` i
    bitFlip = OldBits.complement
#if __GLASGOW_HASKELL__ >= 904
    popCount (I64# x#) = CountOf $ wordToInt (W# (popCnt64# (wordToWord64# (int2Word# (int64ToInt# x#)))))
    countLeadingZeros (I64# w#) = CountOf $ wordToInt (W# (clz64# (wordToWord64# (int2Word# (int64ToInt# w#)))))
    countTrailingZeros (I64# w#) = CountOf $ wordToInt (W# (ctz64# (wordToWord64# (int2Word# (int64ToInt# w#)))))
#else
    popCount (I64# x#) = CountOf $ wordToInt (W# (popCnt64# (int2Word# x#)))
    countLeadingZeros (I64# w#) = CountOf $ wordToInt (W# (clz64# (int2Word# w#)))
    countTrailingZeros (I64# w#) = CountOf $ wordToInt (W# (ctz64# (int2Word# w#)))
#endif
instance BitOps Int64 where
    (.&.)    a b    = (a OldBits..&. b)
    (.|.)    a b    = (a OldBits..|. b)
    (.^.)    a b    = (a `OldBits.xor` b)
    (.<<.)   a (CountOf w) = (a `OldBits.shiftL` w)
    (.>>.)   a (CountOf w) = (a `OldBits.shiftR` w)
#else
instance FiniteBitsOps Int64 where
    numberOfBits _ = 64
    rotateL w (CountOf i) = w `OldBits.rotateL` i
    rotateR w (CountOf i) = w `OldBits.rotateR` i
    bitFlip = OldBits.complement
    popCount (I64# x#) = CountOf $ wordToInt (W# (popCnt64# (int64ToWord64# x#)))
    countLeadingZeros (I64# w#) = CountOf $ wordToInt (W# (clz64# (int64ToWord64# w#)))
    countTrailingZeros (I64# w#) = CountOf $ wordToInt (W# (ctz64# (int64ToWord64# w#)))
instance BitOps Int64 where
    (.&.)    a b    = (a OldBits..&. b)
    (.|.)    a b    = (a OldBits..|. b)
    (.^.)    a b    = (a `OldBits.xor` b)
    (.<<.)   a (CountOf w) = (a `OldBits.shiftL` w)
    (.>>.)   a (CountOf w) = (a `OldBits.shiftR` w)

#endif
