{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE CPP                        #-}
module Foundation.Primitive.UTF8.BA
    ( nextAscii
    , nextAsciiDigit
    , expectAscii
    , write
    -- temporary
    , primRead
    ) where

import           GHC.Int
import           GHC.Types
import           GHC.Word
import           GHC.Prim
import           Foundation.Internal.Base
import           Foundation.Internal.Primitive
import           Foundation.Numerical
import           Foundation.Primitive.Types.OffsetSize
import           Foundation.Primitive.Monad
import           Foundation.Primitive.Types
import           Foundation.Bits

type Immutable = ByteArray#
type Mutable prim = MutableByteArray# (PrimState prim)

primWrite :: PrimMonad prim => Mutable prim -> Offset Word8 -> Word8 -> prim ()
primWrite = primMbaWrite

primRead :: PrimMonad prim => Mutable prim -> Offset Word8 -> prim Word8
primRead = primMbaRead

primIndex :: Immutable -> Offset Word8 -> Word8
primIndex = primBaIndex

nextAscii :: Immutable -> Offset Word8 -> (# Word8, Bool #)
nextAscii ba n = (# w, not (testBit w 7) #)
  where
    !w = primIndex ba n
{-# INLINE nextAscii #-}

-- | nextAsciiBa specialized to get a digit between 0 and 9 (included)
nextAsciiDigit :: Immutable -> Offset Word8 -> (# Word8, Bool #)
nextAsciiDigit ba n = (# d, d < 0xa #)
  where !d = primIndex ba n - 0x30
{-# INLINE nextAsciiDigit #-}

expectAscii :: Immutable -> Offset Word8 -> Word8 -> Bool
expectAscii ba n v = primIndex ba n == v
{-# INLINE expectAscii #-}

write :: PrimMonad prim => Mutable prim -> Offset8 -> Char -> prim Offset8
write mba i c
    | bool# (ltWord# x 0x80##   ) = encode1
    | bool# (ltWord# x 0x800##  ) = encode2
    | bool# (ltWord# x 0x10000##) = encode3
    | otherwise                   = encode4
  where
    !(I# xi) = fromEnum c
    !x       = int2Word# xi

    encode1 = primWrite mba i (W8# x) >> return (i + Offset 1)
    encode2 = do
        let x1  = or# (uncheckedShiftRL# x 6#) 0xc0##
            x2  = toContinuation x
        primWrite mba i     (W8# x1)
        primWrite mba (i+1) (W8# x2)
        return (i + Offset 2)

    encode3 = do
        let x1  = or# (uncheckedShiftRL# x 12#) 0xe0##
            x2  = toContinuation (uncheckedShiftRL# x 6#)
            x3  = toContinuation x
        primWrite mba i            (W8# x1)
        primWrite mba (i+Offset 1) (W8# x2)
        primWrite mba (i+Offset 2) (W8# x3)
        return (i + Offset 3)

    encode4 = do
        let x1  = or# (uncheckedShiftRL# x 18#) 0xf0##
            x2  = toContinuation (uncheckedShiftRL# x 12#)
            x3  = toContinuation (uncheckedShiftRL# x 6#)
            x4  = toContinuation x
        primWrite mba i            (W8# x1)
        primWrite mba (i+Offset 1) (W8# x2)
        primWrite mba (i+Offset 2) (W8# x3)
        primWrite mba (i+Offset 3) (W8# x4)
        return (i + Offset 4)

    toContinuation :: Word# -> Word#
    toContinuation w = or# (and# w 0x3f##) 0x80##
