{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE CPP                        #-}
module Foundation.Primitive.UTF8.Addr
    ( Immutable
    , Mutable
    -- * functions
    ,  nextAscii
    , nextAsciiDigit
    , expectAscii
    , next
    , prev
    , prevSkip
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
import           Foundation.Primitive.UTF8.Helper
import           Foundation.Primitive.UTF8.Table
import           Foundation.Bits

type Immutable = Addr#
type Mutable (prim :: * -> *) = Addr#

primWrite :: PrimMonad prim => Mutable prim -> Offset Word8 -> Word8 -> prim ()
primWrite = primAddrWrite

primRead :: PrimMonad prim => Mutable prim -> Offset Word8 -> prim Word8
primRead = primAddrRead

primIndex :: Immutable -> Offset Word8 -> Word8
primIndex = primAddrIndex


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

next :: Immutable -> Offset8 -> (# Char, Offset8 #)
next ba n =
    case getNbBytes# h of
        0# -> (# toChar# h, n + 1 #)
        1# -> (# toChar# (decode2 (primIndex ba (n + Offset 1))) , n + Offset 2 #)
        2# -> (# toChar# (decode3 (primIndex ba (n + Offset 1))
                                  (primIndex ba (n + Offset 2))) , n + Offset 3 #)
        3# -> (# toChar# (decode4 (primIndex ba (n + Offset 1))
                                  (primIndex ba (n + Offset 2))
                                  (primIndex ba (n + Offset 3))) , n + Offset 4 #)
        r -> error ("next: internal error: invalid input: offset=" <> show n <> " table=" <> show (I# r) <> " h=" <> show (W# h))
  where
    !(W8# h) = primIndex ba n

    decode2 :: Word8 -> Word#
    decode2 (W8# c1) =
        or# (uncheckedShiftL# (maskHeader2# h) 6#)
            (maskContinuation# c1)

    decode3 :: Word8 -> Word8 -> Word#
    decode3 (W8# c1) (W8# c2) =
        or3# (uncheckedShiftL# (maskHeader3# h) 12#)
             (uncheckedShiftL# (maskContinuation# c1) 6#)
             (maskContinuation# c2)

    decode4 :: Word8 -> Word8 -> Word8 -> Word#
    decode4 (W8# c1) (W8# c2) (W8# c3) =
        or4# (uncheckedShiftL# (maskHeader4# h) 18#)
             (uncheckedShiftL# (maskContinuation# c1) 12#)
             (uncheckedShiftL# (maskContinuation# c2) 6#)
             (maskContinuation# c3)

-- Given a non null offset, give the previous character and the offset of this character
-- will fail bad if apply at the beginning of string or an empty string.
prev :: Immutable -> Offset Word8 -> (# Char, Offset8 #)
prev ba offset =
    case primIndex ba prevOfs1 of
        (W8# v1) | isContinuation# v1 -> atLeast2 (maskContinuation# v1)
                 | otherwise          -> (# toChar# v1, prevOfs1 #)
  where
    sz1 = CountOf 1
    !prevOfs1 = offset `offsetMinusE` sz1
    prevOfs2 = prevOfs1 `offsetMinusE` sz1
    prevOfs3 = prevOfs2 `offsetMinusE` sz1
    prevOfs4 = prevOfs3 `offsetMinusE` sz1
    atLeast2 !v  =
        case primIndex ba prevOfs2 of
            (W8# v2) | isContinuation# v2 -> atLeast3 (or# (uncheckedShiftL# (maskContinuation# v2) 6#) v)
                     | otherwise          -> (# toChar# (or# (uncheckedShiftL# (maskHeader2# v2) 6#) v), prevOfs2 #)
    atLeast3 !v =
        case primIndex ba prevOfs3 of
            (W8# v3) | isContinuation# v3 -> atLeast4 (or# (uncheckedShiftL# (maskContinuation# v3) 12#) v)
                     | otherwise          -> (# toChar# (or# (uncheckedShiftL# (maskHeader3# v3) 12#) v), prevOfs2 #)
    atLeast4 !v =
        case primIndex ba prevOfs4 of
            (W8# v4) -> (# toChar# (or# (uncheckedShiftL# (maskHeader4# v4) 18#) v), prevOfs4 #)

prevSkip :: Immutable -> Offset Word8 -> Offset Word8
prevSkip ba offset = loop (offset `offsetMinusE` sz1)
  where
    sz1 = CountOf 1
    loop o
        | isContinuation (primIndex ba o) = loop (o `offsetMinusE` sz1)
        | otherwise                       = o

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
{-# INLINE write #-}
