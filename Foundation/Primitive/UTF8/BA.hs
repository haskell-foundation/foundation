{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE CPP                        #-}
module Foundation.Primitive.UTF8.BA
    ( Immutable
    , Mutable
    -- * functions
    , nextAscii
    , nextAsciiDigit
    , expectAscii
    , next
    , prev
    , prevSkip
    , write
    , toList
    , all
    , any
    , foldr
    , length
    -- temporary
    , primIndex
    , primIndex64
    , primRead
    , primWrite
    ) where

import           GHC.Int
import           GHC.Types
import           GHC.Word
import           GHC.Prim
import           Data.Bits
import           Foundation.Internal.Base hiding (toList)
import           Foundation.Internal.Primitive
import           Data.Proxy
import           Foundation.Numerical.Additive
import           Foundation.Numerical.Subtractive
import           Foundation.Primitive.Types.OffsetSize
import           Foundation.Primitive.Monad
import           Foundation.Primitive.Types
import           Foundation.Primitive.UTF8.Helper
import           Foundation.Primitive.UTF8.Table
import           Foundation.Primitive.UTF8.Types

type Immutable = ByteArray#
type Mutable prim = MutableByteArray# (PrimState prim)

primWrite :: PrimMonad prim => Mutable prim -> Offset Word8 -> Word8 -> prim ()
primWrite = primMbaWrite

primRead :: PrimMonad prim => Mutable prim -> Offset Word8 -> prim Word8
primRead = primMbaRead

primIndex :: Immutable -> Offset Word8 -> Word8
primIndex = primBaIndex

primIndex64 :: Immutable -> Offset Word64 -> Word64
primIndex64 = primBaIndex

nextAscii :: Immutable -> Offset Word8 -> StepASCII
nextAscii ba n = StepASCII w
  where
    !w = primIndex ba n
{-# INLINE nextAscii #-}

-- | nextAsciiBa specialized to get a digit between 0 and 9 (included)
nextAsciiDigit :: Immutable -> Offset Word8 -> StepDigit
nextAsciiDigit ba n = StepDigit (primIndex ba n - 0x30)
{-# INLINE nextAsciiDigit #-}

expectAscii :: Immutable -> Offset Word8 -> Word8 -> Bool
expectAscii ba n v = primIndex ba n == v
{-# INLINE expectAscii #-}

next :: Immutable -> Offset8 -> Step
next ba n =
    case getNbBytes h of
        0 -> Step (toChar1 h) (n + Offset 1)
        1 -> Step (toChar2 h (primIndex ba (n + Offset 1))) (n + Offset 2)
        2 -> Step (toChar3 h (primIndex ba (n + Offset 1))
                             (primIndex ba (n + Offset 2))) (n + Offset 3)
        3 -> Step (toChar4 h (primIndex ba (n + Offset 1))
                             (primIndex ba (n + Offset 2))
                             (primIndex ba (n + Offset 3))) (n + Offset 4)
        r -> error ("next: internal error: invalid input: offset=" <> show n <> " table=" <> show r <> " h=" <> show h)
  where
    !h = primIndex ba n
{-# INLINE next #-}

-- Given a non null offset, give the previous character and the offset of this character
-- will fail bad if apply at the beginning of string or an empty string.
prev :: Immutable -> Offset Word8 -> StepBack
prev ba offset =
    case primIndex ba prevOfs1 of
        (W8# v1) | isContinuation# v1 -> atLeast2 (maskContinuation# v1)
                 | otherwise          -> StepBack (toChar# v1) prevOfs1
  where
    sz1 = CountOf 1
    !prevOfs1 = offset `offsetMinusE` sz1
    prevOfs2 = prevOfs1 `offsetMinusE` sz1
    prevOfs3 = prevOfs2 `offsetMinusE` sz1
    prevOfs4 = prevOfs3 `offsetMinusE` sz1
    atLeast2 !v  =
        case primIndex ba prevOfs2 of
            (W8# v2) | isContinuation# v2 -> atLeast3 (or# (uncheckedShiftL# (maskContinuation# v2) 6#) v)
                     | otherwise          -> StepBack (toChar# (or# (uncheckedShiftL# (maskHeader2# v2) 6#) v)) prevOfs2
    atLeast3 !v =
        case primIndex ba prevOfs3 of
            (W8# v3) | isContinuation# v3 -> atLeast4 (or# (uncheckedShiftL# (maskContinuation# v3) 12#) v)
                     | otherwise          -> StepBack (toChar# (or# (uncheckedShiftL# (maskHeader3# v3) 12#) v)) prevOfs3
    atLeast4 !v =
        case primIndex ba prevOfs4 of
            (W8# v4) -> StepBack (toChar# (or# (uncheckedShiftL# (maskHeader4# v4) 18#) v)) prevOfs4

prevSkip :: Immutable -> Offset Word8 -> Offset Word8
prevSkip ba offset = loop (offset `offsetMinusE` sz1)
  where
    sz1 = CountOf 1
    loop o
        | isContinuation (primIndex ba o) = loop (o `offsetMinusE` sz1)
        | otherwise                       = o

write :: PrimMonad prim => Mutable prim -> Offset8 -> Char -> prim Offset8
write mba !i !c
    | bool# (ltWord# x 0x80##   ) = encode1
    | bool# (ltWord# x 0x800##  ) = encode2
    | bool# (ltWord# x 0x10000##) = encode3
    | otherwise                   = encode4
  where
    !(I# xi) = fromEnum c
    !x       = int2Word# xi

    encode1 = primWrite mba i (W8# x) >> pure (i + Offset 1)
    encode2 = do
        let x1  = or# (uncheckedShiftRL# x 6#) 0xc0##
            x2  = toContinuation x
        primWrite mba i     (W8# x1)
        primWrite mba (i+1) (W8# x2)
        pure (i + Offset 2)

    encode3 = do
        let x1  = or# (uncheckedShiftRL# x 12#) 0xe0##
            x2  = toContinuation (uncheckedShiftRL# x 6#)
            x3  = toContinuation x
        primWrite mba i            (W8# x1)
        primWrite mba (i+Offset 1) (W8# x2)
        primWrite mba (i+Offset 2) (W8# x3)
        pure (i + Offset 3)

    encode4 = do
        let x1  = or# (uncheckedShiftRL# x 18#) 0xf0##
            x2  = toContinuation (uncheckedShiftRL# x 12#)
            x3  = toContinuation (uncheckedShiftRL# x 6#)
            x4  = toContinuation x
        primWrite mba i            (W8# x1)
        primWrite mba (i+Offset 1) (W8# x2)
        primWrite mba (i+Offset 2) (W8# x3)
        primWrite mba (i+Offset 3) (W8# x4)
        pure (i + Offset 4)

    toContinuation :: Word# -> Word#
    toContinuation w = or# (and# w 0x3f##) 0x80##
{-# INLINE write #-}

toList :: Immutable -> Offset Word8 -> Offset Word8 -> [Char]
toList ba !start !end = loop start
  where
    loop !idx
        | idx == end = []
        | otherwise  = c : loop idx'
      where (Step c idx') = next ba idx

all :: (Char -> Bool) -> Immutable -> Offset Word8 -> Offset Word8 -> Bool
all predicate ba start end = loop start
  where
    loop !idx
        | idx == end  = True
        | predicate c = loop idx'
        | otherwise   = False
      where (Step c idx') = next ba idx
{-# INLINE all #-}

any :: (Char -> Bool) -> Immutable -> Offset Word8 -> Offset Word8 -> Bool
any predicate ba start end = loop start
  where
    loop !idx
        | idx == end  = False
        | predicate c = True
        | otherwise   = loop idx'
      where (Step c idx') = next ba idx
{-# INLINE any #-}

foldr :: Immutable -> Offset Word8 -> Offset Word8 -> (Char -> a -> a) -> a -> a
foldr dat start end f acc = loop start
  where
    loop !i
        | i == end  = acc
        | otherwise =
            let (Step c i') = next dat i
             in c `f` loop i'
{-# INLINE foldr #-}

length :: Immutable -> Offset Word8 -> Offset Word8 -> CountOf Char
length dat start end
    | start == end = 0
    | otherwise    = processStart 0 start
  where
    end64 :: Offset Word64
    end64 = offsetInElements end

    prx64 :: Proxy Word64
    prx64 = Proxy

    mask64_80 :: Word64
    mask64_80 = 0x8080808080808080

    processStart :: CountOf Char -> Offset Word8 -> CountOf Char
    processStart !c !i
        | i == end                = c
        | offsetIsAligned prx64 i = processAligned c (offsetInElements i)
        | otherwise               =
            let h    = primIndex dat i
                cont = (h .&. 0xc0) == 0x80
                c'   = if cont then c else c+1
             in processStart c' (i+1)
    processAligned :: CountOf Char -> Offset Word64 -> CountOf Char
    processAligned !c !i
        | i >= end64 = processEnd c (offsetInBytes i)
        | otherwise  =
            let !h   = primIndex64 dat i
                !h80 = h .&. mask64_80
             in if h80 == 0
                 then processAligned (c+8) (i+1)
                 else let !nbAscii = if h80 == mask64_80 then 0 else CountOf (8 - popCount h80)
                          !nbHigh  = CountOf $ popCount (h .&. (h80 `unsafeShiftR` 1))
                       in processAligned (c + nbAscii + nbHigh) (i+1)
    processEnd !c !i
        | i == end  = c
        | otherwise =
            let h    = primIndex dat i
                cont = (h .&. 0xc0) == 0x80
                c'   = if cont then c else c+1
             in processStart c' (i+1)
{-# INLINE length #-}
