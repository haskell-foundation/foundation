-- Module      : Foundation.Primitive.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Foundation.Primitive.Types
    ( PrimType(..)
    , primBaIndex
    , primMbaRead
    , primMbaWrite
    , primArrayIndex
    , primMutableArrayRead
    , primMutableArrayWrite
    , primOffsetOfE
    , primOffsetRecast
    , sizeRecast
    , offsetAsSize
    , sizeAsOffset
    , sizeInBytes
    , primWordGetByteAndShift
    , primWord64GetByteAndShift
    , primWord64GetHiLo
    ) where

#include "MachDeps.h"

import           GHC.Prim
import           GHC.Int
import           GHC.Types
import           GHC.Word
import           Foreign.C.Types
import           Foundation.Internal.Proxy
import           Foundation.Internal.Base
import           Foundation.Primitive.Types.OffsetSize
import           Foundation.Primitive.Endianness
import           Foundation.Primitive.Monad
import           Foundation.Numerical.Multiplicative
import qualified Prelude (quot)

#if WORD_SIZE_IN_BITS < 64
import           GHC.IntWord64
#endif

#ifdef FOUNDATION_BOUNDS_CHECK

divBytes :: PrimType ty => Offset ty -> (Int -> Int)
divBytes ofs = \x -> x `Prelude.quot` (getSize Proxy ofs)
  where
    getSize :: PrimType ty => Proxy ty -> Offset ty -> Int
    getSize p _ = let (Size sz) = primSizeInBytes p in sz

baLength :: PrimType ty => Offset ty -> ByteArray# -> Int
baLength ofs ba = divBytes ofs (I# (sizeofByteArray# ba))

mbaLength :: PrimType ty => Offset ty -> MutableByteArray# st -> Int
mbaLength ofs ba = divBytes ofs (I# (sizeofMutableByteArray# ba))

aLength :: Array# ty -> Int
aLength ba = I# (sizeofArray# ba)

maLength :: MutableArray# st ty -> Int
maLength ba = I# (sizeofMutableArray# ba)

boundCheckError :: [Char] -> Offset ty -> Int -> a
boundCheckError ty (Offset ofs) len =
    error (ty <> " offset=" <> show ofs <> " len=" <> show len)

baCheck :: PrimType ty => ByteArray# -> Offset ty -> Bool
baCheck ba ofs@(Offset o) = o < 0 || o >= baLength ofs ba

mbaCheck :: PrimType ty => MutableByteArray# st -> Offset ty -> Bool
mbaCheck mba ofs@(Offset o) = o < 0 || o >= mbaLength ofs mba

aCheck :: Array# ty -> Offset ty -> Bool
aCheck ba (Offset o) = o < 0 || o >= aLength ba

maCheck :: MutableArray# st ty -> Offset ty -> Bool
maCheck ma (Offset o) = o < 0 || o >= maLength ma

primBaIndex :: PrimType ty => ByteArray# -> Offset ty -> ty
primBaIndex ba ofs
    | baCheck ba ofs = boundCheckError "bytearray-index" ofs (baLength ofs ba)
    | otherwise      = primBaUIndex ba ofs
{-# NOINLINE primBaIndex #-}

primMbaRead :: (PrimType ty, PrimMonad prim) => MutableByteArray# (PrimState prim) -> Offset ty -> prim ty
primMbaRead mba ofs
    | mbaCheck mba ofs = boundCheckError "mutablebytearray-read" ofs (mbaLength ofs mba)
    | otherwise        = primMbaURead mba ofs
{-# NOINLINE primMbaRead #-}

primMbaWrite :: (PrimType ty, PrimMonad prim) => MutableByteArray# (PrimState prim) -> Offset ty -> ty -> prim ()
primMbaWrite mba ofs ty
    | mbaCheck mba ofs = boundCheckError "mutablebytearray-write" ofs (mbaLength ofs mba)
    | otherwise        = primMbaUWrite mba ofs ty
{-# NOINLINE primMbaWrite #-}

primArrayIndex :: Array# ty -> Offset ty -> ty
primArrayIndex a o@(Offset (I# ofs))
    | aCheck a o = boundCheckError "array-index" o (aLength a)
    | otherwise  = let (# v #) = indexArray# a ofs in v
{-# NOINLINE primArrayIndex #-}

primMutableArrayRead :: PrimMonad prim => MutableArray# (PrimState prim) ty -> Offset ty -> prim ty
primMutableArrayRead ma o@(Offset (I# ofs))
    | maCheck ma o = boundCheckError "array-read" o (maLength ma)
    | otherwise    = primitive $ \s1 -> readArray# ma ofs s1
{-# NOINLINE primMutableArrayRead #-}

primMutableArrayWrite :: PrimMonad prim => MutableArray# (PrimState prim) ty -> Offset ty -> ty -> prim ()
primMutableArrayWrite ma o@(Offset (I# ofs)) v
    | maCheck ma o = boundCheckError "array-write" o (maLength ma)
    | otherwise    = primitive $ \s1 -> let !s2 = writeArray# ma ofs v s1 in (# s2, () #)
{-# NOINLINE primMutableArrayWrite #-}

#else

primBaIndex :: PrimType ty => ByteArray# -> Offset ty -> ty
primBaIndex = primBaUIndex
{-# INLINE primBaIndex #-}

primMbaRead :: (PrimType ty, PrimMonad prim) => MutableByteArray# (PrimState prim) -> Offset ty -> prim ty
primMbaRead = primMbaURead
{-# INLINE primMbaRead #-}

primMbaWrite :: (PrimType ty, PrimMonad prim) => MutableByteArray# (PrimState prim) -> Offset ty -> ty -> prim ()
primMbaWrite = primMbaUWrite
{-# INLINE primMbaWrite #-}

primArrayIndex :: Array# ty -> Offset ty -> ty
primArrayIndex a (Offset (I# ofs)) = let (# v #) = indexArray# a ofs in v
{-# INLINE primArrayIndex #-}

primMutableArrayRead :: PrimMonad prim => MutableArray# (PrimState prim) ty -> Offset ty -> prim ty
primMutableArrayRead ma (Offset (I# ofs)) = primitive $ \s1 -> readArray# ma ofs s1
{-# INLINE primMutableArrayRead #-}

primMutableArrayWrite :: PrimMonad prim => MutableArray# (PrimState prim) ty -> Offset ty -> ty -> prim ()
primMutableArrayWrite ma (Offset (I# ofs)) v =
    primitive $ \s1 -> let !s2 = writeArray# ma ofs v s1 in (# s2, () #)
{-# INLINE primMutableArrayWrite #-}

#endif

-- | Represent the accessor for types that can be stored in the UArray and MUArray.
--
-- Types need to be a instance of storable and have fixed sized.
class Eq ty => PrimType ty where
    -- | get the size in bytes of a ty element
    primSizeInBytes :: Proxy ty -> Size8

    -----
    -- ByteArray section
    -----

    -- | return the element stored at a specific index
    primBaUIndex :: ByteArray# -> Offset ty -> ty

    -----
    -- MutableByteArray section
    -----

    -- | Read an element at an index in a mutable array
    primMbaURead :: PrimMonad prim
                => MutableByteArray# (PrimState prim) -- ^ mutable array to read from
                -> Offset ty                         -- ^ index of the element to retrieve
                -> prim ty                           -- ^ the element returned

    -- | Write an element to a specific cell in a mutable array.
    primMbaUWrite :: PrimMonad prim
                 => MutableByteArray# (PrimState prim) -- ^ mutable array to modify
                 -> Offset ty                         -- ^ index of the element to modify
                 -> ty                                 -- ^ the new value to store
                 -> prim ()

    -----
    -- Addr# section
    -----

    -- | Read from Address, without a state. the value read should be considered a constant for all
    -- pratical purpose, otherwise bad thing will happens.
    primAddrIndex :: Addr# -> Offset ty -> ty

    -- | Read a value from Addr in a specific primitive monad
    primAddrRead :: PrimMonad prim
                 => Addr#
                 -> Offset ty
                 -> prim ty
    -- | Write a value to Addr in a specific primitive monad
    primAddrWrite :: PrimMonad prim
                  => Addr#
                  -> Offset ty
                  -> ty
                  -> prim ()

sizeInt, sizeWord :: Size Word8
#if WORD_SIZE_IN_BITS == 64
sizeInt = Size 8
sizeWord = Size 8
#else
sizeInt = Size 4
sizeWord = Size 4
#endif

{-# SPECIALIZE [3] primBaUIndex :: ByteArray# -> Offset Word8 -> Word8 #-}

instance PrimType Int where
    primSizeInBytes _ = sizeInt
    {-# INLINE primSizeInBytes #-}
    primBaUIndex ba (Offset (I# n)) = I# (indexIntArray# ba n)
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readIntArray# mba n s1 in (# s2, I# r #)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset (I# n)) (I# w) = primitive $ \s1 -> (# writeIntArray# mba n w s1, () #)
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset (I# n)) = I# (indexIntOffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readIntOffAddr# addr n s1 in (# s2, I# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (I# w) = primitive $ \s1 -> (# writeIntOffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}

instance PrimType Word where
    primSizeInBytes _ = sizeWord
    {-# INLINE primSizeInBytes #-}
    primBaUIndex ba (Offset (I# n)) = W# (indexWordArray# ba n)
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readWordArray# mba n s1 in (# s2, W# r #)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset (I# n)) (W# w) = primitive $ \s1 -> (# writeWordArray# mba n w s1, () #)
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset (I# n)) = W# (indexWordOffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readWordOffAddr# addr n s1 in (# s2, W# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (W# w) = primitive $ \s1 -> (# writeWordOffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}

instance PrimType Word8 where
    primSizeInBytes _ = Size 1
    {-# INLINE primSizeInBytes #-}
    primBaUIndex ba (Offset (I# n)) = W8# (indexWord8Array# ba n)
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readWord8Array# mba n s1 in (# s2, W8# r #)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset (I# n)) (W8# w) = primitive $ \s1 -> (# writeWord8Array# mba n w s1, () #)
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset (I# n)) = W8# (indexWord8OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readWord8OffAddr# addr n s1 in (# s2, W8# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (W8# w) = primitive $ \s1 -> (# writeWord8OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}

instance PrimType Word16 where
    primSizeInBytes _ = Size 2
    {-# INLINE primSizeInBytes #-}
    primBaUIndex ba (Offset (I# n)) = W16# (indexWord16Array# ba n)
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readWord16Array# mba n s1 in (# s2, W16# r #)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset (I# n)) (W16# w) = primitive $ \s1 -> (# writeWord16Array# mba n w s1, () #)
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset (I# n)) = W16# (indexWord16OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readWord16OffAddr# addr n s1 in (# s2, W16# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (W16# w) = primitive $ \s1 -> (# writeWord16OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Word32 where
    primSizeInBytes _ = Size 4
    {-# INLINE primSizeInBytes #-}
    primBaUIndex ba (Offset (I# n)) = W32# (indexWord32Array# ba n)
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readWord32Array# mba n s1 in (# s2, W32# r #)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset (I# n)) (W32# w) = primitive $ \s1 -> (# writeWord32Array# mba n w s1, () #)
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset (I# n)) = W32# (indexWord32OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readWord32OffAddr# addr n s1 in (# s2, W32# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (W32# w) = primitive $ \s1 -> (# writeWord32OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Word64 where
    primSizeInBytes _ = Size 8
    {-# INLINE primSizeInBytes #-}
    primBaUIndex ba (Offset (I# n)) = W64# (indexWord64Array# ba n)
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readWord64Array# mba n s1 in (# s2, W64# r #)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset (I# n)) (W64# w) = primitive $ \s1 -> (# writeWord64Array# mba n w s1, () #)
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset (I# n)) = W64# (indexWord64OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readWord64OffAddr# addr n s1 in (# s2, W64# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (W64# w) = primitive $ \s1 -> (# writeWord64OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Int8 where
    primSizeInBytes _ = Size 1
    {-# INLINE primSizeInBytes #-}
    primBaUIndex ba (Offset (I# n)) = I8# (indexInt8Array# ba n)
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readInt8Array# mba n s1 in (# s2, I8# r #)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset (I# n)) (I8# w) = primitive $ \s1 -> (# writeInt8Array# mba n w s1, () #)
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset (I# n)) = I8# (indexInt8OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readInt8OffAddr# addr n s1 in (# s2, I8# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (I8# w) = primitive $ \s1 -> (# writeInt8OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Int16 where
    primSizeInBytes _ = Size 2
    {-# INLINE primSizeInBytes #-}
    primBaUIndex ba (Offset (I# n)) = I16# (indexInt16Array# ba n)
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readInt16Array# mba n s1 in (# s2, I16# r #)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset (I# n)) (I16# w) = primitive $ \s1 -> (# writeInt16Array# mba n w s1, () #)
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset (I# n)) = I16# (indexInt16OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readInt16OffAddr# addr n s1 in (# s2, I16# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (I16# w) = primitive $ \s1 -> (# writeInt16OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Int32 where
    primSizeInBytes _ = Size 4
    {-# INLINE primSizeInBytes #-}
    primBaUIndex ba (Offset (I# n)) = I32# (indexInt32Array# ba n)
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readInt32Array# mba n s1 in (# s2, I32# r #)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset (I# n)) (I32# w) = primitive $ \s1 -> (# writeInt32Array# mba n w s1, () #)
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset (I# n)) = I32# (indexInt32OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readInt32OffAddr# addr n s1 in (# s2, I32# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (I32# w) = primitive $ \s1 -> (# writeInt32OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Int64 where
    primSizeInBytes _ = Size 8
    {-# INLINE primSizeInBytes #-}
    primBaUIndex ba (Offset (I# n)) = I64# (indexInt64Array# ba n)
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readInt64Array# mba n s1 in (# s2, I64# r #)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset (I# n)) (I64# w) = primitive $ \s1 -> (# writeInt64Array# mba n w s1, () #)
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset (I# n)) = I64# (indexInt64OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readInt64OffAddr# addr n s1 in (# s2, I64# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (I64# w) = primitive $ \s1 -> (# writeInt64OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}

instance PrimType Float where
    primSizeInBytes _ = Size 4
    {-# INLINE primSizeInBytes #-}
    primBaUIndex ba (Offset (I# n)) = F# (indexFloatArray# ba n)
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readFloatArray# mba n s1 in (# s2, F# r #)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset (I# n)) (F# w) = primitive $ \s1 -> (# writeFloatArray# mba n w s1, () #)
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset (I# n)) = F# (indexFloatOffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readFloatOffAddr# addr n s1 in (# s2, F# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (F# w) = primitive $ \s1 -> (# writeFloatOffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Double where
    primSizeInBytes _ = Size 8
    {-# INLINE primSizeInBytes #-}
    primBaUIndex ba (Offset (I# n)) = D# (indexDoubleArray# ba n)
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readDoubleArray# mba n s1 in (# s2, D# r #)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset (I# n)) (D# w) = primitive $ \s1 -> (# writeDoubleArray# mba n w s1, () #)
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset (I# n)) = D# (indexDoubleOffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readDoubleOffAddr# addr n s1 in (# s2, D# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (D# w) = primitive $ \s1 -> (# writeDoubleOffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}

instance PrimType Char where
    primSizeInBytes _ = Size 4
    {-# INLINE primSizeInBytes #-}
    primBaUIndex ba (Offset (I# n)) = C# (indexWideCharArray# ba n)
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readWideCharArray# mba n s1 in (# s2, C# r #)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset (I# n)) (C# w) = primitive $ \s1 -> (# writeWideCharArray# mba n w s1, () #)
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset (I# n)) = C# (indexWideCharOffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readWideCharOffAddr# addr n s1 in (# s2, C# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (C# w) = primitive $ \s1 -> (# writeWideCharOffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}

instance PrimType CChar where
    primSizeInBytes _ = Size 1
    {-# INLINE primSizeInBytes #-}
    primBaUIndex ba (Offset n) = CChar (primBaUIndex ba (Offset n))
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset n) = CChar <$> primMbaURead mba (Offset n)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset n) (CChar int8) = primMbaUWrite mba (Offset n) int8
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset n) = CChar $ primAddrIndex addr (Offset n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset n) = CChar <$> primAddrRead addr (Offset n)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset n) (CChar int8) = primAddrWrite addr (Offset n) int8
    {-# INLINE primAddrWrite #-}
instance PrimType CUChar where
    primSizeInBytes _ = Size 1
    {-# INLINE primSizeInBytes #-}
    primBaUIndex ba (Offset n) = CUChar (primBaUIndex ba (Offset n :: Offset Word8))
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset n) = CUChar <$> primMbaURead mba (Offset n :: Offset Word8)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset n) (CUChar w8) = primMbaUWrite mba (Offset n) w8
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset n) = CUChar $ primAddrIndex addr (Offset n :: Offset Word8)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset n) = CUChar <$> primAddrRead addr (Offset n :: Offset Word8)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset n) (CUChar w8) = primAddrWrite addr (Offset n) w8
    {-# INLINE primAddrWrite #-}

instance PrimType a => PrimType (LE a) where
    primSizeInBytes _ = primSizeInBytes (Proxy :: Proxy a)
    {-# INLINE primSizeInBytes #-}
    primBaUIndex ba (Offset a) = LE $ primBaUIndex ba (Offset a)
    {-# INLINE primBaUIndex #-}
    primMbaURead ba (Offset a) = LE <$> primMbaURead ba (Offset a)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset a) (LE w) = primMbaUWrite mba (Offset a) w
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset a) = LE $ primAddrIndex addr (Offset a)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset a) = LE <$> primAddrRead addr (Offset a)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset a) (LE w) = primAddrWrite addr (Offset a) w
    {-# INLINE primAddrWrite #-}
instance PrimType a => PrimType (BE a) where
    primSizeInBytes _ = primSizeInBytes (Proxy :: Proxy a)
    {-# INLINE primSizeInBytes #-}
    primBaUIndex ba (Offset a) = BE $ primBaUIndex ba (Offset a)
    {-# INLINE primBaUIndex #-}
    primMbaURead ba (Offset a) = BE <$> primMbaURead ba (Offset a)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset a) (BE w) = primMbaUWrite mba (Offset a) w
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset a) = BE $ primAddrIndex addr (Offset a)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset a) = BE <$> primAddrRead addr (Offset a)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset a) (BE w) = primAddrWrite addr (Offset a) w
    {-# INLINE primAddrWrite #-}


-- | Cast a Size linked to type A (Size A) to a Size linked to type B (Size B)
sizeRecast :: forall a b . (PrimType a, PrimType b) => Size a -> Size b
sizeRecast sz = Size (bytes `Prelude.quot` szB)
  where !szA          = primSizeInBytes (Proxy :: Proxy a)
        !(Size szB)   = primSizeInBytes (Proxy :: Proxy b)
        !(Size bytes) = sizeOfE szA sz
{-# INLINE [1] sizeRecast #-}
{-# RULES "sizeRecast from Word8" [2] forall a . sizeRecast a = sizeRecastBytes a #-}

sizeRecastBytes :: forall b . PrimType b => Size Word8 -> Size b
sizeRecastBytes (Size w) = Size (w `Prelude.quot` szB)
  where !(Size szB) = primSizeInBytes (Proxy :: Proxy b)
{-# INLINE [1] sizeRecastBytes #-}

sizeInBytes :: forall a . PrimType a => Size a -> Size Word8
sizeInBytes sz = sizeOfE (primSizeInBytes (Proxy :: Proxy a)) sz

primOffsetRecast :: forall a b . (PrimType a, PrimType b) => Offset a -> Offset b
primOffsetRecast !ofs =
    let !(Offset bytes) = offsetOfE szA ofs
     in Offset (bytes `Prelude.quot` szB)
  where
    !szA        = primSizeInBytes (Proxy :: Proxy a)
    !(Size szB) = primSizeInBytes (Proxy :: Proxy b)
{-# INLINE [1] primOffsetRecast #-}
{-# RULES "primOffsetRecast W8" [3] forall a . primOffsetRecast a = primOffsetRecastBytes a #-}

primOffsetRecastBytes :: forall b . PrimType b => Offset Word8 -> Offset b
primOffsetRecastBytes !(Offset o) = Offset (szA `Prelude.quot` o)
  where !(Size szA) = primSizeInBytes (Proxy :: Proxy b)
{-# INLINE [1] primOffsetRecastBytes #-}

primOffsetOfE :: PrimType a => Offset a -> Offset8
primOffsetOfE = getOffset Proxy
  where getOffset :: PrimType a => Proxy a -> Offset a -> Offset8
        getOffset proxy = offsetOfE (primSizeInBytes proxy)

primWordGetByteAndShift :: Word# -> (# Word#, Word# #)
primWordGetByteAndShift w = (# and# w 0xff##, uncheckedShiftRL# w 8# #)
{-# INLINE primWordGetByteAndShift #-}

#if WORD_SIZE_IN_BITS == 64
primWord64GetByteAndShift :: Word# -> (# Word#, Word# #)
primWord64GetByteAndShift = primWord64GetByteAndShift

primWord64GetHiLo :: Word# -> (# Word#, Word# #)
primWord64GetHiLo w = (# uncheckedShiftRL# w 32# , and# w 0xffffffff## #)
#else
primWord64GetByteAndShift :: Word64# -> (# Word#, Word64# #)
primWord64GetByteAndShift w = (# and# (word64ToWord# w) 0xff##, uncheckedShiftRL64# w 8# #)

primWord64GetHiLo :: Word64# -> (# Word#, Word# #)
primWord64GetHiLo w = (# word64ToWord# (uncheckedShiftRL64# w 32#), word64ToWord# w #)
#endif
{-# INLINE primWord64GetByteAndShift #-}
