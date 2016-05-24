-- |
-- Module      : Core.Primitive.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Core.Primitive.Types
    ( PrimType(..)
    ) where

import           GHC.Prim
import           GHC.Int
import           GHC.Types
import           GHC.Word
import           Core.Internal.Proxy
import           Core.Internal.Primitive
import           Core.Internal.Base
import           Core.Primitive.Monad

-- | Represent the accessor for types that can be stored in the UVector and MUVector.
--
-- Types need to be a instance of storable and have fixed sized.
class PrimType ty where
    -- | get the size in bits of a ty element
    sizeInBits :: Proxy ty -> Int

    -----
    -- ByteArray section
    -----

    -- | return the element stored at a specific index
    primBaIndex :: ByteArray# -> Int -> ty

    -----
    -- MutableByteArray section
    -----

    -- | Read an element at an index in a mutable array
    primMbaRead :: PrimMonad prim
                => MutableByteArray# (PrimState prim) -- ^ mutable array to read from
                -> Int                               -- ^ index of the element to retrieve
                -> prim ty                           -- ^ the element returned

    -- | Write an element to a specific cell in a mutable array.
    primMbaWrite :: PrimMonad prim
                 => MutableByteArray# (PrimState prim) -- ^ mutable array to modify
                 -> Int                                -- ^ index of the element to modify
                 -> ty                                 -- ^ the new value to store
                 -> prim ()

    -----
    -- Addr# section
    -----

    -- | Read from Address, without a state. the value read should be considered a constant for all
    -- pratical purpose, otherwise bad thing will happens.
    primAddrIndex :: Addr# -> Int -> ty

    -- | Read a value from Addr in a specific primitive monad
    primAddrRead :: PrimMonad prim
                 => Addr#
                 -> Int -- TODO: byte or index ?
                 -> prim ty
    -- | Write a value to Addr in a specific primitive monad
    primAddrWrite :: PrimMonad prim
                  => Addr#
                  -> Int -- TODO: byte or index ?
                  -> ty
                  -> prim ()

-- return the index and mask to a bit in a bitmap
bitmapAddr :: Int# -> (# Int# , Word# #)
bitmapAddr !i = (# idx, mask #)
  where (# !idx, !bit #) = compatQuotRemInt# i 4#
        !mask = case bit of
                    0#  -> 0x1##
                    1#  -> 0x2##
                    2#  -> 0x4##
                    3#  -> 0x8##
                    4#  -> 0x10##
                    5#  -> 0x20##
                    6#  -> 0x40##
                    7#  -> 0x80##
                    8#  -> 0x100##
                    9#  -> 0x200##
                    10# -> 0x400##
                    11# -> 0x800##
                    12# -> 0x1000##
                    13# -> 0x2000##
                    14# -> 0x4000##
                    15# -> 0x8000##
                    16# -> 0x10000##
                    17# -> 0x20000##
                    18# -> 0x40000##
                    19# -> 0x80000##
                    20# -> 0x100000##
                    21# -> 0x200000##
                    22# -> 0x400000##
                    23# -> 0x800000##
                    24# -> 0x1000000##
                    25# -> 0x2000000##
                    26# -> 0x4000000##
                    27# -> 0x8000000##
                    28# -> 0x10000000##
                    29# -> 0x20000000##
                    30# -> 0x40000000##
                    _   -> 0x80000000##

instance PrimType Bool where
    sizeInBits _ = 1
    {-# INLINE sizeInBits #-}
    primBaIndex ba (I# n) =
         bool# (0# /=# word2Int# (and# v mask))
      where (# idx, mask #) = bitmapAddr n
            !v = indexWord32Array# ba idx
    {-# INLINE primBaIndex #-}
    primMbaRead mba (I# n) = primitive $ \s1 ->
        case readWord32Array# mba idx s1 of
            (# s2, v #) -> (# s2, bool# (word2Int# (and# v mask) ==# 0#) #)
      where (# !idx, !mask #) = bitmapAddr n
    {-# INLINE primMbaRead #-}
    primMbaWrite mba (I# n) setValue = primitive $ \s1 ->
        case readWord32Array# mba idx s1 of
            (# s2, v #) -> (# writeWord32Array# mba idx (newVal v) s2, () #)
      where (# !idx, !mask #) = bitmapAddr n
            newVal v
                | setValue  = or# v mask
                | otherwise = and# v (not# mask)
    {-# INLINE primMbaWrite #-}
    primAddrIndex addr (I# n) =
         bool# (0# /=# word2Int# (and# v mask))
      where (# idx, mask #) = bitmapAddr n
            !v = indexWord32OffAddr# addr idx
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (I# n) = primitive $ \s1 ->
        case readWord32OffAddr# addr idx s1 of
            (# s2, v #) -> (# s2, bool# (word2Int# (and# v mask) ==# 0#) #)
      where (# !idx, !mask #) = bitmapAddr n
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (I# n) setValue = primitive $ \s1 ->
        case readWord32OffAddr# addr idx s1 of
            (# s2, v #) -> (# writeWord32OffAddr# addr idx (newVal v) s2, () #)
      where (# !idx, !mask #) = bitmapAddr n
            newVal v
                | setValue  = or# v mask
                | otherwise = and# v (not# mask)
    {-# INLINE primAddrWrite #-}

instance PrimType Word8 where
    sizeInBits _ = 8
    {-# INLINE sizeInBits #-}
    primBaIndex ba (I# n) = W8# (indexWord8Array# ba n)
    {-# INLINE primBaIndex #-}
    primMbaRead mba (I# n) = primitive $ \s1 -> let (# s2, r #) = readWord8Array# mba n s1 in (# s2, W8# r #)
    {-# INLINE primMbaRead #-}
    primMbaWrite mba (I# n) (W8# w) = primitive $ \s1 -> (# writeWord8Array# mba n w s1, () #)
    {-# INLINE primMbaWrite #-}
    primAddrIndex addr (I# n) = W8# (indexWord8OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (I# n) = primitive $ \s1 -> let (# s2, r #) = readWord8OffAddr# addr n s1 in (# s2, W8# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (I# n) (W8# w) = primitive $ \s1 -> (# writeWord8OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}

instance PrimType Word16 where
    sizeInBits _ = 16
    {-# INLINE sizeInBits #-}
    primBaIndex ba (I# n) = W16# (indexWord16Array# ba n)
    {-# INLINE primBaIndex #-}
    primMbaRead mba (I# n) = primitive $ \s1 -> let (# s2, r #) = readWord16Array# mba n s1 in (# s2, W16# r #)
    {-# INLINE primMbaRead #-}
    primMbaWrite mba (I# n) (W16# w) = primitive $ \s1 -> (# writeWord16Array# mba n w s1, () #)
    {-# INLINE primMbaWrite #-}
    primAddrIndex addr (I# n) = W16# (indexWord16OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (I# n) = primitive $ \s1 -> let (# s2, r #) = readWord16OffAddr# addr n s1 in (# s2, W16# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (I# n) (W16# w) = primitive $ \s1 -> (# writeWord16OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Word32 where
    sizeInBits _ = 32
    {-# INLINE sizeInBits #-}
    primBaIndex ba (I# n) = W32# (indexWord32Array# ba n)
    {-# INLINE primBaIndex #-}
    primMbaRead mba (I# n) = primitive $ \s1 -> let (# s2, r #) = readWord32Array# mba n s1 in (# s2, W32# r #)
    {-# INLINE primMbaRead #-}
    primMbaWrite mba (I# n) (W32# w) = primitive $ \s1 -> (# writeWord32Array# mba n w s1, () #)
    {-# INLINE primMbaWrite #-}
    primAddrIndex addr (I# n) = W32# (indexWord32OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (I# n) = primitive $ \s1 -> let (# s2, r #) = readWord32OffAddr# addr n s1 in (# s2, W32# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (I# n) (W32# w) = primitive $ \s1 -> (# writeWord32OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Word64 where
    sizeInBits _ = 64
    {-# INLINE sizeInBits #-}
    primBaIndex ba (I# n) = W64# (indexWord64Array# ba n)
    {-# INLINE primBaIndex #-}
    primMbaRead mba (I# n) = primitive $ \s1 -> let (# s2, r #) = readWord64Array# mba n s1 in (# s2, W64# r #)
    {-# INLINE primMbaRead #-}
    primMbaWrite mba (I# n) (W64# w) = primitive $ \s1 -> (# writeWord64Array# mba n w s1, () #)
    {-# INLINE primMbaWrite #-}
    primAddrIndex addr (I# n) = W64# (indexWord64OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (I# n) = primitive $ \s1 -> let (# s2, r #) = readWord64OffAddr# addr n s1 in (# s2, W64# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (I# n) (W64# w) = primitive $ \s1 -> (# writeWord64OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Int8 where
    sizeInBits _ = 8
    {-# INLINE sizeInBits #-}
    primBaIndex ba (I# n) = I8# (indexInt8Array# ba n)
    {-# INLINE primBaIndex #-}
    primMbaRead mba (I# n) = primitive $ \s1 -> let (# s2, r #) = readInt8Array# mba n s1 in (# s2, I8# r #)
    {-# INLINE primMbaRead #-}
    primMbaWrite mba (I# n) (I8# w) = primitive $ \s1 -> (# writeInt8Array# mba n w s1, () #)
    {-# INLINE primMbaWrite #-}
    primAddrIndex addr (I# n) = I8# (indexInt8OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (I# n) = primitive $ \s1 -> let (# s2, r #) = readInt8OffAddr# addr n s1 in (# s2, I8# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (I# n) (I8# w) = primitive $ \s1 -> (# writeInt8OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Int16 where
    sizeInBits _ = 16
    {-# INLINE sizeInBits #-}
    primBaIndex ba (I# n) = I16# (indexInt16Array# ba n)
    {-# INLINE primBaIndex #-}
    primMbaRead mba (I# n) = primitive $ \s1 -> let (# s2, r #) = readInt16Array# mba n s1 in (# s2, I16# r #)
    {-# INLINE primMbaRead #-}
    primMbaWrite mba (I# n) (I16# w) = primitive $ \s1 -> (# writeInt16Array# mba n w s1, () #)
    {-# INLINE primMbaWrite #-}
    primAddrIndex addr (I# n) = I16# (indexInt16OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (I# n) = primitive $ \s1 -> let (# s2, r #) = readInt16OffAddr# addr n s1 in (# s2, I16# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (I# n) (I16# w) = primitive $ \s1 -> (# writeInt16OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Int32 where
    sizeInBits _ = 32
    {-# INLINE sizeInBits #-}
    primBaIndex ba (I# n) = I32# (indexInt32Array# ba n)
    {-# INLINE primBaIndex #-}
    primMbaRead mba (I# n) = primitive $ \s1 -> let (# s2, r #) = readInt32Array# mba n s1 in (# s2, I32# r #)
    {-# INLINE primMbaRead #-}
    primMbaWrite mba (I# n) (I32# w) = primitive $ \s1 -> (# writeInt32Array# mba n w s1, () #)
    {-# INLINE primMbaWrite #-}
    primAddrIndex addr (I# n) = I32# (indexInt32OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (I# n) = primitive $ \s1 -> let (# s2, r #) = readInt32OffAddr# addr n s1 in (# s2, I32# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (I# n) (I32# w) = primitive $ \s1 -> (# writeInt32OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Int64 where
    sizeInBits _ = 64
    {-# INLINE sizeInBits #-}
    primBaIndex ba (I# n) = I64# (indexInt64Array# ba n)
    {-# INLINE primBaIndex #-}
    primMbaRead mba (I# n) = primitive $ \s1 -> let (# s2, r #) = readInt64Array# mba n s1 in (# s2, I64# r #)
    {-# INLINE primMbaRead #-}
    primMbaWrite mba (I# n) (I64# w) = primitive $ \s1 -> (# writeInt64Array# mba n w s1, () #)
    {-# INLINE primMbaWrite #-}
    primAddrIndex addr (I# n) = I64# (indexInt64OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (I# n) = primitive $ \s1 -> let (# s2, r #) = readInt64OffAddr# addr n s1 in (# s2, I64# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (I# n) (I64# w) = primitive $ \s1 -> (# writeInt64OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}

instance PrimType Float where
    sizeInBits _ = 32
    {-# INLINE sizeInBits #-}
    primBaIndex ba (I# n) = F# (indexFloatArray# ba n)
    {-# INLINE primBaIndex #-}
    primMbaRead mba (I# n) = primitive $ \s1 -> let (# s2, r #) = readFloatArray# mba n s1 in (# s2, F# r #)
    {-# INLINE primMbaRead #-}
    primMbaWrite mba (I# n) (F# w) = primitive $ \s1 -> (# writeFloatArray# mba n w s1, () #)
    {-# INLINE primMbaWrite #-}
    primAddrIndex addr (I# n) = F# (indexFloatOffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (I# n) = primitive $ \s1 -> let (# s2, r #) = readFloatOffAddr# addr n s1 in (# s2, F# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (I# n) (F# w) = primitive $ \s1 -> (# writeFloatOffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Double where
    sizeInBits _ = 64
    {-# INLINE sizeInBits #-}
    primBaIndex ba (I# n) = D# (indexDoubleArray# ba n)
    {-# INLINE primBaIndex #-}
    primMbaRead mba (I# n) = primitive $ \s1 -> let (# s2, r #) = readDoubleArray# mba n s1 in (# s2, D# r #)
    {-# INLINE primMbaRead #-}
    primMbaWrite mba (I# n) (D# w) = primitive $ \s1 -> (# writeDoubleArray# mba n w s1, () #)
    {-# INLINE primMbaWrite #-}
    primAddrIndex addr (I# n) = D# (indexDoubleOffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (I# n) = primitive $ \s1 -> let (# s2, r #) = readDoubleOffAddr# addr n s1 in (# s2, D# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (I# n) (D# w) = primitive $ \s1 -> (# writeDoubleOffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
