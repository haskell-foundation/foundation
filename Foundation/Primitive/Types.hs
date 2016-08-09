-- |
-- Module      : Foundation.Primitive.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Foundation.Primitive.Types
    ( PrimType(..)
    , primOffsetOfE
    , primOffsetRecast
    , sizeRecast
    , offsetAsSize
    , sizeAsOffset
    ) where

import           GHC.Prim
import           GHC.Int
import           GHC.Types
import           GHC.Word
import           Foundation.Internal.Proxy
import           Foundation.Internal.Base
import           Foundation.Internal.Types
import           Foundation.Primitive.Monad
import qualified Prelude (quot)

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
    primBaIndex :: ByteArray# -> Offset ty -> ty

    -----
    -- MutableByteArray section
    -----

    -- | Read an element at an index in a mutable array
    primMbaRead :: PrimMonad prim
                => MutableByteArray# (PrimState prim) -- ^ mutable array to read from
                -> Offset ty                         -- ^ index of the element to retrieve
                -> prim ty                           -- ^ the element returned

    -- | Write an element to a specific cell in a mutable array.
    primMbaWrite :: PrimMonad prim
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

{-# SPECIALIZE [3] primBaIndex :: ByteArray# -> Offset Word8 -> Word8 #-}

instance PrimType Word8 where
    primSizeInBytes _ = Size 1
    {-# INLINE primSizeInBytes #-}
    primBaIndex ba (Offset (I# n)) = W8# (indexWord8Array# ba n)
    {-# INLINE primBaIndex #-}
    primMbaRead mba (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readWord8Array# mba n s1 in (# s2, W8# r #)
    {-# INLINE primMbaRead #-}
    primMbaWrite mba (Offset (I# n)) (W8# w) = primitive $ \s1 -> (# writeWord8Array# mba n w s1, () #)
    {-# INLINE primMbaWrite #-}
    primAddrIndex addr (Offset (I# n)) = W8# (indexWord8OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readWord8OffAddr# addr n s1 in (# s2, W8# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (W8# w) = primitive $ \s1 -> (# writeWord8OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}

instance PrimType Word16 where
    primSizeInBytes _ = Size 2
    {-# INLINE primSizeInBytes #-}
    primBaIndex ba (Offset (I# n)) = W16# (indexWord16Array# ba n)
    {-# INLINE primBaIndex #-}
    primMbaRead mba (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readWord16Array# mba n s1 in (# s2, W16# r #)
    {-# INLINE primMbaRead #-}
    primMbaWrite mba (Offset (I# n)) (W16# w) = primitive $ \s1 -> (# writeWord16Array# mba n w s1, () #)
    {-# INLINE primMbaWrite #-}
    primAddrIndex addr (Offset (I# n)) = W16# (indexWord16OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readWord16OffAddr# addr n s1 in (# s2, W16# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (W16# w) = primitive $ \s1 -> (# writeWord16OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Word32 where
    primSizeInBytes _ = Size 4
    {-# INLINE primSizeInBytes #-}
    primBaIndex ba (Offset (I# n)) = W32# (indexWord32Array# ba n)
    {-# INLINE primBaIndex #-}
    primMbaRead mba (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readWord32Array# mba n s1 in (# s2, W32# r #)
    {-# INLINE primMbaRead #-}
    primMbaWrite mba (Offset (I# n)) (W32# w) = primitive $ \s1 -> (# writeWord32Array# mba n w s1, () #)
    {-# INLINE primMbaWrite #-}
    primAddrIndex addr (Offset (I# n)) = W32# (indexWord32OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readWord32OffAddr# addr n s1 in (# s2, W32# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (W32# w) = primitive $ \s1 -> (# writeWord32OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Word64 where
    primSizeInBytes _ = Size 8
    {-# INLINE primSizeInBytes #-}
    primBaIndex ba (Offset (I# n)) = W64# (indexWord64Array# ba n)
    {-# INLINE primBaIndex #-}
    primMbaRead mba (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readWord64Array# mba n s1 in (# s2, W64# r #)
    {-# INLINE primMbaRead #-}
    primMbaWrite mba (Offset (I# n)) (W64# w) = primitive $ \s1 -> (# writeWord64Array# mba n w s1, () #)
    {-# INLINE primMbaWrite #-}
    primAddrIndex addr (Offset (I# n)) = W64# (indexWord64OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readWord64OffAddr# addr n s1 in (# s2, W64# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (W64# w) = primitive $ \s1 -> (# writeWord64OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Int8 where
    primSizeInBytes _ = Size 1
    {-# INLINE primSizeInBytes #-}
    primBaIndex ba (Offset (I# n)) = I8# (indexInt8Array# ba n)
    {-# INLINE primBaIndex #-}
    primMbaRead mba (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readInt8Array# mba n s1 in (# s2, I8# r #)
    {-# INLINE primMbaRead #-}
    primMbaWrite mba (Offset (I# n)) (I8# w) = primitive $ \s1 -> (# writeInt8Array# mba n w s1, () #)
    {-# INLINE primMbaWrite #-}
    primAddrIndex addr (Offset (I# n)) = I8# (indexInt8OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readInt8OffAddr# addr n s1 in (# s2, I8# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (I8# w) = primitive $ \s1 -> (# writeInt8OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Int16 where
    primSizeInBytes _ = Size 2
    {-# INLINE primSizeInBytes #-}
    primBaIndex ba (Offset (I# n)) = I16# (indexInt16Array# ba n)
    {-# INLINE primBaIndex #-}
    primMbaRead mba (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readInt16Array# mba n s1 in (# s2, I16# r #)
    {-# INLINE primMbaRead #-}
    primMbaWrite mba (Offset (I# n)) (I16# w) = primitive $ \s1 -> (# writeInt16Array# mba n w s1, () #)
    {-# INLINE primMbaWrite #-}
    primAddrIndex addr (Offset (I# n)) = I16# (indexInt16OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readInt16OffAddr# addr n s1 in (# s2, I16# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (I16# w) = primitive $ \s1 -> (# writeInt16OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Int32 where
    primSizeInBytes _ = Size 4
    {-# INLINE primSizeInBytes #-}
    primBaIndex ba (Offset (I# n)) = I32# (indexInt32Array# ba n)
    {-# INLINE primBaIndex #-}
    primMbaRead mba (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readInt32Array# mba n s1 in (# s2, I32# r #)
    {-# INLINE primMbaRead #-}
    primMbaWrite mba (Offset (I# n)) (I32# w) = primitive $ \s1 -> (# writeInt32Array# mba n w s1, () #)
    {-# INLINE primMbaWrite #-}
    primAddrIndex addr (Offset (I# n)) = I32# (indexInt32OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readInt32OffAddr# addr n s1 in (# s2, I32# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (I32# w) = primitive $ \s1 -> (# writeInt32OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Int64 where
    primSizeInBytes _ = Size 8
    {-# INLINE primSizeInBytes #-}
    primBaIndex ba (Offset (I# n)) = I64# (indexInt64Array# ba n)
    {-# INLINE primBaIndex #-}
    primMbaRead mba (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readInt64Array# mba n s1 in (# s2, I64# r #)
    {-# INLINE primMbaRead #-}
    primMbaWrite mba (Offset (I# n)) (I64# w) = primitive $ \s1 -> (# writeInt64Array# mba n w s1, () #)
    {-# INLINE primMbaWrite #-}
    primAddrIndex addr (Offset (I# n)) = I64# (indexInt64OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readInt64OffAddr# addr n s1 in (# s2, I64# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (I64# w) = primitive $ \s1 -> (# writeInt64OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}

instance PrimType Float where
    primSizeInBytes _ = Size 4
    {-# INLINE primSizeInBytes #-}
    primBaIndex ba (Offset (I# n)) = F# (indexFloatArray# ba n)
    {-# INLINE primBaIndex #-}
    primMbaRead mba (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readFloatArray# mba n s1 in (# s2, F# r #)
    {-# INLINE primMbaRead #-}
    primMbaWrite mba (Offset (I# n)) (F# w) = primitive $ \s1 -> (# writeFloatArray# mba n w s1, () #)
    {-# INLINE primMbaWrite #-}
    primAddrIndex addr (Offset (I# n)) = F# (indexFloatOffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readFloatOffAddr# addr n s1 in (# s2, F# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (F# w) = primitive $ \s1 -> (# writeFloatOffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Double where
    primSizeInBytes _ = Size 8
    {-# INLINE primSizeInBytes #-}
    primBaIndex ba (Offset (I# n)) = D# (indexDoubleArray# ba n)
    {-# INLINE primBaIndex #-}
    primMbaRead mba (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readDoubleArray# mba n s1 in (# s2, D# r #)
    {-# INLINE primMbaRead #-}
    primMbaWrite mba (Offset (I# n)) (D# w) = primitive $ \s1 -> (# writeDoubleArray# mba n w s1, () #)
    {-# INLINE primMbaWrite #-}
    primAddrIndex addr (Offset (I# n)) = D# (indexDoubleOffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readDoubleOffAddr# addr n s1 in (# s2, D# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (D# w) = primitive $ \s1 -> (# writeDoubleOffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}

instance PrimType Char where
    primSizeInBytes _ = Size 4
    {-# INLINE primSizeInBytes #-}
    primBaIndex ba (Offset (I# n)) = C# (indexWideCharArray# ba n)
    {-# INLINE primBaIndex #-}
    primMbaRead mba (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readWideCharArray# mba n s1 in (# s2, C# r #)
    {-# INLINE primMbaRead #-}
    primMbaWrite mba (Offset (I# n)) (C# w) = primitive $ \s1 -> (# writeWideCharArray# mba n w s1, () #)
    {-# INLINE primMbaWrite #-}
    primAddrIndex addr (Offset (I# n)) = C# (indexWideCharOffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let (# s2, r #) = readWideCharOffAddr# addr n s1 in (# s2, C# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (C# w) = primitive $ \s1 -> (# writeWideCharOffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}

-- | Cast a Size linked to type A (Size A) to a Size linked to type B (Size B)
sizeRecast :: (PrimType a, PrimType b) => Size a -> Size b
sizeRecast = doRecast Proxy Proxy
  where doRecast :: (PrimType a, PrimType b) => Proxy a -> Proxy b -> Size a -> Size b
        doRecast pa pb sz =
            let szA          = primSizeInBytes pa
                (Size szB)   = primSizeInBytes pb
                (Size bytes) = sizeOfE szA sz
             in Size (bytes `Prelude.quot` szB)

primOffsetRecast :: (PrimType a, PrimType b) => Offset a -> Offset b
primOffsetRecast = doRecast Proxy Proxy
  where doRecast :: (PrimType a, PrimType b) => Proxy a -> Proxy b -> Offset a -> Offset b
        doRecast pa pb ofs =
            let szA            = primSizeInBytes pa
                (Size szB)     = primSizeInBytes pb
                (Offset bytes) = offsetOfE szA ofs
             in Offset (bytes `Prelude.quot` szB)

primOffsetOfE :: PrimType a => Offset a -> Offset8
primOffsetOfE = getOffset Proxy
  where getOffset :: PrimType a => Proxy a -> Offset a -> Offset8
        getOffset proxy = offsetOfE (primSizeInBytes proxy)

sizeAsOffset :: Size a -> Offset a
sizeAsOffset (Size a) = Offset a
{-# INLINE sizeAsOffset #-}

offsetAsSize :: Offset a -> Size a
offsetAsSize (Offset a) = Size a
{-# INLINE offsetAsSize #-}
