-- |
-- Module      : Foundation.Array.Unboxed.Mutable
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- A simple array abstraction that allow to use typed
-- array of bytes where the array is pinned in memory
-- to allow easy use with Foreign interfaces, ByteString
-- and always aligned to 64 bytes.
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Foundation.Array.Unboxed.Mutable
    ( MUArray(..)
    -- * Property queries
    , sizeInMutableBytesOfContent
    , mutableLength
    , mutableOffset
    , mutableSame
    , onMutableBackend
    -- * Allocation & Copy
    , new
    , newPinned
    , newNative
    , mutableForeignMem
    , copyAt
    , copyFromPtr
    , copyToPtr
    , sub
    -- , copyAddr
    -- * Reading and Writing cells
    , unsafeWrite
    , unsafeRead
    , write
    , read
    , withMutablePtr
    ) where

import           GHC.Prim
import           GHC.Types
import           GHC.Ptr
import           Foundation.Internal.Base
import           Foundation.Internal.Primitive
import           Foundation.Internal.Proxy
import           Foundation.Primitive.Types.OffsetSize
import           Foundation.Primitive.Monad
import           Foundation.Primitive.Types
import           Foundation.Primitive.FinalPtr
import           Foundation.Primitive.Exception
import           Foundation.Primitive.UArray.Base
import           Foundation.Numerical
import           Foreign.Marshal.Utils (copyBytes)

sizeInMutableBytesOfContent :: forall ty s . PrimType ty => MUArray ty s -> Size8
sizeInMutableBytesOfContent _ = primSizeInBytes (Proxy :: Proxy ty)
{-# INLINE sizeInMutableBytesOfContent #-}

-- | read a cell in a mutable array.
--
-- If the index is out of bounds, an error is raised.
read :: (PrimMonad prim, PrimType ty) => MUArray ty (PrimState prim) -> Offset ty -> prim ty
read array n
    | isOutOfBound n len = primOutOfBound OOB_Read n len
    | otherwise          = unsafeRead array n
  where len = mutableLength array
{-# INLINE read #-}

-- | Write to a cell in a mutable array.
--
-- If the index is out of bounds, an error is raised.
write :: (PrimMonad prim, PrimType ty) => MUArray ty (PrimState prim) -> Offset ty -> ty -> prim ()
write array n val
    | isOutOfBound n len = primOutOfBound OOB_Write n len
    | otherwise          = unsafeWrite array n val
  where
    len = mutableLength array
{-# INLINE write #-}

empty :: PrimMonad prim => prim (MUArray ty (PrimState prim))
empty = primitive $ \s1 -> case newByteArray# 0# s1 of { (# s2, mba #) -> (# s2, MUArrayMBA 0 0 mba #) }

mutableSame :: MUArray ty st -> MUArray ty st -> Bool
mutableSame (MUArrayMBA sa ea ma) (MUArrayMBA sb eb mb)     = (sa == sb) && (ea == eb) && bool# (sameMutableByteArray# ma mb)
mutableSame (MUArrayAddr s1 e1 f1) (MUArrayAddr s2 e2 f2) = (s1 == s2) && (e1 == e2) && finalPtrSameMemory f1 f2
mutableSame MUArrayMBA {}     MUArrayAddr {}   = False
mutableSame MUArrayAddr {}   MUArrayMBA {}     = False

mutableForeignMem :: (PrimMonad prim, PrimType ty)
                  => FinalPtr ty -- ^ the start pointer with a finalizer
                  -> Int         -- ^ the number of elements (in elements, not bytes)
                  -> prim (MUArray ty (PrimState prim))
mutableForeignMem fptr nb = return $ MUArrayAddr (Offset 0) (CountOf nb) fptr

sub :: (PrimMonad prim, PrimType ty)
    => MUArray ty (PrimState prim)
    -> Int -- The number of elements to drop ahead
    -> Int -- Then the number of element to retain
    -> prim (MUArray ty (PrimState prim))
sub (MUArrayMBA start sz mba) dropElems' takeElems
    | takeElems <= 0 = empty
    | resultEmpty    = empty
    | otherwise      = return $ MUArrayMBA (start `offsetPlusE` dropElems) (min (CountOf takeElems) (sz - dropElems)) mba
  where
    dropElems = max 0 (CountOf dropElems')
    resultEmpty = dropElems >= sz
sub (MUArrayAddr start sz addr) dropElems' takeElems
    | takeElems <= 0 = empty
    | resultEmpty    = empty
    | otherwise      = return $ MUArrayAddr (start `offsetPlusE` dropElems) (min (CountOf takeElems) (sz - dropElems)) addr
  where
    dropElems = max 0 (CountOf dropElems')
    resultEmpty = dropElems >= sz

-- | return the numbers of elements in a mutable array
mutableLength :: PrimType ty => MUArray ty st -> CountOf ty
mutableLength (MUArrayMBA _ end _)   = end
mutableLength (MUArrayAddr _ end _) = end

withMutablePtrHint :: forall ty prim a . (PrimMonad prim, PrimType ty)
                   => Bool
                   -> Bool
                   -> MUArray ty (PrimState prim)
                   -> (Ptr ty -> prim a)
                   -> prim a
withMutablePtrHint _ _ (MUArrayAddr start _ fptr)  f =
    withFinalPtr fptr (\ptr -> f (ptr `plusPtr` os))
  where
    sz           = primSizeInBytes (Proxy :: Proxy ty)
    !(Offset os) = offsetOfE sz start
withMutablePtrHint skipCopy skipCopyBack vec@(MUArrayMBA start vecSz a) f
    | isMutablePinned vec == Pinned = mutableByteArrayContent a >>= \ptr -> f (ptr `plusPtr` os)
    | otherwise                     = do
        trampoline <- newPinned vecSz
        if not skipCopy
            then copyAt trampoline 0 vec 0 vecSz
            else pure ()
        r <- withMutablePtr trampoline f
        if not skipCopyBack
            then copyAt vec 0 trampoline 0 vecSz
            else pure ()
        pure r
  where
    !(Offset os) = offsetOfE sz start
    sz           = primSizeInBytes (Proxy :: Proxy ty)

    mutableByteArrayContent :: PrimMonad prim => MutableByteArray# (PrimState prim) -> prim (Ptr ty)
    mutableByteArrayContent mba = primitive $ \s1 ->
        case unsafeFreezeByteArray# mba s1 of
            (# s2, ba #) -> (# s2, Ptr (byteArrayContents# ba) #)

-- | Create a pointer on the beginning of the mutable array
-- and call a function 'f'.
--
-- The mutable buffer can be mutated by the 'f' function
-- and the change will be reflected in the mutable array
--
-- If the mutable array is unpinned, a trampoline buffer
-- is created and the data is only copied when 'f' return.
withMutablePtr :: (PrimMonad prim, PrimType ty)
               => MUArray ty (PrimState prim)
               -> (Ptr ty -> prim a)
               -> prim a
withMutablePtr = withMutablePtrHint False False

-- | Copy from a pointer, @count@ elements, into the mutable array
copyFromPtr :: forall prim ty . (PrimMonad prim, PrimType ty)
            => Ptr ty -> CountOf ty -> MUArray ty (PrimState prim) -> prim ()
copyFromPtr src@(Ptr src#) count marr
    | count > arrSz = primOutOfBound OOB_MemCopy (sizeAsOffset count) arrSz
    | otherwise     = onMutableBackend copyNative copyPtr marr
  where
    arrSz = mutableLength marr
    ofs = mutableOffset marr

    sz = primSizeInBytes (Proxy :: Proxy ty)
    !(CountOf bytes@(I# bytes#)) = sizeOfE sz count
    !(Offset od@(I# od#)) = offsetOfE sz $ ofs

    copyNative mba = primitive $ \st -> (# copyAddrToByteArray# src# mba od# bytes# st, () #)
    copyPtr fptr = withFinalPtr fptr $ \dst ->
        unsafePrimFromIO $ copyBytes (dst `plusPtr` od) src bytes

-- | Copy all the block content to the memory starting at the destination address
copyToPtr :: forall ty prim . (PrimType ty, PrimMonad prim)
          => MUArray ty (PrimState prim) -- ^ the source mutable array to copy
          -> Ptr ty                      -- ^ The destination address where the copy is going to start
          -> prim ()
copyToPtr marr dst@(Ptr dst#) = onMutableBackend copyNative copyPtr marr
  where
    copyNative mba = primitive $ \s1 ->
        case unsafeFreezeByteArray# mba s1 of
            (# s2, ba #) -> (# compatCopyByteArrayToAddr# ba os# dst# szBytes# s2, () #)
    copyPtr fptr = unsafePrimFromIO $ withFinalPtr fptr $ \ptr ->
        copyBytes dst (ptr `plusPtr` os) szBytes

    !(Offset os@(I# os#)) = offsetInBytes $ mutableOffset marr
    !(CountOf szBytes@(I# szBytes#)) = sizeInBytes $ mutableLength marr

mutableOffset :: MUArray ty st -> Offset ty
mutableOffset (MUArrayMBA ofs _ _) = ofs
mutableOffset (MUArrayAddr ofs _ _) = ofs
