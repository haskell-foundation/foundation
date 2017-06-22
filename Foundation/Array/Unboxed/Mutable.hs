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
    , mutableLengthSize
    , mutableSame
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
  where len = mutableLengthSize array
{-# INLINE read #-}

-- | Write to a cell in a mutable array.
--
-- If the index is out of bounds, an error is raised.
write :: (PrimMonad prim, PrimType ty) => MUArray ty (PrimState prim) -> Offset ty -> ty -> prim ()
write array n val
    | isOutOfBound n len = primOutOfBound OOB_Write n len
    | otherwise          = unsafeWrite array n val
  where
    len = mutableLengthSize array
{-# INLINE write #-}

empty :: PrimMonad prim => prim (MUArray ty (PrimState prim))
empty = primitive $ \s1 -> case newByteArray# 0# s1 of { (# s2, mba #) -> (# s2, MUVecMA 0 0 unpinned mba #) }

mutableSame :: MUArray ty st -> MUArray ty st -> Bool
mutableSame (MUVecMA sa ea _ ma) (MUVecMA sb eb _ mb) = (sa == sb) && (ea == eb) && bool# (sameMutableByteArray# ma mb)
mutableSame (MUVecAddr s1 e1 f1) (MUVecAddr s2 e2 f2) = (s1 == s2) && (e1 == e2) && finalPtrSameMemory f1 f2
mutableSame MUVecMA {}     MUVecAddr {}   = False
mutableSame MUVecAddr {}   MUVecMA {}     = False

mutableForeignMem :: (PrimMonad prim, PrimType ty)
                  => FinalPtr ty -- ^ the start pointer with a finalizer
                  -> Int         -- ^ the number of elements (in elements, not bytes)
                  -> prim (MUArray ty (PrimState prim))
mutableForeignMem fptr nb = return $ MUVecAddr (Offset 0) (CountOf nb) fptr

sub :: (PrimMonad prim, PrimType ty)
    => MUArray ty (PrimState prim)
    -> Int -- The number of elements to drop ahead
    -> Int -- Then the number of element to retain
    -> prim (MUArray ty (PrimState prim))
sub (MUVecMA start sz pstatus mba) dropElems' takeElems
    | takeElems <= 0 = empty
    | resultEmpty    = empty
    | otherwise      = return $ MUVecMA (start `offsetPlusE` dropElems) (min (CountOf takeElems) (sz - dropElems)) pstatus mba
  where
    dropElems = max 0 (CountOf dropElems')
    resultEmpty = dropElems >= sz
sub (MUVecAddr start sz addr) dropElems' takeElems
    | takeElems <= 0 = empty
    | resultEmpty    = empty
    | otherwise      = return $ MUVecAddr (start `offsetPlusE` dropElems) (min (CountOf takeElems) (sz - dropElems)) addr
  where
    dropElems = max 0 (CountOf dropElems')
    resultEmpty = dropElems >= sz

{-
copyAddr :: (PrimMonad prim, PrimType ty)
         => MUArray ty (PrimState prim) -- ^ destination array
         -> Offset ty                  -- ^ offset at destination
         -> Ptr Word8                   -- ^ source ptr
         -> Offset ty                  -- ^ offset at source
         -> CountOf ty                    -- ^ number of elements to copy
         -> prim ()
copyAddr (MUVecMA dstStart _ _ dst) dstOfs (Ptr src) srcOfs sz = primitive $ \s ->
    (# compatCopyAddrToByteArray# (plusAddr# src os) dst od sz s, () #)
copyAddr (MUVecAddr start _ fptr) od src os sz =
    withFinalPtr fptr $ \dst ->
        unsafePrimFromIO $ copyBytes (dst `plusPtr` od) (src `plusPtr` os) sz
        --memcpy addr to addr
        -}

-- | return the numbers of elements in a mutable array
mutableLength :: PrimType ty => MUArray ty st -> Int
mutableLength (MUVecMA _ (CountOf end) _ _) = end
mutableLength (MUVecAddr _ (CountOf end) _) = end

mutableLengthSize :: PrimType ty => MUArray ty st -> CountOf ty
mutableLengthSize (MUVecMA _ end _ _) = end
mutableLengthSize (MUVecAddr _ end _) = end

withMutablePtrHint :: forall ty prim a . (PrimMonad prim, PrimType ty)
                   => Bool
                   -> Bool
                   -> MUArray ty (PrimState prim)
                   -> (Ptr ty -> prim a)
                   -> prim a
withMutablePtrHint _ _ (MUVecAddr start _ fptr)  f =
    withFinalPtr fptr (\ptr -> f (ptr `plusPtr` os))
  where
    sz           = primSizeInBytes (Proxy :: Proxy ty)
    !(Offset os) = offsetOfE sz start
withMutablePtrHint skipCopy skipCopyBack vec@(MUVecMA start vecSz pstatus a) f
    | isPinned pstatus = mutableByteArrayContent a >>= \ptr -> f (ptr `plusPtr` os)
    | otherwise        = do
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
copyFromPtr (Ptr p) count (MUVecMA ofs arrSz _ mba)
    | count > arrSz = primOutOfBound OOB_MemCopy (sizeAsOffset count) arrSz
    | otherwise     = primitive $ \st -> (# copyAddrToByteArray# p mba od countBytes st, () #)
  where
    !sz                     = primSizeInBytes (Proxy :: Proxy ty)
    !(CountOf (I# countBytes)) = sizeOfE sz count
    !(Offset (I# od))       = offsetOfE sz ofs
copyFromPtr p count (MUVecAddr ofs arrSz fptr)
    | count > arrSz = primOutOfBound OOB_MemCopy (sizeAsOffset count) arrSz
    | otherwise     = withFinalPtr fptr $ \dstPtr ->
        unsafePrimFromIO $ copyBytes (dstPtr `plusPtr` os) p bytes
  where
        sz = primSizeInBytes (Proxy :: Proxy ty)
        !(CountOf bytes) = sizeOfE sz count
        !(Offset os) = offsetOfE sz ofs

-- | Copy all the block content to the memory starting at the destination address
copyToPtr :: forall ty prim . (PrimType ty, PrimMonad prim)
          => MUArray ty (PrimState prim) -- ^ the source mutable array to copy
          -> Ptr ty                      -- ^ The destination address where the copy is going to start
          -> prim ()
copyToPtr (MUVecMA start sz _ ma) (Ptr p) = primitive $ \s1 ->
    case unsafeFreezeByteArray# ma s1 of
        (# s2, ba #) -> (# compatCopyByteArrayToAddr# ba offset p szBytes s2, () #)
  where
    !(Offset (I# offset)) = offsetInBytes start
    !(CountOf (I# szBytes)) = sizeInBytes sz
copyToPtr (MUVecAddr start sz fptr) dst =
    unsafePrimFromIO $ withFinalPtr fptr $ \ptr -> copyBytes dst (ptr `plusPtr` os) szBytes
  where
    !(Offset os)    = offsetInBytes start
    !(CountOf szBytes) = sizeInBytes sz
