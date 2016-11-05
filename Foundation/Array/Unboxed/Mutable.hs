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
module Foundation.Array.Unboxed.Mutable
    ( MUArray(..)
    -- * Property queries
    , sizeInMutableBytesOfContent
    , mutableLength
    , mutableSame
    -- * Allocation & Copy
    , new
    , newPinned
    , newNative
    , mutableForeignMem
    , copyAt
    -- , copyAddr
    -- * Reading and Writing cells
    , unsafeWrite
    , unsafeRead
    , write
    , read
    ) where

import           GHC.Prim
import           GHC.Types
import           GHC.Ptr
import           Foundation.Internal.Base
import qualified Foundation.Internal.Environment as Environment
import           Foundation.Internal.Types
import           Foundation.Internal.Primitive
import           Foundation.Internal.Proxy
import           Foundation.Primitive.Monad
import           Foundation.Primitive.Types
import           Foundation.Primitive.FinalPtr
import           Foundation.Array.Common
import           Foundation.Numerical
-- import           Foreign.Marshal.Utils (copyBytes)

-- | A Mutable array of types built on top of GHC primitive.
--
-- Element in this array can be modified in place.
data MUArray ty st =
      MUVecMA {-# UNPACK #-} !(Offset ty)
              {-# UNPACK #-} !(Size ty)
              {-# UNPACK #-} !PinnedStatus
                             (MutableByteArray# st)
    | MUVecAddr {-# UNPACK #-} !(Offset ty)
                {-# UNPACK #-} !(Size ty)
                               !(FinalPtr ty)

mutableArrayProxyTy :: MUArray ty st -> Proxy ty
mutableArrayProxyTy _ = Proxy

sizeInMutableBytesOfContent :: PrimType ty => MUArray ty s -> Size8
sizeInMutableBytesOfContent = primSizeInBytes . mutableArrayProxyTy
{-# INLINE sizeInMutableBytesOfContent #-}

-- | read a cell in a mutable array.
--
-- If the index is out of bounds, an error is raised.
read :: (PrimMonad prim, PrimType ty) => MUArray ty (PrimState prim) -> Int -> prim ty
read array n
    | n < 0 || n >= len = primThrow (OutOfBound OOB_Read n len)
    | otherwise         = unsafeRead array n
  where len = mutableLength array
{-# INLINE read #-}

-- | read from a cell in a mutable array without bounds checking.
--
-- Reading from invalid memory can return unpredictable and invalid values.
-- use 'read' if unsure.
unsafeRead :: (PrimMonad prim, PrimType ty) => MUArray ty (PrimState prim) -> Int -> prim ty
unsafeRead (MUVecMA start _ _ mba) i = primMbaRead mba (start+.i)
unsafeRead (MUVecAddr start _ fptr) i = withFinalPtr fptr $ \(Ptr addr) -> primAddrRead addr (start+.i)
{-# INLINE unsafeRead #-}

-- | Write to a cell in a mutable array.
--
-- If the index is out of bounds, an error is raised.
write :: (PrimMonad prim, PrimType ty) => MUArray ty (PrimState prim) -> Int -> ty -> prim ()
write array n val
    | n < 0 || n >= len = primThrow (OutOfBound OOB_Write n len)
    | otherwise         = unsafeWrite array n val
  where
    len = mutableLength array
{-# INLINE write #-}

-- | write to a cell in a mutable array without bounds checking.
--
-- Writing with invalid bounds will corrupt memory and your program will
-- become unreliable. use 'write' if unsure.
unsafeWrite :: (PrimMonad prim, PrimType ty) => MUArray ty (PrimState prim) -> Int -> ty -> prim ()
unsafeWrite (MUVecMA start _ _ mba)  i v = primMbaWrite mba (start+.i) v
unsafeWrite (MUVecAddr start _ fptr) i v = withFinalPtr fptr $ \(Ptr addr) -> primAddrWrite addr (start+.i) v
{-# INLINE unsafeWrite #-}

-- | Create a new pinned mutable array of size @n.
--
-- all the cells are uninitialized and could contains invalid values.
--
-- All mutable arrays are allocated on a 64 bits aligned addresses
newPinned :: (PrimMonad prim, PrimType ty) => Size ty -> prim (MUArray ty (PrimState prim))
newPinned n = newFake n Proxy
  where newFake :: (PrimMonad prim, PrimType ty) => Size ty -> Proxy ty -> prim (MUArray ty (PrimState prim))
        newFake sz ty = primitive $ \s1 ->
            case newAlignedPinnedByteArray# bytes 8# s1 of
                (# s2, mba #) -> (# s2, MUVecMA (Offset 0) sz pinned mba #)
          where
                !(Size (I# bytes)) = sizeOfE (primSizeInBytes ty) sz
        {-# INLINE newFake #-}

newUnpinned :: (PrimMonad prim, PrimType ty) => Size ty -> prim (MUArray ty (PrimState prim))
newUnpinned n = newFake n Proxy
  where newFake :: (PrimMonad prim, PrimType ty) => Size ty -> Proxy ty -> prim (MUArray ty (PrimState prim))
        newFake sz ty = primitive $ \s1 ->
            case newByteArray# bytes s1 of
                (# s2, mba #) -> (# s2, MUVecMA (Offset 0) sz unpinned mba #)
          where
                !(Size (I# bytes)) = sizeOfE (primSizeInBytes ty) sz
        {-# INLINE newFake #-}

-- | Create a new mutable array of size @n.
--
-- When memory for a new array is allocated, we decide if that memory region
-- should be pinned (will not be copied around by GC) or unpinned (can be
-- moved around by GC) depending on its size.
--
-- You can change the threshold value used by setting the environment variable
-- @HS_FOUNDATION_UARRAY_UNPINNED_MAX@.
new :: (PrimMonad prim, PrimType ty) => Size ty -> prim (MUArray ty (PrimState prim))
new sz
    | sizeRecast sz <= maxSizeUnpinned = newUnpinned sz
    | otherwise                        = newPinned sz
  where
    -- Safe to use here: If the value changes during runtime, this will only
    -- have an impact on newly created arrays.
    maxSizeUnpinned = Environment.unsafeUArrayUnpinnedMaxSize
{-# INLINE new #-}

mutableSame :: MUArray ty st -> MUArray ty st -> Bool
mutableSame (MUVecMA sa ea _ ma) (MUVecMA sb eb _ mb) = and [ sa == sb, ea == eb, bool# (sameMutableByteArray# ma mb)]
mutableSame (MUVecAddr s1 e1 f1) (MUVecAddr s2 e2 f2) = and [ s1 == s2, e1 == e2, finalPtrSameMemory f1 f2 ]
mutableSame (MUVecMA {})     (MUVecAddr {})   = False
mutableSame (MUVecAddr {})   (MUVecMA {})     = False


newNative :: (PrimMonad prim, PrimType ty) => Size ty -> (MutableByteArray# (PrimState prim) -> prim ()) -> prim (MUArray ty (PrimState prim))
newNative n f = do
    muvec <- new n
    case muvec of
        (MUVecMA _ _ _ mba) -> f mba >> return muvec
        (MUVecAddr {})      -> error "internal error: unboxed new only supposed to allocate natively"

mutableForeignMem :: (PrimMonad prim, PrimType ty)
                  => FinalPtr ty -- ^ the start pointer with a finalizer
                  -> Int         -- ^ the number of elements (in elements, not bytes)
                  -> prim (MUArray ty (PrimState prim))
mutableForeignMem fptr nb = return $ MUVecAddr (Offset 0) (Size nb) fptr

-- | Copy a number of elements from an array to another array with offsets
copyAt :: (PrimMonad prim, PrimType ty)
       => MUArray ty (PrimState prim) -- ^ destination array
       -> Offset ty                  -- ^ offset at destination
       -> MUArray ty (PrimState prim) -- ^ source array
       -> Offset ty                  -- ^ offset at source
       -> Size ty                    -- ^ number of elements to copy
       -> prim ()
copyAt (MUVecMA dstStart _ _ dstMba) ed uvec@(MUVecMA srcStart _ _ srcBa) es n =
    primitive $ \st -> (# copyMutableByteArray# srcBa os dstMba od nBytes st, () #)
  where
    !sz                 = primSizeInBytes (mutableArrayProxyTy uvec)
    !(Offset (I# os))   = offsetOfE sz (srcStart + es)
    !(Offset (I# od))   = offsetOfE sz (dstStart + ed)
    !(Size (I# nBytes)) = sizeOfE sz n
copyAt (MUVecMA dstStart _ _ dstMba) ed muvec@(MUVecAddr srcStart _ srcFptr) es n =
    withFinalPtr srcFptr $ \srcPtr ->
        let !(Ptr srcAddr) = srcPtr `plusPtr` os
         in primitive $ \s -> (# compatCopyAddrToByteArray# srcAddr dstMba od nBytes s, () #)
  where
    !sz                 = primSizeInBytes (mutableArrayProxyTy muvec)
    !(Offset os)        = offsetOfE sz (srcStart + es)
    !(Offset (I# od))   = offsetOfE sz (dstStart + ed)
    !(Size (I# nBytes)) = sizeOfE sz n
copyAt dst od src os n = loop od os
  where
    !(Offset endIndex) = os `offsetPlusE` n
    loop !(Offset d) !(Offset i)
        | i == endIndex = return ()
        | otherwise     = unsafeRead src i >>= unsafeWrite dst d >> loop (Offset $ d+1) (Offset $ i+1)

{-
copyAddr :: (PrimMonad prim, PrimType ty)
         => MUArray ty (PrimState prim) -- ^ destination array
         -> Offset ty                  -- ^ offset at destination
         -> Ptr Word8                   -- ^ source ptr
         -> Offset ty                  -- ^ offset at source
         -> Size ty                    -- ^ number of elements to copy
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
mutableLength (MUVecMA _ (Size end) _ _) = end
mutableLength (MUVecAddr _ (Size end) _) = end
