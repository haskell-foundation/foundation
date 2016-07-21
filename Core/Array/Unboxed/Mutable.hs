-- |
-- Module      : Core.Array.Unboxed.Mutable
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
module Core.Array.Unboxed.Mutable
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
    , copyAddr
    -- * Reading and Writing cells
    , unsafeWrite
    , unsafeRead
    , write
    , read
    ) where

import           GHC.Prim
import           GHC.Types
import           GHC.Ptr
import           Core.Internal.Base
import           Core.Internal.Primitive
import           Core.Internal.Proxy
import           Core.Primitive.Monad
import           Core.Primitive.Types
import           Core.Primitive.FinalPtr
import           Core.Array.Common
import           Core.Number
import           Foreign.Marshal.Utils (copyBytes)

-- | A Mutable array of types built on top of GHC primitive.
--
-- Element in this array can be modified in place.
data MUArray ty st = MUVecMA {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !PinnedStatus (MutableByteArray# st)
                   | MUVecAddr {-# UNPACK #-} !Int {-# UNPACK #-} !Int !(FinalPtr ty)

mutableArrayProxyTy :: MUArray ty st -> Proxy ty
mutableArrayProxyTy _ = Proxy

sizeInMutableBytesOfContent :: PrimType ty => MUArray ty s -> Int
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
unsafeRead (MUVecMA start _ _ mba) i = primMbaRead mba (start+i)
unsafeRead (MUVecAddr start _ fptr) i = withFinalPtr fptr $ \(Ptr addr) -> primAddrRead addr (start+i)
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
unsafeWrite (MUVecMA start _ _ mba)  i v = primMbaWrite mba (start+i) v
unsafeWrite (MUVecAddr start _ fptr) i v = withFinalPtr fptr $ \(Ptr addr) -> primAddrWrite addr (start+i) v
{-# INLINE unsafeWrite #-}

-- | Create a new pinned mutable array of size @n.
--
-- all the cells are uninitialized and could contains invalid values.
--
-- All mutable arrays are allocated on a 64 bits aligned addresses
newPinned :: (PrimMonad prim, PrimType ty) => Int -> prim (MUArray ty (PrimState prim))
newPinned n = newFake Proxy
  where newFake :: (PrimMonad prim, PrimType ty) => Proxy ty -> prim (MUArray ty (PrimState prim))
        newFake ty = primitive $ \s1 ->
            case newAlignedPinnedByteArray# bytes 8# s1 of
                (# s2, mba #) -> (# s2, MUVecMA 0 n pinned mba #)
          where
                !(I# bytes) = n * primSizeInBytes ty
        {-# INLINE newFake #-}
{-# INLINE new #-}

newUnpinned :: (PrimMonad prim, PrimType ty) => Int -> prim (MUArray ty (PrimState prim))
newUnpinned n = newFake Proxy
  where newFake :: (PrimMonad prim, PrimType ty) => Proxy ty -> prim (MUArray ty (PrimState prim))
        newFake ty = primitive $ \s1 ->
            case newByteArray# bytes s1 of
                (# s2, mba #) -> (# s2, MUVecMA 0 n unpinned mba #)
          where
                !(I# bytes) = n * primSizeInBytes ty

-- | Create a new mutable array of size @n.
--
-- TODO: heuristic to allocated unpinned (< 1K for example)
new :: (PrimMonad prim, PrimType ty) => Int -> prim (MUArray ty (PrimState prim))
new n
    | n > 0     = newPinned n
    | otherwise = newUnpinned n


mutableSame :: MUArray ty st -> MUArray ty2 st -> Bool
mutableSame (MUVecMA sa ea _ ma) (MUVecMA sb eb _ mb) = and [ sa == sb, ea == eb, bool# (sameMutableByteArray# ma mb)]
mutableSame (MUVecAddr _ f1) (MUVecAddr _ f2) = finalPtrSameMemory f1 f2
mutableSame (MUVecMA {})     (MUVecAddr {})   = False
mutableSame (MUVecAddr {})   (MUVecMA {})     = False


newNative :: (PrimMonad prim, PrimType ty) => Int -> (MutableByteArray# (PrimState prim) -> prim ()) -> prim (MUArray ty (PrimState prim))
newNative n f = do
    muvec <- new n
    case muvec of
        (MUVecMA _ mba) -> f mba >> return muvec
        (MUVecAddr {})  -> error "internal error: unboxed new only supposed to allocate natively"

mutableForeignMem :: (PrimMonad prim, PrimType ty)
                  => FinalPtr ty -- ^ the start pointer with a finalizer
                  -> Int         -- ^ the number of elements (in elements, not bytes)
                  -> prim (MUArray ty (PrimState prim))
mutableForeignMem fptr nb = return $ MUVecAddr nb fptr

-- | Copy a number of elements from an array to another array with offsets
copyAt :: (PrimMonad prim, PrimType ty)
       => MUArray ty (PrimState prim) -- ^ destination array
       -> Int                -- ^ offset at destination
       -> MUArray ty (PrimState prim) -- ^ source array
       -> Int                -- ^ offset at source
       -> Int                -- ^ number of elements to copy
       -> prim ()
copyAt (MUVecMA _ dstMba) ed uvec@(MUVecMA _ srcBa) es n =
    primitive $ \st -> (# copyMutableByteArray# srcBa os dstMba od nBytes st, () #)
  where
    sz = primSizeInBytes (mutableArrayProxyTy uvec)
    !(I# os)     = es * sz
    !(I# od)     = ed * sz
    !(I# nBytes) = n * sz
copyAt (MUVecMA _ dstMba) ed muvec@(MUVecAddr _ srcFptr) es n =
    withFinalPtr srcFptr $ \srcPtr ->
        let !(Ptr srcAddr) = srcPtr `plusPtr` os
         in primitive $ \s -> (# compatCopyAddrToByteArray# srcAddr dstMba od nBytes s, () #)
  where
    sz  = primSizeInBytes (mutableArrayProxyTy muvec)
    !os = es * sz
    !(I# od)     = ed * sz
    !(I# nBytes) = n * sz
copyAt dst od src os n = loop od os
  where
    endIndex = os + n
    loop d i
        | i == endIndex = return ()
        | otherwise     = unsafeRead src i >>= unsafeWrite dst d >> loop (d+1) (i+1)

copyAddr :: (PrimMonad prim, PrimType ty)
         => MUArray ty (PrimState prim) -- ^ destination array
         -> Int                -- ^ offset at destination
         -> Ptr Word8          -- ^ source ptr
         -> Int                -- ^ offset at source
         -> Int                -- ^ number of elements to copy
         -> prim ()
copyAddr (MUVecMA _ dst) (I# od) (Ptr src) (I# os) (I# sz) = primitive $ \s ->
    (# compatCopyAddrToByteArray# (plusAddr# src os) dst od sz s, () #)
copyAddr (MUVecAddr _ fptr) od src os sz =
    withFinalPtr fptr $ \dst ->
        unsafePrimFromIO $ copyBytes (dst `plusPtr` od) (src `plusPtr` os) sz
        --memcpy addr to addr

-- | return the numbers of elements in a mutable array
mutableLength :: PrimType ty => MUArray ty st -> Int
mutableLength = divBits Proxy
  where
    divBits :: PrimType ty => Proxy ty -> MUArray ty st -> Int
    divBits proxy (MUVecMA _ a) =
        let !(I# szBits) = primSizeInBytes proxy
            !elems       = quotInt# (sizeofMutableByteArray# a) szBits
         in I# elems
    divBits _     (MUVecAddr len _) = len
{-# INLINE mutableLength #-}
