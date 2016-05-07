-- |
-- Module      : Core.Primitive.Utils
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Core.Primitive.Utils
    ( primCopyFreezedBytes
    , primCopyFreezedBytesOffset
    , primCopyFreezedW32
    , primCopyFreezedW64
    ) where

import           Core.Internal.Base
import           Core.Internal.Primitive
import           Core.Primitive.Monad
import           GHC.Prim

-- | Copy all bytes from a byteArray# to a mutableByteArray#
primCopyFreezedBytes :: PrimMonad m => MutableByteArray# (PrimState m) -> ByteArray# -> m ()
primCopyFreezedBytes mba ba = primitive $ \st ->
    (# copyByteArray# ba 0# mba 0# (sizeofByteArray# ba) st , () #)
{-# INLINE primCopyFreezedBytes #-}

-- | Copy @nbBytes bytes from a byteArray# to a mutableByteArray# starting at an offset
primCopyFreezedBytesOffset :: PrimMonad m => MutableByteArray# (PrimState m) -> Int# -> ByteArray# -> Int# -> m ()
primCopyFreezedBytesOffset mba ofs ba nbBytes = primitive $ \st ->
    (# copyByteArray# ba 0# mba ofs nbBytes st , () #)
{-# INLINE primCopyFreezedBytesOffset #-}

-- | same as 'primCopyFreezedBytes' except copy using 32 bits word
primCopyFreezedW32 :: PrimMonad m => MutableByteArray# (PrimState m) -> ByteArray# -> m ()
primCopyFreezedW32 mba ba = primitive $ \st -> (# loop st 0#, () #)
  where
    !len = quotInt# (sizeofByteArray# ba) 8#
    loop !st !n
        | bool# (n ==# len) = st
        | otherwise         = loop (writeWord32Array# mba n (indexWord32Array# ba n) st) (n +# 1#)
    {-# INLINE loop #-}
{-# INLINE primCopyFreezedW32 #-}

-- | same as 'primCopyFreezedBytes' except copy using 64 bits word
primCopyFreezedW64 :: PrimMonad m => MutableByteArray# (PrimState m) -> ByteArray# -> m ()
primCopyFreezedW64 mba ba = primitive $ \st -> (# loop st 0#, () #)
  where
    !len = quotInt# (sizeofByteArray# ba) 8#
    loop !st !n
        | bool# (n ==# len) = st
        | otherwise         = loop (writeWord64Array# mba n (indexWord64Array# ba n) st) (n +# 1#)
    {-# INLINE loop #-}
{-# INLINE primCopyFreezedW64 #-}
