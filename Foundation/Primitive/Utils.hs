-- |
-- Module      : Foundation.Primitive.Utils
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Foundation.Primitive.Utils
    ( primCopyFreezedBytes
    , primCopyFreezedBytesOffset
    , primCopyFreezedW32
    , primCopyFreezedW64
    , primMutableAddrSlideToStart
    , primMutableByteArraySlideToStart
    ) where

import           Foundation.Internal.Base
import           Foundation.Primitive.Types.OffsetSize
import           Foundation.Internal.Primitive
import           Foundation.Primitive.Monad
import           GHC.Prim
import           GHC.Types

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

primMutableByteArraySlideToStart :: PrimMonad m => MutableByteArray# (PrimState m) -> Offset8 -> Offset8 -> m ()
primMutableByteArraySlideToStart mba (Offset (I# ofs)) (Offset (I# end)) = primitive $ \st ->
    (# copyMutableByteArray# mba 0# mba ofs (end -# ofs) st, () #)

primMutableAddrSlideToStart :: PrimMonad m => Addr# -> Offset8 -> Offset8 -> m ()
primMutableAddrSlideToStart addr (Offset (I# ofsIni)) (Offset (I# end)) = primitive $ \st -> (# loop st 0# ofsIni, () #)
  where
    loop !st !dst !ofs
        | bool# (ofs ==# end) = st
        | otherwise           =
            case readWord8OffAddr# addr ofs st of { (# st', v #) ->
            case writeWord8OffAddr# addr dst v st' of { st'' ->
                loop st'' (dst +# 1#) (ofs +# 1#) }}
