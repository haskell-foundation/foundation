module Foundation.Array.Unboxed.ByteArray
    ( MutableByteArray
    , mutableByteArraySet
    , mutableByteArraySetBetween
    , mutableByteArrayMove
    ) where

import Foundation.Internal.Base
import Foundation.Primitive.Monad
import Foundation.Array.Common
import Foundation.Array.Unboxed.Mutable
import Foundation.Numerical
import Control.Monad (forM_)

-- | Mutable Byte Array alias
type MutableByteArray st = MUArray Word8 st

mutableByteArraySet :: PrimMonad prim => MUArray Word8 (PrimState prim) -> Word8 -> prim ()
mutableByteArraySet mba val = do
    -- naive haskell way. TODO: call memset or a 32-bit/64-bit method
    forM_ [0..(len-1)] $ \i -> unsafeWrite mba i val
  where
    len = mutableLength mba

mutableByteArraySetBetween :: PrimMonad prim => MUArray Word8 (PrimState prim) -> Word8 -> Int -> Int -> prim ()
mutableByteArraySetBetween mba val offset size
    | offset < 0                        = throw (OutOfBound OOB_MemSet offset len)
    | offset > len || offset+size > len = throw (OutOfBound OOB_MemSet (offset+size) len)
    | otherwise =
        -- TODO same as mutableByteArraySet
        forM_ [offset..(offset+size-1)] $ \i -> unsafeWrite mba i val
  where
    len = mutableLength mba

mutableByteArrayMove :: PrimMonad prim => MUArray Word8 (PrimState prim) -> Int -> Int -> Int -> prim ()
mutableByteArrayMove _mba _ofs _sz = undefined
