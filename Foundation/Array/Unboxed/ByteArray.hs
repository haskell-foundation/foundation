module Foundation.Array.Unboxed.ByteArray
    ( MutableByteArray
    , mutableByteArraySet
    -- , mutableByteArraySetBetween
    -- , mutableByteArrayMove
    ) where

import Foundation.Internal.Base
import Foundation.Primitive.Types.OffsetSize
import Foundation.Primitive.Monad
import Foundation.Array.Unboxed.Mutable
import Control.Monad (forM_)

-- | Mutable Byte Array alias
type MutableByteArray st = MUArray Word8 st

mutableByteArraySet :: PrimMonad prim => MUArray Word8 (PrimState prim) -> Word8 -> prim ()
mutableByteArraySet mba val = do
    -- naive haskell way. TODO: call memset or a 32-bit/64-bit method
    forM_ [0..(sizeLastOffset len)] $ \i -> unsafeWrite mba i val
  where
    len = mutableLengthSize mba

{-
mutableByteArraySetBetween :: PrimMonad prim => MUArray Word8 (PrimState prim) -> Word8 -> Offset Word8 -> CountOf Word8 -> prim ()
mutableByteArraySetBetween mba val offset size
    | offset < 0                        = primOutOfBound OOB_MemSet offset                      len
    | offset > len || offset+size > len = primOutOfBound OOB_MemSet (offset `OffsetPlusE` size) len
    | otherwise =
        -- TODO same as mutableByteArraySet
        forM_ [offset..(offset + sizeLastOffset size)] $ \i -> unsafeWrite mba i val
  where
    len = mutableLengthSize mba

mutableByteArrayMove :: PrimMonad prim => MUArray Word8 (PrimState prim) -> Int -> Int -> Int -> prim ()
mutableByteArrayMove _mba _ofs _sz = undefined
    -}
