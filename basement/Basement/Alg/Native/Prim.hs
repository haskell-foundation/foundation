{-# LANGUAGE MagicHash #-}
module Basement.Alg.Native.Prim
    ( Immutable
    , Mutable
    , primIndex
    , primIndex64
    , primRead
    , primWrite
    ) where

import           GHC.Types
import           GHC.Prim
import           GHC.Word
import           Basement.Types.OffsetSize
import           Basement.PrimType
import           Basement.Monad

type Immutable = ByteArray#
type Mutable st = MutableByteArray# st

primIndex :: PrimType ty => Immutable -> Offset ty -> ty
primIndex = primBaIndex
{-# INLINE primIndex #-}

primIndex64 :: Immutable -> Offset Word64 -> Word64
primIndex64 = primIndex
{-# INLINE primIndex64 #-}

primRead :: (PrimMonad prim, PrimType ty) => Mutable (PrimState prim) -> Offset ty -> prim ty
primRead = primMbaRead
{-# INLINE primRead #-}

primWrite :: (PrimMonad prim, PrimType ty) => Mutable (PrimState prim) -> Offset ty -> ty -> prim ()
primWrite = primMbaWrite
{-# INLINE primWrite #-}
