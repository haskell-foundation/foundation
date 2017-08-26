{-# LANGUAGE MagicHash #-}
module Basement.Alg.Native.Prim
    ( Immutable
    , Mutable
    , primIndex
    , primRead
    , primWrite
    ) where

import           GHC.Types
import           GHC.Prim
import           Basement.Types.OffsetSize
import           Basement.PrimType
import           Basement.Monad

type Immutable = ByteArray#
type Mutable st = MutableByteArray# st

primIndex :: PrimType ty => Immutable -> Offset ty -> ty
primIndex = primBaIndex

primRead :: (PrimMonad prim, PrimType ty) => Mutable (PrimState prim) -> Offset ty -> prim ty
primRead = primMbaRead

primWrite :: (PrimMonad prim, PrimType ty) => Mutable (PrimState prim) -> Offset ty -> ty -> prim ()
primWrite = primMbaWrite
