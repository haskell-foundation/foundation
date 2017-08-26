{-# LANGUAGE MagicHash #-}
module Basement.Alg.Foreign.Prim
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

type Immutable = Addr#
type Mutable st = Addr#

primIndex :: PrimType ty => Immutable -> Offset ty -> ty
primIndex = primAddrIndex

primRead :: (PrimMonad prim, PrimType ty) => Mutable (PrimState prim) -> Offset ty -> prim ty
primRead = primAddrRead

primWrite :: (PrimMonad prim, PrimType ty) => Mutable (PrimState prim) -> Offset ty -> ty -> prim ()
primWrite = primAddrWrite
