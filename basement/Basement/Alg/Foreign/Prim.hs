{-# LANGUAGE MagicHash #-}
module Basement.Alg.Foreign.Prim
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

type Immutable = Addr#
type Mutable st = Addr#

primIndex :: PrimType ty => Immutable -> Offset ty -> ty
primIndex = primAddrIndex
{-# INLINE primIndex #-}

primIndex64 :: Immutable -> Offset Word64 -> Word64
primIndex64 = primIndex
{-# INLINE primIndex64 #-}

primRead :: (PrimMonad prim, PrimType ty) => Mutable (PrimState prim) -> Offset ty -> prim ty
primRead = primAddrRead
{-# INLINE primRead #-}

primWrite :: (PrimMonad prim, PrimType ty) => Mutable (PrimState prim) -> Offset ty -> ty -> prim ()
primWrite = primAddrWrite
{-# INLINE primWrite #-}
