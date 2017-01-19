-- |
-- Module      : Foundation.Array.Mutable
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
module Foundation.Collection.Mutable
    ( MutableCollection(..)
    ) where

import Foundation.Primitive.Monad
import Foundation.Internal.Base
import Foundation.Internal.Types

import qualified Foundation.Array.Unboxed.Mutable as MUV
import qualified Foundation.Array.Unboxed as UV
import qualified Foundation.Array.Boxed as BA

-- | Collection of things that can be made mutable, modified and then freezed into an MutableFreezed collection
class MutableCollection c where
    -- unfortunately: cannot set mutUnsafeWrite to default to mutWrite .. same for read..
    {-# MINIMAL thaw, freeze, mutNew, mutWrite, mutRead, mutUnsafeWrite, mutUnsafeRead #-}
    type MutableFreezed c
    type MutableKey c
    type MutableValue c

    unsafeThaw   :: PrimMonad prim => MutableFreezed c -> prim (c (PrimState prim))
    unsafeThaw = thaw
    unsafeFreeze :: PrimMonad prim => c (PrimState prim) -> prim (MutableFreezed c)
    unsafeFreeze = freeze

    thaw   :: PrimMonad prim => MutableFreezed c -> prim (c (PrimState prim))
    freeze :: PrimMonad prim => c (PrimState prim) -> prim (MutableFreezed c)

    mutNew :: PrimMonad prim => Int -> prim (c (PrimState prim))

    mutUnsafeWrite :: PrimMonad prim => c (PrimState prim) -> MutableKey c -> MutableValue c -> prim ()
    mutWrite       :: PrimMonad prim => c (PrimState prim) -> MutableKey c -> MutableValue c -> prim ()
    mutUnsafeRead :: PrimMonad prim => c (PrimState prim) -> MutableKey c -> prim (MutableValue c)
    mutRead       :: PrimMonad prim => c (PrimState prim) -> MutableKey c -> prim (MutableValue c)

instance UV.PrimType ty => MutableCollection (MUV.MUArray ty) where
    type MutableFreezed (MUV.MUArray ty) = UV.UArray ty
    type MutableKey (MUV.MUArray ty) = Int
    type MutableValue (MUV.MUArray ty) = ty

    thaw = UV.thaw
    freeze = UV.freeze
    unsafeThaw = UV.unsafeThaw
    unsafeFreeze = UV.unsafeFreeze

    mutNew i = MUV.new (Size i)

    mutUnsafeWrite = MUV.unsafeWrite
    mutUnsafeRead = MUV.unsafeRead
    mutWrite = MUV.write
    mutRead = MUV.read

instance MutableCollection (BA.MArray ty) where
    type MutableFreezed (BA.MArray ty) = BA.Array ty
    type MutableKey (BA.MArray ty) = Int
    type MutableValue (BA.MArray ty) = ty

    thaw = BA.thaw
    freeze = BA.freeze
    unsafeThaw = BA.unsafeThaw
    unsafeFreeze = BA.unsafeFreeze

    mutNew n = BA.new (Size n)
    mutUnsafeWrite = BA.unsafeWrite
    mutUnsafeRead = BA.unsafeRead
    mutWrite = BA.write
    mutRead = BA.read
