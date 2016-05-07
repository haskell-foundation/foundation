-- |
-- Module      : Core.Vector.Mutable
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
module Core.Collection.Mutable
    ( MutableCollection(..)
    ) where

import Core.Primitive.Monad

-- | Collection of things that can be made mutable, modified and then freezed into an immutable collection
class MutableCollection c where
    -- unfortunately: cannot set mutUnsafeWrite to default to mutWrite .. same for read..
    {-# MINIMAL thaw, freeze, mutWrite, mutRead, mutUnsafeWrite, mutUnsafeRead #-}
    type Collection c
    type MutableKey c
    type MutableValue c

    unsafeThaw   :: PrimMonad prim => Collection c -> prim (c (PrimState prim))
    unsafeThaw = thaw
    unsafeFreeze :: PrimMonad prim => c (PrimState prim) -> prim (Collection c)
    unsafeFreeze = freeze

    thaw   :: PrimMonad prim => Collection c -> prim (c (PrimState prim))
    freeze :: PrimMonad prim => c (PrimState prim) -> prim (Collection c)

    mutUnsafeWrite :: PrimMonad prim => c (PrimState prim) -> MutableKey c -> MutableValue c -> prim ()
    mutWrite       :: PrimMonad prim => c (PrimState prim) -> MutableKey c -> MutableValue c -> prim ()
    mutUnsafeRead :: PrimMonad prim => c (PrimState prim) -> MutableKey c -> prim (MutableValue c)
    mutRead       :: PrimMonad prim => c (PrimState prim) -> MutableKey c -> prim (MutableValue c)
