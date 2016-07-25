-- |
-- Module      : Core.String.Encoding.UTF16
-- License     : BSD-style
-- Maintainer  : Foundation
-- Stability   : experimental
-- Portability : portable
--
{-# LANGUAGE UnboxedTuples #-}
module Core.String.Encoding.UTF16
    ( write
    , next
    ) where

import Core.Internal.Base
import Core.Internal.Primitive
import Core.Internal.Types
import Core.Primitive.Types
import Core.Primitive.Monad
import Core.Array.Unboxed
import Core.Array.Unboxed.Mutable (MUArray)

next :: UArray Word16 -> Offset Word16 -> (# Char, Offset Word16 #)
next ba (Offset n) =
    (# 'a', Offset n #)

write :: PrimMonad prim => MUArray Word16 (PrimState prim) -> Offset Word16 -> Char -> prim (Offset Word16)
write mba idx@(Offset i) c =
    return idx
