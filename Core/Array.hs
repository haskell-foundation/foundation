-- |
-- Module      : Core.Array
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- Simple Array and Almost-Array-like data structure
--
-- Generally accessible in o(1)
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Core.Array
    ( Array
    , MArray
    , UArray
    , MUArray
    , PrimType
    , ByteArray
    , MutableByteArray
    -- exceptions
    , OutOfBound
    ) where

import           Core.Array.Common
import           Core.Array.Boxed
import           Core.Array.Unboxed
import           Core.Array.Unboxed.Mutable
import           Core.Array.Unboxed.ByteArray
