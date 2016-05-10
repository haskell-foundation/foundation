-- |
-- Module      : Core.Vector
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- Simple Vector and Almost-Vector-like data structure
--
-- Generally accessible in o(1)
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Core.Vector
    ( Vector
    , MVector
    , UVector
    , MUVector
    -- exceptions
    , OutOfBound
    ) where

import           Core.Vector.Common
import           Core.Vector.Boxed
import           Core.Vector.Unboxed
