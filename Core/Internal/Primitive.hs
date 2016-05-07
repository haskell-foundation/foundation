-- |
-- Module      : Core.Internal.Primitive
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}
module Core.Internal.Primitive
    ( bool#
    , andI#
    ) where

import qualified Prelude
import           GHC.Prim

-- | turn an Int# into a Bool
--
-- Since GHC 7.8, boolean primitive don't return Bool but Int#.
#if MIN_VERSION_base(4,7,0)
bool# :: Int# -> Prelude.Bool
bool# v = tagToEnum# v
#else
bool# :: Prelude.Bool -> Prelude.Bool
bool# v = v
#endif

#if !MIN_VERSION_base(4,7,0)
andI# :: Int# -> Int# -> Int#
andI# a b = word2Int# (and# (int2Word# a) (int2Word# b))
#endif
