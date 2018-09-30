-- |
-- Module      : Basement.Compat.Primitive
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Basement.Compat.Primitive
    ( bool#
    , PinnedStatus(..), toPinnedStatus#
    , compatQuotRemInt#
    , compatCopyAddrToByteArray#
    , compatCopyByteArrayToAddr#
    , compatMkWeak#
    , compatGetSizeofMutableByteArray#
    , compatShrinkMutableByteArray#
    , compatResizeMutableByteArray#
    , compatIsByteArrayPinned#
    , compatIsMutableByteArrayPinned#
    , Word(..)
    ) where

import qualified Prelude
import           GHC.Exts
import           GHC.Prim
import           GHC.Word
import           GHC.IO

import           Basement.Compat.PrimTypes

--  GHC 8.4  | Base 4.11
--  GHC 8.2  | Base 4.10
--  GHC 8.0  | Base 4.9
--  GHC 7.10 | Base 4.8
--  GHC 7.8  | Base 4.7
--  GHC 7.6  | Base 4.6
--  GHC 7.4  | Base 4.5
--
--  More complete list:
--  https://wiki.haskell.org/Base_package

-- | Flag record whether a specific byte array is pinned or not
data PinnedStatus = Pinned | Unpinned
    deriving (Prelude.Eq)

toPinnedStatus# :: Pinned# -> PinnedStatus
toPinnedStatus# 0# = Unpinned
toPinnedStatus# _  = Pinned

-- | turn an Int# into a Bool
bool# :: Int# -> Prelude.Bool
bool# v = isTrue# v
{-# INLINE bool# #-}

-- | A version friendly of quotRemInt#
compatQuotRemInt# :: Int# -> Int# -> (# Int#, Int# #)
compatQuotRemInt# = quotRemInt#
{-# INLINE compatQuotRemInt# #-}

-- | A version friendly fo copyAddrToByteArray#
--
-- only available from GHC 7.8
compatCopyAddrToByteArray# :: Addr# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
compatCopyAddrToByteArray# = copyAddrToByteArray#
{-# INLINE compatCopyAddrToByteArray# #-}

-- | A version friendly fo copyByteArrayToAddr#
--
-- only available from GHC 7.8
compatCopyByteArrayToAddr# :: ByteArray# -> Int# -> Addr# -> Int# -> State# s -> State# s
compatCopyByteArrayToAddr# = copyByteArrayToAddr#
{-# INLINE compatCopyByteArrayToAddr# #-}

-- | A mkWeak# version that keep working on 8.0
--
-- signature change in ghc-prim:
-- * 0.4: mkWeak# :: o -> b -> c                                             -> State# RealWorld -> (#State# RealWorld, Weak# b#)
-- * 0.5 :mkWeak# :: o -> b -> (State# RealWorld -> (#State# RealWorld, c#)) -> State# RealWorld -> (#State# RealWorld, Weak# b#)
--
compatMkWeak# :: o -> b -> Prelude.IO () -> State# RealWorld -> (#State# RealWorld, Weak# b #)
compatMkWeak# o b c s = mkWeak# o b (case c of { IO f -> f }) s
{-# INLINE compatMkWeak# #-}

compatGetSizeofMutableByteArray# :: MutableByteArray# s -> State# s -> (#State# s, Int# #)
compatGetSizeofMutableByteArray# mba s = getSizeofMutableByteArray# mba s
{-# INLINE compatGetSizeofMutableByteArray# #-}

compatShrinkMutableByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
compatShrinkMutableByteArray# mba i s =
    case shrinkMutableByteArray# mba i s of { s2 -> (# s2, mba #) }
{-# INLINE compatShrinkMutableByteArray# #-}

--shrinkMutableByteArray# :: MutableByteArray# s -> Int# -> State# s -> State# s
compatResizeMutableByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
compatResizeMutableByteArray# mba i s = resizeMutableByteArray# mba i s
{-# INLINE compatResizeMutableByteArray# #-}

#if __GLASGOW_HASKELL__ >= 802
compatIsByteArrayPinned# :: ByteArray# -> Pinned#
compatIsByteArrayPinned# ba = isByteArrayPinned# ba

compatIsMutableByteArrayPinned# :: MutableByteArray# s -> Pinned#
compatIsMutableByteArrayPinned# ba = isMutableByteArrayPinned# ba
#else
foreign import ccall unsafe "basement_is_bytearray_pinned"
    compatIsByteArrayPinned# :: ByteArray# -> Pinned#

foreign import ccall unsafe "basement_is_bytearray_pinned"
    compatIsMutableByteArrayPinned# :: MutableByteArray# s -> Pinned#
#endif
