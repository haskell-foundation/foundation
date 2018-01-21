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
    , compatAndI#
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
#if __GLASGOW_HASKELL__ >= 800
import           GHC.IO
#endif

import           Basement.Compat.PrimTypes

--  GHC 8.0  | Base 4.9
--  GHC 7.10 | Base 4.8
--  GHC 7.8  | Base 4.7
--  GHC 7.6  | Base 4.6
--  GHC 7.4  | Base 4.5

-- | Flag record whether a specific byte array is pinned or not
data PinnedStatus = Pinned | Unpinned
    deriving (Prelude.Eq)

toPinnedStatus# :: Pinned# -> PinnedStatus
toPinnedStatus# 0# = Unpinned
toPinnedStatus# _  = Pinned

-- | turn an Int# into a Bool
--
-- Since GHC 7.8, boolean primitive don't return Bool but Int#.
#if MIN_VERSION_base(4,7,0)
bool# :: Int# -> Prelude.Bool
bool# v = isTrue# v
#else
bool# :: Prelude.Bool -> Prelude.Bool
bool# v = v
#endif
{-# INLINE bool# #-}

-- | A version friendly of andI#
compatAndI# :: Int# -> Int# -> Int#
#if !MIN_VERSION_base(4,7,0)
compatAndI# a b = word2Int# (and# (int2Word# a) (int2Word# b))
#else
compatAndI# = andI#
#endif
{-# INLINE compatAndI# #-}

-- | A version friendly of quotRemInt#
compatQuotRemInt# :: Int# -> Int# -> (# Int#, Int# #)
compatQuotRemInt# = quotRemInt#
{-# INLINE compatQuotRemInt# #-}

-- | A version friendly fo copyAddrToByteArray#
--
-- only available from GHC 7.8
compatCopyAddrToByteArray# :: Addr# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
#if MIN_VERSION_base(4,7,0)
compatCopyAddrToByteArray# = copyAddrToByteArray#
#else
compatCopyAddrToByteArray# addr ba ofs sz stini =
    loop ofs 0# stini
  where
    loop o i st
        | bool# (i ==# sz)  = st
        | Prelude.otherwise =
            case readWord8OffAddr# addr i st of
                (# st2, w #) -> loop (o +# 1#) (i +# 1#) (writeWord8Array# ba o w st2)
#endif
{-# INLINE compatCopyAddrToByteArray# #-}

-- | A version friendly fo copyByteArrayToAddr#
--
-- only available from GHC 7.8
compatCopyByteArrayToAddr# :: ByteArray# -> Int# -> Addr# -> Int# -> State# s -> State# s
#if MIN_VERSION_base(4,7,0)
compatCopyByteArrayToAddr# = copyByteArrayToAddr#
#else
compatCopyByteArrayToAddr# ba ofs addr sz stini =
    loop ofs 0# stini
  where
    loop o i st
        | bool# (i ==# sz)  = st
        | Prelude.otherwise =
            loop (o +# 1#) (i +# 1#) (writeWord8OffAddr# addr i (indexWord8Array# ba o) st)
#endif
{-# INLINE compatCopyByteArrayToAddr# #-}

-- | A mkWeak# version that keep working on 8.0
--
-- signature change in ghc-prim:
-- * 0.4: mkWeak# :: o -> b -> c                                             -> State# RealWorld -> (#State# RealWorld, Weak# b#)
-- * 0.5 :mkWeak# :: o -> b -> (State# RealWorld -> (#State# RealWorld, c#)) -> State# RealWorld -> (#State# RealWorld, Weak# b#)
--
compatMkWeak# :: o -> b -> Prelude.IO () -> State# RealWorld -> (#State# RealWorld, Weak# b #)
#if __GLASGOW_HASKELL__ >= 800
compatMkWeak# o b c s = mkWeak# o b (case c of { IO f -> f }) s
#else
compatMkWeak# o b c s = mkWeak# o b c s
#endif
{-# INLINE compatMkWeak# #-}

compatGetSizeofMutableByteArray# :: MutableByteArray# s -> State# s -> (#State# s, Int# #)
#if __GLASGOW_HASKELL__ >= 800
compatGetSizeofMutableByteArray# mba s = getSizeofMutableByteArray# mba s
#else
compatGetSizeofMutableByteArray# mba s = (# s, sizeofMutableByteArray# mba #)
#endif
{-# INLINE compatGetSizeofMutableByteArray# #-}

compatShrinkMutableByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
#if __GLASGOW_HASKELL__ >= 800
compatShrinkMutableByteArray# mba i s =
    case shrinkMutableByteArray# mba i s of { s2 -> (# s2, mba #) }
#else
compatShrinkMutableByteArray# src i s =
    -- not check whether i is smaller than the size of the buffer
    case newAlignedPinnedByteArray# i 8# s of { (# s2, dst #) ->
    case copyMutableByteArray# src 0# dst 0# i s2 of { s3 -> (# s3, dst #) }}
#endif
{-# INLINE compatShrinkMutableByteArray# #-}

--shrinkMutableByteArray# :: MutableByteArray# s -> Int# -> State# s -> State# s
compatResizeMutableByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
#if __GLASGOW_HASKELL__ >= 800
compatResizeMutableByteArray# mba i s = resizeMutableByteArray# mba i s
#else
compatResizeMutableByteArray# src i s =
    case newAlignedPinnedByteArray# i 8# s of { (# s2, dst #) ->
    case copyMutableByteArray# src 0# dst 0# nbBytes s2 of { s3 -> (# s3, dst #) }}
  where
    isGrow = bool# (i ># len)
    nbBytes
        | isGrow            = len
        | Prelude.otherwise = i
    !len = sizeofMutableByteArray# src
#endif
{-# INLINE compatResizeMutableByteArray# #-}

#if __GLASGOW_HASKELL__ >= 802
compatIsByteArrayPinned# :: ByteArray# -> Pinned#
compatIsByteArrayPinned# ba = isByteArrayPinned# ba

compatIsMutableByteArrayPinned# :: MutableByteArray# s -> Pinned#
compatIsMutableByteArrayPinned# ba = isMutableByteArrayPinned# ba
#else
foreign import ccall unsafe "foundation_is_bytearray_pinned"
    compatIsByteArrayPinned# :: ByteArray# -> Pinned#

foreign import ccall unsafe "foundation_is_bytearray_pinned"
    compatIsMutableByteArrayPinned# :: MutableByteArray# s -> Pinned#
#endif
