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
    , Offset#
    , Size#
    , compatAndI#
    , compatQuotRemInt#
    , compatCopyAddrToByteArray#
    , Word(..)
    ) where

import qualified Prelude
import           GHC.Prim
import           GHC.Word

--  GHC 8.0  | Base 4.9
--  GHC 7.10 | Base 4.8
--  GHC 7.8  | Base 4.7
--  GHC 7.6  | Base 4.6
--  GHC 7.4  | Base 4.5

-- | Offset in a bytearray, string, type alias
--
-- for code documentation purpose only, just a simple type alias on Int#
type Offset# = Int#

-- | Size in bytes type alias
--
-- for code documentation purpose only, just a simple type alias on Int#
type Size# = Int#

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
#if !MIN_VERSION_base(4,6,0)
compatQuotRemInt# a b = (# quotInt# a b, remInt# a b #)
#else
compatQuotRemInt# = quotRemInt#
#endif
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
