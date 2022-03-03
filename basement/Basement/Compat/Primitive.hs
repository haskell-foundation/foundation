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
    , compatMkWeak#
    , compatIsByteArrayPinned#
    , compatIsMutableByteArrayPinned#
    , unsafeCoerce#
    , Word(..)
    , Word8#
    , Word16#
    , Word32#
    , Int8#
    , Int16#
    , Int32#
    -- word upper sizing
    , word8ToWord16#
    , word8ToWord32#
    , word8ToWord#
    , word16ToWord8#
    , word16ToWord32#
    , word16ToWord#
    , word32ToWord#
    -- word down sizing
    , word32ToWord8#
    , word32ToWord16#
    , wordToWord32#
    , wordToWord16#
    , wordToWord8#
    -- int upper sizing
    , int8ToInt16#
    , int8ToInt32#
    , int8ToInt#
    , int16ToInt32#
    , int16ToInt#
    , int32ToInt#
    -- int down sizing
    , intToInt8#
    , intToInt16#
    , intToInt32#
    -- other
    , word8ToInt#
    , word8ToInt16#
    , word8ToInt32#
    , charToWord32#
    , word8ToChar#
    , word16ToChar#
    , word32ToChar#
    , wordToChar#

    -- word8 ops
    , plusWord8#
    -- word16 ops
    , uncheckedShiftRLWord16#
    , plusWord16#
    -- word32 ops
    , uncheckedShiftRLWord32#
    , plusWord32#
    -- int8 ops
    , plusInt8#
    -- int16 ops
    , plusInt16#
    -- int32 ops
    , plusInt32#
    ) where


import qualified Prelude
import           GHC.Exts hiding (Word8#, Word16#, Word32#, Int8#, Int16#, Int32#, plusWord8#, plusWord16#, plusInt8#, plusInt16#)
import           GHC.Prim hiding (Word8#, Word16#, Word32#, Int8#, Int16#, Int32#, plusWord8#, plusWord16#, plusInt8#, plusInt16#)
import           GHC.Word
import           GHC.IO

import           Basement.Compat.PrimTypes

#if __GLASGOW_HASKELL__ >= 902
import           GHC.Exts (Word8#, Word16#, Word32#, Int8#, Int16#, Int32#, plusWord8#, plusWord16#, plusInt8#, plusInt16#)
#endif

--  GHC 9.2  | Base 4.16
--  GHC 9.0  | Base 4.15
--  GHC 8.8  | Base 4.13 4.14
--  GHC 8.6  | Base 4.12
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

-- | A mkWeak# version that keep working on 8.0
--
-- signature change in ghc-prim:
-- * 0.4: mkWeak# :: o -> b -> c                                             -> State# RealWorld -> (#State# RealWorld, Weak# b#)
-- * 0.5 :mkWeak# :: o -> b -> (State# RealWorld -> (#State# RealWorld, c#)) -> State# RealWorld -> (#State# RealWorld, Weak# b#)
--
compatMkWeak# :: o -> b -> Prelude.IO () -> State# RealWorld -> (#State# RealWorld, Weak# b #)
compatMkWeak# o b c s = mkWeak# o b (case c of { IO f -> f }) s
{-# INLINE compatMkWeak# #-}

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

#if __GLASGOW_HASKELL__ >= 902

word8ToWord16# :: Word8# -> Word16#
word8ToWord16# a = wordToWord16# (word8ToWord# a)

word8ToWord32# :: Word8# -> Word32#
word8ToWord32# a = wordToWord32# (word8ToWord# a)

word16ToWord8# :: Word16# -> Word8#
word16ToWord8# a = wordToWord8# (word16ToWord# a)

word16ToWord32# :: Word16# -> Word32#
word16ToWord32# a = wordToWord32# (word16ToWord# a)

word32ToWord8# :: Word32# -> Word8#
word32ToWord8# a = wordToWord8# (word32ToWord# a)

word32ToWord16# :: Word32# -> Word16#
word32ToWord16# a = wordToWord16# (word32ToWord# a)

int8ToInt16# :: Int8# -> Int16#
int8ToInt16# i = intToInt16# (int8ToInt# i)

int8ToInt32# :: Int8# -> Int32#
int8ToInt32# i = intToInt32# (int8ToInt# i)

int16ToInt32# :: Int16# -> Int32#
int16ToInt32# i = intToInt32# (int16ToInt# i)

word8ToInt16# :: Word8# -> Int16#
word8ToInt16# i = intToInt16# (word2Int# (word8ToWord# i))

word8ToInt32# :: Word8# -> Int32#
word8ToInt32# i = intToInt32# (word2Int# (word8ToWord# i))

word8ToInt# :: Word8# -> Int#
word8ToInt# i = word2Int# (word8ToWord# i)

charToWord32# :: Char# -> Word32#
charToWord32# ch = wordToWord32# (int2Word# (ord# ch))

word8ToChar# :: Word8# -> Char#
word8ToChar# ch = chr# (word2Int# (word8ToWord# ch))

word16ToChar# :: Word16# -> Char#
word16ToChar# ch = chr# (word2Int# (word16ToWord# ch))

word32ToChar# :: Word32# -> Char#
word32ToChar# ch = chr# (word2Int# (word32ToWord# ch))

wordToChar# :: Word# -> Char#
wordToChar# ch = chr# (word2Int# ch)

#else
type Word8# = Word#
type Word16# = Word#
type Word32# = Word#

type Int8# = Int#
type Int16# = Int#
type Int32# = Int#

word8ToWord16# :: Word8# -> Word16#
word8ToWord16# a = a

word8ToWord32# :: Word8# -> Word32#
word8ToWord32# a = a

word8ToWord# :: Word8# -> Word#
word8ToWord# a = a

word16ToWord32# :: Word16# -> Word32#
word16ToWord32# a = a

word16ToWord8# :: Word16# -> Word8#
word16ToWord8# w = narrow8Word# w

word16ToWord# :: Word16# -> Word#
word16ToWord# a = a

word32ToWord8# :: Word32# -> Word8#
word32ToWord8# w = narrow8Word# w

word32ToWord16# :: Word32# -> Word16#
word32ToWord16# w = narrow16Word# w

word32ToWord# :: Word32# -> Word#
word32ToWord# a = a

wordToWord32# :: Word# -> Word32#
wordToWord32# w = narrow32Word# w

wordToWord16# :: Word# -> Word16#
wordToWord16# w = narrow16Word# w

wordToWord8# :: Word# -> Word8#
wordToWord8# w = narrow8Word# w

charToWord32# :: Char# -> Word32#
charToWord32# ch = int2Word# (ord# ch)

word8ToInt16# :: Word8# -> Int16#
word8ToInt16# w = word2Int# w

word8ToInt32# :: Word8# -> Int32#
word8ToInt32# w = word2Int# w

word8ToInt# :: Word8# -> Int#
word8ToInt# w = word2Int# w

word8ToChar# :: Word8# -> Char#
word8ToChar# w = chr# (word2Int# w)

word16ToChar# :: Word16# -> Char#
word16ToChar# w = chr# (word2Int# w)

word32ToChar# :: Word32# -> Char#
word32ToChar# w = chr# (word2Int# w)

wordToChar# :: Word# -> Char#
wordToChar# ch = chr# (word2Int# ch)

int8ToInt16# :: Int8# -> Int16#
int8ToInt16# a = a

int8ToInt32# :: Int8# -> Int32#
int8ToInt32# a = a

int8ToInt# :: Int8# -> Int#
int8ToInt# a = a

int16ToInt32# :: Int16# -> Int32#
int16ToInt32# a = a

int16ToInt# :: Int16# -> Int#
int16ToInt# a = a

int32ToInt# :: Int32# -> Int#
int32ToInt# a = a

intToInt8# :: Int# -> Int8#
intToInt8# i = narrow8Int# i

intToInt16# :: Int# -> Int16#
intToInt16# i = narrow16Int# i

intToInt32# :: Int# -> Int32#
intToInt32# i = narrow32Int# i

uncheckedShiftRLWord16# = uncheckedShiftRL#

uncheckedShiftRLWord32# = uncheckedShiftRL#

plusWord8# :: Word8# -> Word8# -> Word8#
plusWord8# a b = narrow8Word# (plusWord# a b)

plusWord16# :: Word16# -> Word16# -> Word16#
plusWord16# a b = narrow16Word# (plusWord# a b)

plusWord32# :: Word32# -> Word32# -> Word32#
plusWord32# a b = narrow32Word# (plusWord# a b)

plusInt8# :: Int8# -> Int8# -> Int8#
plusInt8# a b = narrow8Int# (a +# b)

plusInt16# :: Int16# -> Int16# -> Int16#
plusInt16# a b = narrow16Int# (a +# b)

plusInt32# :: Int32# -> Int32# -> Int32#
plusInt32# a b = narrow32Int# (a +# b)

#endif
