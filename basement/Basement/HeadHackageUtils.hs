{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
module Basement.HeadHackageUtils where

import GHC.Exts

#if MIN_VERSION_base(4,16,0)
int8ToIntCompat# :: Int8# -> Int#
int8ToIntCompat# = int8ToInt#

int16ToIntCompat# :: Int16# -> Int#
int16ToIntCompat# = int16ToInt#

int32ToIntCompat# :: Int32# -> Int#
int32ToIntCompat# = int32ToInt#

word8ToWordCompat# :: Word8# -> Word#
word8ToWordCompat# = word8ToWord#

word16ToWordCompat# :: Word16# -> Word#
word16ToWordCompat# = word16ToWord#

word32ToWordCompat# :: Word32# -> Word#
word32ToWordCompat# = word32ToWord#

intToInt8Compat# :: Int# -> Int8#
intToInt8Compat# = intToInt8#

intToInt16Compat# :: Int# -> Int16#
intToInt16Compat# = intToInt16#

intToInt32Compat# :: Int# -> Int32#
intToInt32Compat# = intToInt32#

wordToWord8Compat# :: Word# -> Word8#
wordToWord8Compat# = wordToWord8#

wordToWord16Compat# :: Word# -> Word16#
wordToWord16Compat# = wordToWord16#

wordToWord32Compat# :: Word# -> Word32#
wordToWord32Compat# = wordToWord32#

--

narrow8IntCompat# :: Int# -> Int8#
narrow8IntCompat# = intToInt8#

narrow16IntCompat# :: Int# -> Int16#
narrow16IntCompat# = intToInt16#

narrow32IntCompat# :: Int# -> Int32#
narrow32IntCompat# = intToInt32#

narrow8WordCompat# :: Word# -> Word8#
narrow8WordCompat# = wordToWord8#

narrow16WordCompat# :: Word# -> Word16#
narrow16WordCompat# = wordToWord16#

narrow32WordCompat# :: Word# -> Word32#
narrow32WordCompat# = wordToWord32#
#else
-- No-ops
int8ToIntCompat# :: Int# -> Int#
int8ToIntCompat# x = x

int16ToIntCompat# :: Int# -> Int#
int16ToIntCompat# x = x

int32ToIntCompat# :: Int# -> Int#
int32ToIntCompat# x = x

word8ToWordCompat# :: Word# -> Word#
word8ToWordCompat# x = x

word16ToWordCompat# :: Word# -> Word#
word16ToWordCompat# x = x

word32ToWordCompat# :: Word# -> Word#
word32ToWordCompat# x = x

intToInt8Compat# :: Int# -> Int#
intToInt8Compat# x = x

intToInt16Compat# :: Int# -> Int#
intToInt16Compat# x = x

intToInt32Compat# :: Int# -> Int#
intToInt32Compat# x = x

wordToWord8Compat# :: Word# -> Word#
wordToWord8Compat# x = x

wordToWord16Compat# :: Word# -> Word#
wordToWord16Compat# x = x

wordToWord32Compat# :: Word# -> Word#
wordToWord32Compat# x = x

-- Actual narrowing
narrow8IntCompat# :: Int# -> Int#
narrow8IntCompat# = narrow8Int#

narrow16IntCompat# :: Int# -> Int#
narrow16IntCompat# = narrow16Int#

narrow32IntCompat# :: Int# -> Int#
narrow32IntCompat# = narrow32Int#

narrow8WordCompat# :: Word# -> Word#
narrow8WordCompat# = narrow8Word#

narrow16WordCompat# :: Word# -> Word#
narrow16WordCompat# = narrow16Word#

narrow32WordCompat# :: Word# -> Word#
narrow32WordCompat# = narrow32Word#
#endif
