{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns  #-}
module Foundation.Primitive.Base16
    ( unsafeConvertByte
    ) where

import GHC.Prim

-- | Convert a byte value in Word# to two Word#s containing
-- the hexadecimal representation of the Word#
--
-- The output words# are guaranteed to be included in the 0 to 2^7-1 range
--
-- Note that calling convertByte with a value greater than 256
-- will cause segfault or other horrible effect.
unsafeConvertByte :: Word# -> (# Word#, Word# #)
unsafeConvertByte b = (# r tableHi b, r tableLo b #)
  where
    r :: Table -> Word# -> Word#
    r (Table !table) index = indexWord8OffAddr# table (word2Int# index)
{-# INLINE unsafeConvertByte #-}

data Table = Table Addr#

tableLo:: Table
tableLo = Table
    "0123456789abcdef0123456789abcdef\
    \0123456789abcdef0123456789abcdef\
    \0123456789abcdef0123456789abcdef\
    \0123456789abcdef0123456789abcdef\
    \0123456789abcdef0123456789abcdef\
    \0123456789abcdef0123456789abcdef\
    \0123456789abcdef0123456789abcdef\
    \0123456789abcdef0123456789abcdef"#

tableHi :: Table
tableHi = Table
    "00000000000000001111111111111111\
    \22222222222222223333333333333333\
    \44444444444444445555555555555555\
    \66666666666666667777777777777777\
    \88888888888888889999999999999999\
    \aaaaaaaaaaaaaaaabbbbbbbbbbbbbbbb\
    \ccccccccccccccccdddddddddddddddd\
    \eeeeeeeeeeeeeeeeffffffffffffffff"#

