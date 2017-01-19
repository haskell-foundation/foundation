-- Extra bits stuff
module Foundation.Bits
    ( (.<<.)
    , (.>>.)
    , Bits(..)
    , alignRoundUp
    , alignRoundDown
    , htonl, htons
    , ntohl, ntohs
    ) where

import Foundation.Internal.Base
import Foundation.Internal.ByteSwap
import Foundation.Numerical
import Foundation.System.Info (Endianness(..), endianness)
import Data.Bits
import Data.Word (Word16, Word32)

-- | Unsafe Shift Left Operator
(.<<.) :: Bits a => a -> Int -> a
(.<<.) = unsafeShiftL

-- | Unsafe Shift Right Operator
(.>>.) :: Bits a => a -> Int -> a
(.>>.) = unsafeShiftR

-- | Round up (if needed) to a multiple of @alignment@ closst to @m@
--
-- @alignment@ needs to be a power of two
--
-- alignRoundUp 16 8 = 16
-- alignRoundUp 15 8 = 16
alignRoundUp :: Int -- ^ Number to Align
             -> Int -- ^ Alignment (power of 2)
             -> Int
alignRoundUp m alignment = (m + (alignment-1)) .&. complement (alignment-1)

-- | Round down (if needed) to a multiple of @alignment@ closest to @m@
--
-- @alignment@ needs to be a power of two
--
-- > alignRoundDown 15 8 = 8
-- > alignRoundDown 8 8  = 8
alignRoundDown :: Int -- ^ Number to Align
               -> Int -- ^ Alignment (power of 2)
               -> Int
alignRoundDown m alignment = m .&. complement (alignment-1)

-- | perform conversion from Host to Network endianness
htons :: Word16 -> Word16
htons w = case endianness of
    LittleEndian -> byteSwap16 w
    BigEndian    -> w

-- | perform conversion from Network to Host endianness
ntohs :: Word16 -> Word16
ntohs w = case endianness of
    LittleEndian -> byteSwap16 w
    BigEndian    -> w

-- | perform conversion from Host to Network endianness
htonl :: Word32 -> Word32
htonl w = case endianness of
    LittleEndian -> byteSwap32 w
    BigEndian    -> w

-- | perform conversion from Network to Host endianness
ntohl :: Word32 -> Word32
ntohl w = case endianness of
    LittleEndian -> byteSwap32 w
    BigEndian    -> w
