{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}

module Foundation.Network.IPv4
    ( IPv4
    , fromString, toString
    , fromLString, toLString
    , fromTuple, toTuple
    , htonl, ntohl
    ) where

import Prelude (fromIntegral)

import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String (withCString, CString)

import Foundation.Class.Storable
import Foundation.Internal.Base
import Foundation.Numerical
import Foundation.String (String)

newtype IPv4 = IPv4 Word32
  deriving (Eq, Ord, Typeable)
instance Show IPv4 where
    show = toLString
instance IsString IPv4 where
    fromString = fromLString . toList
instance Storable IPv4 where
    peek ptr = IPv4 <$> peek (castPtr ptr)
    poke ptr (IPv4 w) = poke (castPtr ptr) w

toString :: IPv4 -> String
toString = fromList . toLString

fromLString :: [Char] -> IPv4
fromLString str = unsafePerformIO $ withCString str $ \cstr ->
    IPv4 <$> c_inet_addr cstr

toLString :: IPv4 -> [Char]
toLString ipv4 =
    let (i1, i2, i3, i4) = toTuple ipv4
     in show i1 <> "." <> show i2 <> "." <> show i3 <> "." <> show i4

fromTuple :: (Word8, Word8, Word8, Word8) -> IPv4
fromTuple (i1, i2, i3, i4) =
     htonl $ (((((f i1 * 256) + f i2) * 256) + f i3) * 256) + f i4
  where
    f = fromIntegral

toTuple :: IPv4 -> (Word8, Word8, Word8, Word8)
toTuple ipv4 =
    let n = ntohl ipv4
        i4 =  n                          `mod` 256
        i3 = (n `div`  256)              `mod` 256
        i2 = (n `div` (256 * 256))       `mod` 256
        i1 = (n `div` (256 * 256 * 256)) `mod` 256
     in (f i1, f i2, f i3, f i4)
  where
    f = fromIntegral

htonl :: Word32 -> IPv4
htonl w = unsafePerformIO (IPv4 <$> c_htonl w)
ntohl :: IPv4 -> Word32
ntohl (IPv4 w) = unsafePerformIO $ c_ntohl w

foreign import ccall unsafe "htonl"
    c_htonl :: Word32 -> IO Word32
foreign import ccall unsafe "ntohl"
    c_ntohl :: Word32 -> IO Word32
foreign import ccall unsafe "inet_addr"
    c_inet_addr :: CString -> IO Word32
