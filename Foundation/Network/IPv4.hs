{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Foundation.Network.IPv4
    ( IPv4
    , fromString, toString
    , fromTuple, toTuple
    ) where

import Prelude (fromIntegral)

import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String (withCString, CString)

import Foundation.Class.Storable
import Foundation.Hashing.Hashable
import Foundation.Internal.Base
import Foundation.Internal.Proxy
import Foundation.String (String)
import Foundation.Primitive
import Foundation.Bits

-- | IPv4 data type
newtype IPv4 = IPv4 Word32
  deriving (Eq, Ord, Typeable, Hashable)
instance Show IPv4 where
    show = toLString
instance IsString IPv4 where
    fromString = fromLString
instance Storable IPv4 where
    peek ptr = IPv4 . fromBE <$> peek (castPtr ptr)
    poke ptr (IPv4 w) = poke (castPtr ptr) (toBE w)
instance StorableFixed IPv4 where
    size      _ = size      (Proxy :: Proxy Word32)
    alignment _ = alignment (Proxy :: Proxy Word32)

toString :: IPv4 -> String
toString = fromList . toLString

fromLString :: [Char] -> IPv4
fromLString str = unsafePerformIO $ withCString str $ \cstr ->
    IPv4 . fromBE <$> c_inet_addr cstr

toLString :: IPv4 -> [Char]
toLString ipv4 =
    let (i1, i2, i3, i4) = toTuple ipv4
     in show i1 <> "." <> show i2 <> "." <> show i3 <> "." <> show i4

fromTuple :: (Word8, Word8, Word8, Word8) -> IPv4
fromTuple (i1, i2, i3, i4) =
     IPv4 $     (w1 .<<. 24) .&. 0xFF000000
            .|. (w2 .<<. 16) .&. 0x00FF0000
            .|. (w3 .<<.  8) .&. 0x0000FF00
            .|.  w4          .&. 0x000000FF
  where
    f = fromIntegral
    w1, w2, w3, w4 :: Word32
    w1 = f i1
    w2 = f i2
    w3 = f i3
    w4 = f i4

toTuple :: IPv4 -> (Word8, Word8, Word8, Word8)
toTuple (IPv4 w) =
    (f w1, f w2, f w3, f w4)
  where
    f = fromIntegral
    w1, w2, w3, w4 :: Word32
    w1 = w .>>. 24 .&. 0x000000FF
    w2 = w .>>. 16 .&. 0x000000FF
    w3 = w .>>.  8 .&. 0x000000FF
    w4 = w         .&. 0x000000FF


foreign import ccall unsafe "inet_addr"
    c_inet_addr :: CString -> IO (BE Word32)
