{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}

module Foundation.Network.IPv4
    ( IPv4
    , fromString, toString
    , fromLString, toLString
    , fromTuple, toTuple
    ) where

import Prelude (fromIntegral)

import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String (withCString, CString)

import Foundation.Class.Storable
import Foundation.Internal.Base
import Foundation.Numerical
import Foundation.String (String)
import Foundation.Bits (htonl, ntohl)

newtype IPv4 = IPv4 Word32
  deriving (Eq, Ord, Typeable)
instance Show IPv4 where
    show = toLString
instance IsString IPv4 where
    fromString = fromLString . toList
instance Storable IPv4 where
    peek ptr = IPv4 . ntohl <$> peek (castPtr ptr)
    poke ptr (IPv4 w) = poke (castPtr ptr) (htonl w)

toString :: IPv4 -> String
toString = fromList . toLString

fromLString :: [Char] -> IPv4
fromLString str = unsafePerformIO $ withCString str $ \cstr ->
    IPv4 . ntohl <$> c_inet_addr cstr

toLString :: IPv4 -> [Char]
toLString ipv4 =
    let (i1, i2, i3, i4) = toTuple ipv4
     in show i1 <> "." <> show i2 <> "." <> show i3 <> "." <> show i4

fromTuple :: (Word8, Word8, Word8, Word8) -> IPv4
fromTuple (i1, i2, i3, i4) =
     IPv4 $ (((((f i1 * 256) + f i2) * 256) + f i3) * 256) + f i4
  where
    f = fromIntegral

toTuple :: IPv4 -> (Word8, Word8, Word8, Word8)
toTuple (IPv4 n4) =
    let (i4, n3) = n4 `divMod` 256
        (i3, n2) = n3 `divMod` 256
        (i2, n1) = n2 `divMod` 256
        i1       = n1 `mod` 256
     in (f i1, f i2, f i3, f i4)
  where
    f = fromIntegral

foreign import ccall unsafe "inet_addr"
    c_inet_addr :: CString -> IO Word32
