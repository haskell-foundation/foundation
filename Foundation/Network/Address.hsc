{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Foundation.Network.Address
    ( HostName(..)
    , HostNameInfo(..)
    , getHostNameInfo
    , getHostNameInfo_
    ) where

import Foundation.Class.Storable
import Foundation.Internal.Base
import Foundation.Internal.Proxy
import Foundation.Internal.Types
import Foundation.Hashing (Hashable)
import Foundation.String
import Foundation.Numerical
import Foundation.Network.IPv4

import Prelude (print)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr (nullPtr)

#include "netdb.h"

newtype HostName = HostName { toString :: String }
  deriving (Show, Eq, Ord, Typeable, Hashable)
instance IsString HostName where
    fromString = HostName . fromString

data HostNameInfo address_type = HostNameInfo
    { officialName :: !HostName
    , aliases      :: ![HostName]
    , addresses    :: ![address_type]
    } deriving (Show, Eq, Ord, Typeable)

getHostNameInfo :: StorableFixed IPv4
                => HostName
                -> IO (HostNameInfo IPv4)
getHostNameInfo = getHostNameInfo_ Proxy

getHostNameInfo_ :: StorableFixed IPv4
                 => Proxy address_type
                 -> HostName
                 -> IO (HostNameInfo IPv4)
getHostNameInfo_ p (HostName hn) =
    withCString (toList hn) $ \cname -> do
        let offname_ptr = (#ptr struct hostent, h_name)
        let aliases_ptr = (#ptr struct hostent, h_aliases)
        let addtype_ptr = (#ptr struct hostent, h_addrtype)
        let length_addr = (#ptr struct hostent, h_length)
        let addr_list   = (#ptr struct hostent, h_addr_list)
        ptr <- c_gethostbyname cname

        cptr <- peek $ castPtr (offname_ptr ptr)
        officialName <- getCString cptr

        alptr <- peek $ castPtr (aliases_ptr ptr) :: IO (Ptr (Ptr CChar))
        alptr' <- peek alptr
        aliases <- getAliases alptr'

        ty <- peek $ castPtr (addtype_ptr ptr) :: IO Int32
        len <- peek $ castPtr (length_addr ptr) :: IO Int32
        lptr <- peek $ castPtr (addr_list ptr) :: IO (Ptr (Ptr IPv4))
        lptr' <- peek lptr
        addresses <- getAddresses ty len lptr'
        return $ HostNameInfo (HostName officialName) aliases addresses

getCString :: Ptr CChar -> IO String
getCString ptr = fromList <$> peekCString ptr

getAliases :: Ptr CChar -> IO [HostName]
getAliases ptr
    | nullPtr == ptr = return []
    | otherwise = do
         x <- HostName <$> getCString ptr
         (:) x <$> getAliases (ptr `plusPtr` 1)

getAddresses :: Int32 -> Int32 -> Ptr IPv4 -> IO [IPv4]
getAddresses ty len ptr = do
    x <- peek ptr
    if x == fromTuple(0,0,0,0)
        then return []
        else (:) x <$> getAddresses ty len (ptr `plusPtr` 1)

-- | TODO: use the _r instead
foreign import ccall unsafe "gethostbyname"
    c_gethostbyname :: CString -> IO (Ptr Word8)
