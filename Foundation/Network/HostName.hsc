-- |
-- Module      : Foundation.Network.HostName
-- License     : BSD-style
-- Maintainer  : Nicolas Di Prima <nicolas@primetype.co.uk>
-- Stability   : experimental
-- Portability : portable
--
-- HostName and HostName info
--
-- > getHostNameInfo "github.com" :: IO (HostNameInfo IPv4)
--
-- > getHostNameInfo "google.com" :: IO (HostNameInfo IPv6)
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Foundation.Network.HostName
    ( HostName(..)
    , HostNameInfo(..)
    , getHostNameInfo
    , getHostNameInfo_
    ) where

import Foundation.Class.Storable
import Foundation.Internal.Base
import Foundation.Internal.Proxy
import Foundation.Hashing (Hashable)
import Foundation.String
import Foundation.Network.Address
import Foundation.Array
import Foundation.Collection.Mapable

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr (nullPtr)
import Control.Concurrent.MVar
import System.IO.Unsafe (unsafePerformIO)

import Control.Monad ((=<<))

#ifdef mingw32_HOST_OS
#include <winsock2.h>
#else
#include "netinet/in.h"
#include "netdb.h"
#endif

-- | HostName
--
newtype HostName = HostName { toString :: String }
  deriving (Eq, Ord, Typeable, Hashable)
instance Show HostName where
    show = show . toString
instance IsString HostName where
    fromString = HostName . fromString

-- | HostName's Info
data HostNameInfo address_type = HostNameInfo
    { officialName :: !HostName
        -- ^ official names
    , aliases      :: !(Array HostName)
        -- ^ known aliases
    , addresses    :: !(Array address_type)
        -- ^ known addresses
    } deriving (Show, Eq, Ord, Typeable)

-- | HostName errors
data HostNameError
    = HostNotFound !HostName
        -- ^ the given HostMame was not found
    | NoAssociatedData !HostName
        -- ^ there is not associated info/data to the given HostName
        --
        -- i.e. : no IPv4 info? This might mean you should try IPv6 ?
    | FatalError
        -- ^ getHostNameInfo uses *C* a binding to get the `HostNameInfo`
        --
        -- a fatal error is linked to the underlying *C* function and is not
        -- recoverable.
    | UnknownError !CInt
        -- ^ Unknown Error, `CInt` is the associated error code.
        --
        -- see man gethostbyname for more information
  deriving (Show,Eq,Typeable)

instance Exception HostNameError

-- | get `HostName` info:
--
-- retrieve the official name, the aliases and the addresses associated to this
-- hostname.
--
-- For cross-platform compatibility purpose, this function is using a *C* non
-- re-entrant function `gethostbyname2`. This function is using a `MVar ()` to
-- avoid a race condition and should be safe to use.
--
getHostNameInfo :: (Show address_type, Eq address_type, Address address_type)
                => HostName
                -> IO (HostNameInfo address_type)
getHostNameInfo = getHostNameInfo_ Proxy

globalMutex :: MVar ()
globalMutex = unsafePerformIO (newMVar ())
{-# NOINLINE globalMutex #-}

-- | like `getHostNameInfo` but takes a `Proxy` to help with the type checker.
getHostNameInfo_ :: (Show address_type, Eq address_type, Address address_type)
                 => Proxy address_type
                 -> HostName
                 -> IO (HostNameInfo address_type)
getHostNameInfo_ p h@(HostName hn) =
    withMVar globalMutex $ \_ ->
    withCString (toList hn) $ \cname -> do
        ptr <- loop $ c_gethostbyname2 cname (familyCode p)

        on <- peekHostName . castPtr =<< peek (castPtr $ offname_ptr ptr)

        as <- getAliases . castPtr =<< peek (castPtr $ aliases_ptr ptr)

        addrs <- getAddresses p . castPtr =<< peek (castPtr $ addr_list ptr)
        return $ HostNameInfo on as addrs
  where
    loop f = do
        ptr <- f
        if ptr /= nullPtr
            then return ptr
            else do
                err <- c_get_h_errno
                case err of
                    _ | err == (#const NO_DATA)        -> throwIO $ NoAssociatedData h
                      | err == (#const HOST_NOT_FOUND) -> throwIO $ HostNotFound h
                      | err == (#const TRY_AGAIN)      -> loop f
                      | err == (#const NO_RECOVERY)    -> throwIO FatalError
                      | otherwise                      -> throwIO $ UnknownError err
    offname_ptr = (#ptr struct hostent, h_name)
    aliases_ptr = (#ptr struct hostent, h_aliases)
    addr_list   = (#ptr struct hostent, h_addr_list)

peekHostName :: Ptr Word8 -> IO HostName
peekHostName ptr = HostName . fst . fromBytesLenient <$> peekArrayEndedBy 0x00 ptr

getAliases :: Ptr (Ptr Word8) -> IO (Array HostName)
getAliases ptr = do
    arr <- peekArrayEndedBy nullPtr ptr
    forM arr peekHostName

getAddresses :: Storable address_type
             => Proxy address_type
             -> Ptr (Ptr address_type)
             -> IO (Array address_type)
getAddresses _ ptr = do
    arr <- peekArrayEndedBy nullPtr ptr
    forM arr peek

foreign import ccall unsafe "gethostbyname2"
    c_gethostbyname2 :: CString -> CInt -> IO (Ptr Word8)
foreign import ccall unsafe "get_h_errno"
    c_get_h_errno :: IO CInt
