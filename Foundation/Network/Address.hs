{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Foundation.Network.Address
    ( HostName
    , HostNameInfo(..)
    , getHostNameInfo
    , getHostNameInfo_
    ) where

import Foundation.Class.Storable
import Foundation.Internal.Base
import Foundation.Internal.Proxy
import Foundation.Hashing (Hashable)
import Foundation.String
import Foundation.Network.Socket.Address

newtype HostName = HostName { toString :: String }
  deriving (Show, Eq, Ord, Typeable, Hashable)
instance IsString HostName where
    fromString = HostName . fromString -- TODO: some validation here maybe ?

data HostNameInfo address_type = HostNameInfo
    { officialName :: !HostName
    , aliases      :: ![HostName]
    , addresses    :: ![address_type]
    } deriving (Show, Eq, Ord, Typeable)

getHostNameInfo :: StorableFixed address_type
                => HostName
                -> IO (HostNameInfo address_type)
getHostNameInfo = getHostNameInfo_ Proxy

getHostNameInfo_ :: StorableFixed address_type
                 => Proxy address_type
                 -> HostName
                 -> IO (HostNameInfo address_type)
getHostNameInfo_ p (HostName hn)= undefined
