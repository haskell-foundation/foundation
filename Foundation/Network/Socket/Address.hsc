-- |
-- Module      : Foundation.Network.Socket.Address
-- License     : BSD-style
-- Maintainer  : Nicolas Di Prima <nicolas@primetype.co.uk>
-- Stability   : experimental
-- Portability : portable
--
-- SocketAddress and low level network address types
--
-- A network socket is constrained by its underlying domain, protocols and types.
-- Here, we are not trying to be 100% accurate, we are trying to bring together the
-- known constraints.
--
-- Inet6 and Inet are socket using respectively IPv6 and IPv6 addresses.
--
-- Network Protocols and Network Types are also linked together based on:
--
-- * UDP -> Datagram network type;
-- * TCP -> Stream network type.
--
-- Here, using strong types, type family and associated types we are providing a safe,
-- easy to use interface to manipulate Socket Addresses. You can only create what is
-- provided as safe.
--
-- * `SocketAddress (Inet TCP)`: socket address, using *IPv4* addresses, TCP protocol
--   and Stream network type.
-- * `SocketAddress (Inet6 TCP)`: socket address, using *IPv6* addresses, TCP protocol
--   and Stream network type.
-- * `SocketAddress (Inet UDP)`: socket address, using *IPv4* addresses, UDP protocol
--   and Datagram network type.
-- * `SocketAddress (Inet6 UDP)`: socket address, using *IPv6* addresses, UDP protocol
--   and Datagram network type.
--
-- Advanced developers can safely extend the type family to include missing domains.
--
-- ```
-- let ipv4addr = SocketAddressInet  "8.8.8.8" 53 :: SocketAddressInt   UDP
-- let ipv6addr = SocketAddressInet6     "::1" 80 :: SocketAddressInet6 TCP
-- ```
--
{-# LANGUAGE FlexibleInstances #-}

module Foundation.Network.Socket.Address
    ( Family(..), Protocol(..)
    , Inet, Inet6
    , SocketAddress(..)
    ) where

#ifdef mingw32_HOST_OS
#include <winsock2.h>
#else
#include "netinet/in.h"
#endif

import Foundation.Internal.Base
import Foundation.Internal.Proxy
import Foundation.Primitive.Endianness
import Foundation.Class.Storable

import Foreign.C.Types

import Foundation.Network.IPv4 (IPv4)
import Foundation.Network.IPv6 (IPv6)

newtype PortNumber = PortNumber Word16
  deriving (Show, Eq, Ord, Typeable)

data family SocketAddress f

-- | Domain Family (Inet, Inet6)
class Family f where
    type Address f
    type Port f
    familyCode :: f -> CInt

-- | Protocol and Type
class Family f => Protocol f where
    protocolCode :: f -> CInt
    typeCode :: f -> CInt

data Inet protocol

data TCP
data UDP

instance Family (Inet protocol) where
    type Address (Inet protocol) = IPv4
    type Port (Inet protocol) = PortNumber
    familyCode _ = (#const AF_INET)
instance Protocol (Inet TCP) where
    protocolCode _ = (#const IPPROTO_TCP)
    typeCode _ = (#const SOCK_STREAM)
instance Protocol (Inet UDP) where
    protocolCode _ = (#const IPPROTO_UDP)
    typeCode _ = (#const SOCK_DGRAM)

data instance SocketAddress (Inet protocol)
  = SocketAddressInet
    { inetAddress   :: Address (Inet protocol)
    , inetPort      :: Port (Inet protocol)
    } deriving (Show, Eq, Ord, Typeable)

data Inet6 protocol

instance Family (Inet6 protocol) where
    type Address (Inet6 protocol) = IPv6
    type Port (Inet6 protocol) = PortNumber
    familyCode _ = (#const AF_INET6)
instance Protocol (Inet6 TCP) where
    protocolCode _ = (#const IPPROTO_TCP)
    typeCode _ = (#const SOCK_STREAM)
instance Protocol (Inet6 UDP) where
    protocolCode _ = (#const IPPROTO_UDP)
    typeCode _ = (#const SOCK_DGRAM)

data instance SocketAddress (Inet6 protocol)
  = SocketAddressInet6
    { inet6Address   :: Address (Inet6 protocol)
    , inet6Port      :: Port (Inet6 protocol)
    } deriving (Show, Eq, Ord, Typeable)

instance Storable PortNumber where
    peek ptr = PortNumber . fromLE <$> peek (castPtr ptr)
    poke ptr (PortNumber p) = poke (castPtr ptr) (toLE p)
instance StorableFixed PortNumber where
    size _ = size (Proxy :: Proxy Word16)
    alignment _ = alignment (Proxy :: Proxy Word16)
instance Storable (SocketAddress (Inet protocol)) where
    peek ptr = SocketAddressInet <$> peek ptrIPv4 <*> peek ptrPort
      where
        ptrIPv4 :: Ptr IPv4
        ptrIPv4 = castPtr ptr
        ptrPort :: Ptr PortNumber
        ptrPort = castPtr $ ptrIPv4 `plusPtr` 1
    poke ptr (SocketAddressInet addr port) =
        poke ptrIPv4 addr >> poke ptrPort port
      where
        ptrIPv4 :: Ptr IPv4
        ptrIPv4 = castPtr ptr
        ptrPort :: Ptr PortNumber
        ptrPort = castPtr $ ptrIPv4 `plusPtr` 1
instance Storable (SocketAddress (Inet6 protocol)) where
    peek ptr =
        SocketAddressInet6 <$> peek ptrIPv6 <*> peek ptrPort
      where
        ptrIPv6 :: Ptr IPv6
        ptrIPv6 = castPtr ptr
        ptrPort :: Ptr PortNumber
        ptrPort = castPtr $ ptrIPv6 `plusPtr` 1
    poke ptr (SocketAddressInet6 addr port) =
        poke ptrIPv6 addr >> poke ptrPort port
      where
        ptrIPv6 :: Ptr IPv6
        ptrIPv6 = castPtr ptr
        ptrPort :: Ptr PortNumber
        ptrPort = castPtr $ ptrIPv6 `plusPtr` 1
