{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}

module Foundation.Network.Socket.Internal.Family
    ( SocketAddress (SocketAddressInet, inetAddress, inetPort)
    , Family(..)

    , Inet
    , InetPort(..)
    ) where

#include "netinet/in.h"

import Foreign.C.Types

import Foundation
import Foundation.Internal.Base
import Foundation.Class.Storable
import Foundation.Bits (htons)

import Foundation.Network.IPv4 (IPv4)
import Foundation.Network.Socket.Internal.Protocol
import Foundation.Network.Socket.Internal.Type

data family SocketAddress f

class Family f where
    type Address f
    familyCode :: f -> CInt

data Inet protocol
instance Family (Inet protocol) where
    type Address (Inet protocol) = IPv4
    familyCode _ = (#const AF_INET)
instance Protocol (Inet TCP) where
    protocolCode _ = (#const IPPROTO_TCP)
instance Protocol (Inet UDP) where
    protocolCode _ = (#const IPPROTO_UDP)
instance Type (Inet TCP) where
    typeCode _ = (#const SOCK_STREAM)
instance Type (Inet UDP) where
    typeCode _ = (#const SOCK_DGRAM)

data instance SocketAddress (Inet protocol)
  = SocketAddressInet
    { inetAddress   :: Address (Inet protocol)
    , inetPort      :: InetPort
    }
  deriving (Eq, Show)

newtype InetPort = InetPort Word16
  deriving (Eq, Show)

instance Storable InetPort where
    peek ptr    = do
      p0 <- peekOff (castPtr ptr) (Offset 0 :: Offset Word8)
      p1 <- peekOff (castPtr ptr) (Offset 1 :: Offset Word8)
      return $ InetPort (fromIntegral p0 * 256 + fromIntegral p1)
    poke ptr (InetPort w16) = poke (castPtr ptr) (htons w16)

instance Storable (SocketAddress (Inet protocol)) where
    peek ptr    = do
        a  <- peek $ castPtr (sin_addr wptr)
        p  <- peek $ castPtr (sin_port wptr)
        return $ SocketAddressInet a p
      where
        wptr :: Ptr Word8
        wptr = castPtr ptr
        sin_addr     = (#ptr struct in_addr, s_addr) . (#ptr struct sockaddr_in, sin_addr)
        sin_port     = (#ptr struct sockaddr_in, sin_port)
    poke ptr (SocketAddressInet a p) = do
        c_memset ptr 0 (#const sizeof(struct sockaddr_in))
        poke        (castPtr $ sin_family   wptr) ((#const AF_INET) :: Word16)
        poke        (castPtr $ sin_addr     wptr) a
        poke        (castPtr $ sin_port     wptr) p
      where
        wptr :: Ptr Word8
        wptr = castPtr ptr
        sin_family   = (#ptr struct sockaddr_in, sin_family)
        sin_addr     = (#ptr struct in_addr, s_addr) . (#ptr struct sockaddr_in, sin_addr)
        sin_port = (#ptr struct sockaddr_in, sin_port)
instance StorableFixed (SocketAddress (Inet protocol)) where
    size      _ = (#const sizeof(struct sockaddr_in))
    alignment _ = (#alignment struct sockaddr_in)

foreign import ccall unsafe "memset"
    c_memset :: Ptr a -> CInt -> CSize -> IO ()
