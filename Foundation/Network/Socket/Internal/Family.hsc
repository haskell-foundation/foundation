{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}

module Foundation.Network.Socket.Internal.Family
    ( SocketAddress (SocketAddressInet, inetAddress, inetPort)
    , Family(..)

    , Inet
    , InetAddress(..), InetPort(..)
    , inetAddressFromTuple
    , inetAny
    ) where

#include "netinet/in.h"

import Data.List (foldl1')
import Foreign.C.Types

import Foundation
import Foundation.Internal.Base
import Foundation.Class.Storable

data family SocketAddress f

class Family f where
    familyCode :: f -> CInt

data Inet
instance Family Inet where
    familyCode _ = (#const AF_INET)

data instance SocketAddress Inet
  = SocketAddressInet
    { inetAddress   :: InetAddress
    , inetPort      :: InetPort
    }
  deriving (Eq, Show)

newtype InetPort = InetPort Word16
  deriving (Eq, Show)
newtype InetAddress = InetAddress Word32
  deriving (Eq, Show)
inetAddressFromTuple :: (Word8, Word8, Word8, Word8) -> InetAddress
inetAddressFromTuple (w0, w1, w2, w3) =
    InetAddress $ foldl1' (\x y->x*256+y) [f w0, f w1, f w2, f w3]
  where
    f = fromIntegral
inetAny :: InetAddress
inetAny = InetAddress 0

instance Storable InetPort where
  peek ptr    = do
      p0 <- peekOff (castPtr ptr) (Offset 0 :: Offset Word8)
      p1 <- peekOff (castPtr ptr) (Offset 1 :: Offset Word8)
      return $ InetPort (fromIntegral p0 * 256 + fromIntegral p1)
  poke ptr (InetPort w16) = do
      pokeOff (castPtr ptr) 0 (w16_0 w16)
      pokeOff (castPtr ptr) 1 (w16_1 w16)
    where
      w16_0, w16_1 :: Word16 -> Word8
      w16_0 x = fromIntegral $ (x `div` 256) `mod` 256
      w16_1 x = fromIntegral $ x `mod` 256

instance Storable InetAddress where
    peek ptr    = do
        i0  <- peekOff (castPtr ptr) (Offset 0 :: Offset Word8)
        i1  <- peekOff (castPtr ptr) (Offset 1 :: Offset Word8)
        i2  <- peekOff (castPtr ptr) (Offset 2 :: Offset Word8)
        i3  <- peekOff (castPtr ptr) (Offset 3 :: Offset Word8)
        return $ InetAddress $ (((((f i0 * 256) + f i1) * 256) + f i2) * 256) + f i3
      where
        f = fromIntegral
    poke ptr (InetAddress a) = do
        pokeOff (castPtr ptr) 0 (fromIntegral $ mod (a `div` (256*256*256)) 256 :: Word8)
        pokeOff (castPtr ptr) 1 (fromIntegral $ mod (a `div`     (256*256)) 256 :: Word8)
        pokeOff (castPtr ptr) 2 (fromIntegral $ mod (a `div`           256) 256 :: Word8)
        pokeOff (castPtr ptr) 3 (fromIntegral $ mod a $ 256 :: Word8)

instance Storable (SocketAddress Inet) where
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
instance StorableFixed (SocketAddress Inet) where
    size      _ = (#const sizeof(struct sockaddr_in))
    alignment _ = (#alignment struct sockaddr_in)
foreign import ccall unsafe "memset"
    c_memset :: Ptr a -> CInt -> CSize -> IO ()
