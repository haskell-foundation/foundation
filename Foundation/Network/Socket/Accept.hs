{-# LANGUAGE FlexibleContexts #-}

module Foundation.Network.Socket.Accept
    ( accept
    , AcceptError(..)
    ) where

import Prelude (fromIntegral)
import Control.Monad (when)
import GHC.Conc (threadWaitRead)

import Foreign.C.Error hiding (throwErrno)
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca, allocaBytes)

import Foundation.Internal.Base
import Foundation.Internal.Types
import Foundation.Internal.Proxy
import Foundation.Class.Storable

import qualified Foundation.Network.Socket.Internal as I
import Foundation.Network.Socket.Internal
    ( Socket(..), SocketAddress
    , Family(..)
    , retryWith
    )
import Control.Concurrent.MVar
import Foundation.Network.Socket.Close

-- | error that can be thrown by the command @connect@
--
data AcceptError
    = AcceptError_PermissionError
        -- ^ The destination address is a broadcast address and the socket
        -- option SO_BROADCAST is not set.
    | AcceptError_AddressInUse
        -- ^ The address is already in use.
    | AcceptError_AddressNotAvailable
        -- ^ The specified address is not available on this machine.
    | AcceptError_AddressNotCompatibleWithFamily
        -- ^ Addresses in the specified address family cannot be used with this
        -- socket.
    | AcceptError_Other Errno
        -- ^ If a new protocol family is defined, the socreate process is free
        -- to return any desired error code. The socket() system call will pass
        -- this error code along (even if it is undefined).
  deriving (Eq, Typeable)
instance Show AcceptError where
    show AcceptError_PermissionError
        = "The destination address is a broadcast address and the socket\
          \ option SO_BROADCAST is not set."
    show AcceptError_AddressInUse
        = "The address is already in use."
    show AcceptError_AddressNotAvailable
        = "The specified address is not available on this machine."
    show AcceptError_AddressNotCompatibleWithFamily
        = "Addresses in the specified address family cannot be used with this\
          \ socket."
    show (AcceptError_Other errno)
        = "AcceptError_Other: " <> show (I.SocketError errno)
instance Exception AcceptError

connectErrorFromErrno :: Errno -> AcceptError
connectErrorFromErrno err
    | err == eACCES          = AcceptError_PermissionError
    | err == eADDRINUSE      = AcceptError_AddressInUse
    | err == eADDRNOTAVAIL   = AcceptError_AddressNotAvailable
    | err == eAFNOSUPPORT    = AcceptError_AddressNotCompatibleWithFamily
    | otherwise              = AcceptError_Other err
{-# INLINE connectErrorFromErrno #-}

accept :: (Family f, StorableFixed (SocketAddress f))
       => Socket f t p
       -> IO (Socket f t p, SocketAddress f)
accept s =
    allocaAddr s Proxy $ \addrPtr (CInt sz) ->
      alloca $ \addrLenPtr -> do
        poke addrLenPtr sz
        fd' <- retryWith s (throwIO . connectErrorFromErrno) threadWaitRead $ \fd -> do
                when (fd < I.Fd 0) (I.throwErrno eBADF)
                I.accept fd (castPtr addrPtr) (castPtr addrLenPtr)
        addr <- peek addrPtr
        mvar <- newMVar fd'
        _ <- mkWeakMVar mvar (close (Socket mvar))
        return (Socket mvar, addr)
  where
    allocaAddr :: (Family f, StorableFixed (SocketAddress f))
               => Socket f t p
               -> Proxy (SocketAddress f)
               -> (Ptr (SocketAddress f) -> CInt -> IO a)
               -> IO a
    allocaAddr _ p f =
        let (Size sz) = size p
         in allocaBytes sz $ \ptr -> f (castPtr ptr) (fromIntegral sz)
