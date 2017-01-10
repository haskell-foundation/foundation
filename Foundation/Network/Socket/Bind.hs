{-# LANGUAGE FlexibleContexts #-}

module Foundation.Network.Socket.Bind
    ( bind
    , BindError(..)
    ) where

import Prelude (fromIntegral)
import Control.Monad (when)

import Foreign.C.Error hiding (throwErrno)
import Foreign.Marshal.Alloc (allocaBytes)

import Foundation.Internal.Base
import Foundation.Internal.Types
import Foundation.Class.Storable

import qualified Foundation.Network.Socket.Internal as I
import Foundation.Network.Socket.Internal
    ( Socket(..), SocketAddress
    , Family(..)
    , retryWith
    )

-- | error that can be thrown by the command @connect@
--
data BindError
    = BindError_PermissionError
        -- ^ The destination address is a broadcast address and the socket
        -- option SO_BROADCAST is not set.
    | BindError_AddressInUse
        -- ^ The address is already in use.
    | BindError_AddressNotAvailable
        -- ^ The specified address is not available from the local machine
    | BindError_Other Errno
        -- ^ If a new protocol family is defined, the socreate process is free
        -- to return any desired error code. The socket() system call will pass
        -- this error code along (even if it is undefined).
  deriving (Eq, Typeable)
instance Show BindError where
    show BindError_PermissionError
        = "The destination address is a broadcast address and the socket\
          \ option SO_BROADCAST is not set."
    show BindError_AddressInUse
        = "The address is already in use."
    show BindError_AddressNotAvailable
        = "The specified address is not available from the local this machine."
    show (BindError_Other errno)
        = "BindError_Other: " <> show (I.SocketError errno)
instance Exception BindError

bindErrorFromErrno :: Errno -> BindError
bindErrorFromErrno err
    | err == eACCES          = BindError_PermissionError
    | err == eADDRINUSE      = BindError_AddressInUse
    | err == eADDRNOTAVAIL   = BindError_AddressNotAvailable
    | otherwise              = BindError_Other err
{-# INLINE bindErrorFromErrno #-}

-- | bind the given @SocketAddress@ to the @Socket@
bind :: (Family f, StorableFixed (SocketAddress f))
     => Socket f t p
     -> SocketAddress f
     -> IO ()
bind s addr =
    let (Size sz) = size (Just addr) in
    allocaBytes sz $ \addrptr -> do
        poke addrptr addr
        retryWith s (throwIO . bindErrorFromErrno) (\_ -> return ()) $ \fd -> do
            when (fd < I.Fd 0) (I.throwErrno eBADF)
            I.bind fd (castPtr addrptr) (fromIntegral sz)
