{-# LANGUAGE FlexibleContexts #-}

module Foundation.Network.Socket.Listen
    ( listen
    , ListenError(..)
    ) where

import Prelude (fromIntegral)
import Control.Monad (when)

import Foreign.C.Error hiding (throwErrno)

import Foundation.Internal.Base
import Foundation.Class.Storable

import qualified Foundation.Network.Socket.Internal as I
import Foundation.Network.Socket.Internal
    ( Socket(..), SocketAddress
    , Family(..)
    , retryWith
    )

-- | error that can be thrown by the command @connect@
--
data ListenError
    = ListenError_PermissionError
        -- ^ The destination address is a broadcast address and the socket
        -- option SO_BROADCAST is not set.
    | ListenError_AddressInUse
        -- ^ The address is already in use.
    | ListenError_AddressNotAvailable
        -- ^ The specified address is not available from the local machine
    | ListenError_Other Errno
        -- ^ If a new protocol family is defined, the socreate process is free
        -- to return any desired error code. The socket() system call will pass
        -- this error code along (even if it is undefined).
  deriving (Eq, Typeable)
instance Show ListenError where
    show ListenError_PermissionError
        = "The destination address is a broadcast address and the socket\
          \ option SO_BROADCAST is not set."
    show ListenError_AddressInUse
        = "The address is already in use."
    show ListenError_AddressNotAvailable
        = "The specified address is not available from the local this machine."
    show (ListenError_Other errno)
        = "ListenError_Other: " <> show (I.SocketError errno)
instance Exception ListenError

listenErrorFromErrno :: Errno -> ListenError
listenErrorFromErrno err
    | err == eACCES          = ListenError_PermissionError
    | err == eADDRINUSE      = ListenError_AddressInUse
    | err == eADDRNOTAVAIL   = ListenError_AddressNotAvailable
    | otherwise              = ListenError_Other err
{-# INLINE listenErrorFromErrno #-}

listen :: (Family f, StorableFixed (SocketAddress f))
     => Socket f t p
     -> Int
     -> IO ()
listen s sz =
    retryWith s (throwIO . listenErrorFromErrno) (\_ -> return ()) $ \fd -> do
        when (fd < I.Fd 0) (I.throwErrno eBADF)
        I.listen fd (fromIntegral sz)
