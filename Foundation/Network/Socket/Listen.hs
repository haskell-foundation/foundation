{-# LANGUAGE FlexibleContexts #-}

module Foundation.Network.Socket.Listen
    ( listen
    , ListenError(..)
    ) where

import Prelude (fromIntegral)

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
    = ListenError_AddressInUse
        -- ^ The address is already in use.
    | ListenError_AddressNotAvailable
        -- ^ The specified address is not available from the local machine
    | ListenError_Other I.SocketError
        -- ^ If a new protocol family is defined, the socreate process is free
        -- to return any desired error code. The socket() system call will pass
        -- this error code along (even if it is undefined).
  deriving (Eq, Typeable)
instance Show ListenError where
    show ListenError_AddressInUse
        = "The address is already in use."
    show ListenError_AddressNotAvailable
        = "The specified address is not available from the local this machine."
    show (ListenError_Other errno)
        = "ListenError_Other: " <> show errno
instance Exception ListenError

listenErrorFromSocketError :: I.SocketError -> ListenError
listenErrorFromSocketError err
    | err == I.eAddressInUse        = ListenError_AddressInUse
    | err == I.eAddressNotAvailable = ListenError_AddressNotAvailable
    | otherwise                     = ListenError_Other err
{-# INLINE listenErrorFromSocketError #-}

listen :: (Family f, StorableFixed (SocketAddress f))
     => Socket f
     -> Int
     -> IO ()
listen s sz =
    retryWith s (throwIO . listenErrorFromSocketError) (\_ -> return ()) $ \fd ->
        I.listen fd (fromIntegral sz)
