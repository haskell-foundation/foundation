{-# LANGUAGE FlexibleContexts #-}

module Foundation.Network.Socket.Connect
    ( connect
    , ConnectError(..)
    ) where

import Prelude (fromIntegral)
import Control.Monad (when)
import GHC.Conc (threadWaitRead)

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
data ConnectError
    = ConnectError_PermissionError
        -- ^ The destination address is a broadcast address and the socket
        -- option SO_BROADCAST is not set.
    | ConnectError_AddressInUse
        -- ^ The address is already in use.
    | ConnectError_AddressNotAvailable
        -- ^ The specified address is not available on this machine.
    | ConnectError_AddressNotCompatibleWithFamily
        -- ^ Addresses in the specified address family cannot be used with this
        -- socket.
    | ConnectError_AlreadyBlocking
        -- ^ The socket is non-blocking and a previous connection attempt has
        -- not yet been completed.
    | ConnectError_SocketCorruptedOrInvalid
        -- ^ socket is not a valid descriptor
    | ConnectError_ConnectionRefused
        -- ^ The attempt to connect was ignored (because the target is not
        -- listening for connections) or explicitly rejected.
    | ConnectError_AddressOutOfBound
        -- ^ The address parameter specifies an area outside the process
        -- address space.
    | ConnectError_CannotReach
        -- ^ The target host cannot be reached (e.g., down, disconnected).
    | ConnectError_InvalidArgument
        -- ^ An invalid argument was detected (e.g., address_len is not valid
        -- for the address family, the specified address family is invalid).
    | ConnectError_NetworkInterfaceDown
        -- ^ The local network interface is not functioning.
    | ConnectError_NetworkCannotReach
        -- ^ The network isn't reachable from this host.
    | ConnectError_NotEnoughBuf
        -- ^ The system call was unable to allocate a needed memory buffer.
    | ConnectError_NotSocket
        -- ^ socket is not a file descriptor for a socket.
    | ConnectError_AlreadyListening
        -- ^ Because socket is listening, no connection is allowed.
    | ConnectError_AddressNotCompatibleWithType
        -- ^ address has a different type than the socket that is bound to the
        -- specified peer address.
    | ConnectError_Timeout
        -- ^ Connection establishment timed out without establishing a
        -- connection.
    | ConnectError_RemoteReset
        -- ^ Remote host reset the connection request.
    | ConnectError_Other Errno
        -- ^ If a new protocol family is defined, the socreate process is free
        -- to return any desired error code. The socket() system call will pass
        -- this error code along (even if it is undefined).
  deriving (Eq, Typeable)
instance Show ConnectError where
    show ConnectError_PermissionError
        = "The destination address is a broadcast address and the socket\
          \ option SO_BROADCAST is not set."
    show ConnectError_AddressInUse
        = "The address is already in use."
    show ConnectError_AddressNotAvailable
        = "The specified address is not available on this machine."
    show ConnectError_AddressNotCompatibleWithFamily
        = "Addresses in the specified address family cannot be used with this\
          \ socket."
    show ConnectError_AlreadyBlocking
        = "The socket is non-blocking and a previous connection attempt has\
          \ not yet been completed."
    show ConnectError_SocketCorruptedOrInvalid
        = "socket is not a valid descriptor"
    show ConnectError_ConnectionRefused
        = "The attempt to connect was ignored (because the target is not\
          \ listening for connections) or explicitly rejected."
    show ConnectError_AddressOutOfBound
        = "The address parameter specifies an area outside the process\
          \ address space."
    show ConnectError_CannotReach
        = "The target host cannot be reached (e.g., down, disconnected)."
    show ConnectError_InvalidArgument
        = "An invalid argument was detected (e.g., address_len is not valid\
          \ for the address family, the specified address family is invalid)."
    show ConnectError_NetworkInterfaceDown
        = "The local network interface is not functioning."
    show ConnectError_NetworkCannotReach
        = "The network isn't reachable from this host."
    show ConnectError_NotEnoughBuf
        = "The system call was unable to allocate a needed memory buffer."
    show ConnectError_NotSocket
        = "socket is not a file descriptor for a socket."
    show ConnectError_AlreadyListening
        = "Because socket is listening, no connection is allowed."
    show ConnectError_AddressNotCompatibleWithType
        = "address has a different type than the socket that is bound to the\
          \ specified peer address."
    show ConnectError_Timeout
        = "Connection establishment timed out without establishing a\
          \ connection."
    show ConnectError_RemoteReset
        = "Remote host reset the connection request."
    show (ConnectError_Other errno)
        = "ConnectError_Other: " <> show (I.SocketError errno)
instance Exception ConnectError

connectErrorFromErrno :: Errno -> ConnectError
connectErrorFromErrno err
    | err == eACCES          = ConnectError_PermissionError
    | err == eADDRINUSE      = ConnectError_AddressInUse
    | err == eADDRNOTAVAIL   = ConnectError_AddressNotAvailable
    | err == eAFNOSUPPORT    = ConnectError_AddressNotCompatibleWithFamily
    | err == eALREADY        = ConnectError_AlreadyBlocking
    | err == eBADF           = ConnectError_SocketCorruptedOrInvalid
    | err == eCONNREFUSED    = ConnectError_ConnectionRefused
    | err == eFAULT          = ConnectError_AddressOutOfBound
    | err == eHOSTUNREACH    = ConnectError_CannotReach
    | err == eINVAL          = ConnectError_InvalidArgument
    | err == eNETDOWN        = ConnectError_NetworkInterfaceDown
    | err == eNETUNREACH     = ConnectError_NetworkCannotReach
    | err == eNOBUFS         = ConnectError_NotEnoughBuf
    | err == eNOTSOCK        = ConnectError_NotSocket
    | err == eOPNOTSUPP      = ConnectError_AlreadyListening
    | err == ePROTOTYPE      = ConnectError_AddressNotCompatibleWithType
    | err == eTIMEDOUT       = ConnectError_Timeout
    | err == eCONNRESET      = ConnectError_RemoteReset
    | otherwise              = ConnectError_Other err
{-# INLINE connectErrorFromErrno #-}

-- | connect the @Socket@ to the given @SocketAddress@.
connect :: (Family f, StorableFixed (SocketAddress f))
        => Socket f t p
        -> SocketAddress f
        -> IO ()
connect s addr =
    let (Size sz) = size (Just addr) :: Size Word8 in
    allocaBytes sz $ \addrptr -> do
        poke addrptr addr
        retryWith s (throwIO . connectErrorFromErrno) threadWaitRead $ \fd -> do
            when (fd < I.Fd 0) (I.throwErrno eBADF)
            e <- I.connect fd (castPtr addrptr) (fromIntegral sz)
            return $ case e of
                Left err | err == eISCONN -> Right ()
                         | err == eINPROGRESS -> Left eAGAIN
                         | otherwise      -> Left err
                Right a                   -> Right a
