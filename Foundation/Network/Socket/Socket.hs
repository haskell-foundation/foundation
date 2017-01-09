module Foundation.Network.Socket.Socket
    ( socket
    , SocketError(..)
    ) where

import Data.Function
import Control.Concurrent.MVar

import Foreign.C.Error hiding (throwErrno)

import Foundation.Internal.Base

import qualified Foundation.Network.Socket.Internal as I
import Foundation.Network.Socket.Internal
    ( Socket(..)
    , Family(..), Type(..), Protocol(..)
    )

import Foundation.Network.Socket.Close

-- | error that can be thrown by the command @socket@
--
data SocketError
    = SocketError_PermissionError
        -- ^ Permission to create a socket of the specified type and/or
        -- protocol is denied.
    | SocketError_AddressFamilyNotSupported
        -- ^ The specified address family is not supported.
    | SocketError_ProcessDescriptorTableFull
        -- ^ The per-process descriptor table is full.
    | SocketError_SystemDescriptorTableFull
        -- ^ The system file table is full.
    | SocketError_NotEnoughBufferSpace
        -- ^ Insufficient buffer space is available.  The socket cannot be
        -- created until sufficient resources are freed.
    | SocketError_NotEnoughMemory
        -- ^ Insufficient memory was available to fulfill the request.
    | SocketError_ProtocolNotSupported
        -- ^ The protocol type or the specified protocol is not supported
        -- within this domain.
    | SocketError_SocketTypeNotSupported
        -- ^ The socket type is not supported by the protocol.
    | SocketError_Other Errno
        -- ^ If a new protocol family is defined, the socreate process is free
        -- to return any desired error code. The socket() system call will pass
        -- this error code along (even if it is undefined).
  deriving (Eq, Typeable)
instance Show SocketError where
    show SocketError_PermissionError
        = "Permission to create a socket of the specified type and/or protocol\
          \ is denied"
    show SocketError_AddressFamilyNotSupported
        = "The specified address family is not supported"
    show SocketError_ProcessDescriptorTableFull
        = "The per-process descriptor table is full"
    show SocketError_SystemDescriptorTableFull
        = "The system file table is full"
    show SocketError_NotEnoughBufferSpace
        = "Insufficient buffer space is available.  The socket cannot be\
          \ created until sufficient resources are freed."
    show SocketError_NotEnoughMemory
        = "Insufficient memory was available to fulfill the request"
    show SocketError_ProtocolNotSupported
        = "The protocol type or the specified protocol is not supported\
          \ within this domain"
    show SocketError_SocketTypeNotSupported
        = "The socket type is not supported by the protocol"
    show (SocketError_Other errno)
        = "SocketError_Other: " <> show (I.SocketError errno)
instance Exception SocketError

socketErrorFromErrno :: Errno -> SocketError
socketErrorFromErrno err
    | err == eACCES          = SocketError_PermissionError
    | err == eAFNOSUPPORT    = SocketError_AddressFamilyNotSupported
    | err == eMFILE          = SocketError_ProcessDescriptorTableFull
    | err == eNFILE          = SocketError_SystemDescriptorTableFull
    | err == eNOBUFS         = SocketError_NotEnoughBufferSpace
    | err == eNOMEM          = SocketError_NotEnoughMemory
    | err == ePROTONOSUPPORT = SocketError_ProtocolNotSupported
    | err == ePROTOTYPE      = SocketError_SocketTypeNotSupported
    | otherwise              = SocketError_Other err
{-# INLINE socketErrorFromErrno #-}

-- | create a socket for the given Family, Type and Protocol
--
-- This function may throw an error of type @SocketError@.
--
-- > s <- socket :: IO (Socket Inet Stream TCP)
--
-- Socket will be close when garbage collected or you can also close it
-- manually with @close@.
socket :: (Family f, Type t, Protocol p)
       => IO (Socket f t p)
socket = socket_ undefined undefined undefined
  where
    socket_ :: (Family f, Type t, Protocol p)
            => f -> t -> p -> IO (Socket f t p)
    socket_ f t p = do
        efd <- I.socket (familyCode f) (typeCode t) (protocolCode p)
        case efd of
            Left errno -> throwIO (socketErrorFromErrno errno)
            Right fd   -> do
                mvar <- newMVar fd
                _ <- mkWeakMVar mvar (close (Socket mvar))
                return $ Socket mvar
