module Foundation.Network.Socket.Socket
    ( socket
    , SocketError(..)
    ) where

import Data.Function
import Control.Concurrent.MVar

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
    = SocketError_Other I.SocketError
        -- ^ If a new protocol family is defined, the socreate process is free
        -- to return any desired error code. The socket() system call will pass
        -- this error code along (even if it is undefined).
  deriving (Eq, Typeable)
instance Show SocketError where
    show (SocketError_Other errno)
        = "SocketError_Other: " <> show errno
instance Exception SocketError

socketErrorFromSocketError :: I.SocketError -> SocketError
socketErrorFromSocketError = SocketError_Other
{-# INLINE socketErrorFromSocketError #-}

-- | create a socket for the given Family, Type and Protocol
--
-- This function may throw an error of type @SocketError@.
--
-- > s <- socket :: IO (Socket Inet Stream TCP)
--
-- Socket will be close when garbage collected or you can also close it
-- manually with @close@.
socket :: (Family f, Type f, Protocol f)
       => IO (Socket f)
socket = socket_ undefined
  where
    socket_ :: (Family f, Type f, Protocol f)
            => f -> IO (Socket f)
    socket_ f = do
        efd <- I.socket (familyCode f) (typeCode f) (protocolCode f)
        case efd of
            Left errno -> throwIO (socketErrorFromSocketError errno)
            Right fd   -> do
                mvar <- newMVar fd
                _ <- mkWeakMVar mvar (close (Socket mvar))
                return $ Socket mvar
