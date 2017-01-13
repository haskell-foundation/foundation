{-# LANGUAGE FlexibleContexts #-}

module Foundation.Network.Socket.Connect
    ( connect
    , ConnectError(..)
    ) where

import Prelude (fromIntegral)
import GHC.Conc (threadWaitRead)

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
    = ConnectError_Other I.SocketError
        -- ^ If a new protocol family is defined, the socreate process is free
        -- to return any desired error code. The socket() system call will pass
        -- this error code along (even if it is undefined).
  deriving (Eq, Typeable)
instance Show ConnectError where
    show (ConnectError_Other errno)
        = "ConnectError_Other: " <> show errno
instance Exception ConnectError

connectErrorFromSocketError :: I.SocketError -> ConnectError
connectErrorFromSocketError = ConnectError_Other
{-# INLINE connectErrorFromSocketError #-}

-- | connect the @Socket@ to the given @SocketAddress@.
connect :: (Family f, StorableFixed (SocketAddress f))
        => Socket f
        -> SocketAddress f
        -> IO ()
connect s addr =
    let (Size sz) = size (Just addr) :: Size Word8 in
    allocaBytes sz $ \addrptr -> do
        poke addrptr addr
        retryWith s (throwIO . connectErrorFromSocketError) threadWaitRead $ \fd -> do
            e <- I.connect fd (castPtr addrptr) (fromIntegral sz)
            return $ case e of
                Left err | err == I.eIsConnected -> Right ()
                         | err == I.eInProgress ->  Left I.eAgain
                         | otherwise      -> Left err
                Right a                   -> Right a
