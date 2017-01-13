{-# LANGUAGE FlexibleContexts #-}

module Foundation.Network.Socket.Accept
    ( accept
    , AcceptError(..)
    ) where

import Prelude (fromIntegral)
import GHC.Conc (threadWaitRead)

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
    = AcceptError_Other I.SocketError
        -- ^ If a new protocol family is defined, the socreate process is free
        -- to return any desired error code. The socket() system call will pass
        -- this error code along (even if it is undefined).
  deriving (Eq, Typeable)
instance Show AcceptError where
    show (AcceptError_Other errno)
        = "AcceptError_Other: " <> show errno
instance Exception AcceptError

connectErrorFromSocketError :: I.SocketError -> AcceptError
connectErrorFromSocketError = AcceptError_Other
{-# INLINE connectErrorFromSocketError #-}

accept :: (Family f, StorableFixed (SocketAddress f))
       => Socket f
       -> IO (Socket f, SocketAddress f)
accept s =
    allocaAddr s Proxy $ \addrPtr (CInt sz) ->
      alloca $ \addrLenPtr -> do
        poke addrLenPtr sz
        fd' <- retryWith s (throwIO . connectErrorFromSocketError) threadWaitRead $ \fd ->
                I.accept fd (castPtr addrPtr) (castPtr addrLenPtr)
        addr <- peek addrPtr
        mvar <- newMVar fd'
        _ <- mkWeakMVar mvar (close (Socket mvar))
        return (Socket mvar, addr)
  where
    allocaAddr :: (Family f, StorableFixed (SocketAddress f))
               => Socket f
               -> Proxy (SocketAddress f)
               -> (Ptr (SocketAddress f) -> CInt -> IO a)
               -> IO a
    allocaAddr _ p f =
        let (Size sz) = size p
         in allocaBytes sz $ \ptr -> f (castPtr ptr) (fromIntegral sz)
