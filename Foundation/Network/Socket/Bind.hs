{-# LANGUAGE FlexibleContexts #-}

module Foundation.Network.Socket.Bind
    ( bind
    , BindError(..)
    ) where

import Prelude (fromIntegral)

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
    = BindError_Other I.SocketError
        -- ^ If a new protocol family is defined, the socreate process is free
        -- to return any desired error code. The socket() system call will pass
        -- this error code along (even if it is undefined).
  deriving (Eq, Typeable)
instance Show BindError where
    show (BindError_Other errno)
        = "BindError_Other: " <> show errno
instance Exception BindError

bindErrorFromSocketError :: I.SocketError -> BindError
bindErrorFromSocketError = BindError_Other
{-# INLINE bindErrorFromSocketError #-}

-- | bind the given @SocketAddress@ to the @Socket@
bind :: (Family f, StorableFixed (SocketAddress f))
     => Socket f
     -> SocketAddress f
     -> IO ()
bind s addr =
    let (Size sz) = size (Just addr) in
    allocaBytes sz $ \addrptr -> do
        poke addrptr addr
        retryWith s (throwIO . bindErrorFromSocketError) (\_ -> return ()) $ \fd ->
            I.bind fd (castPtr addrptr) (fromIntegral sz)
