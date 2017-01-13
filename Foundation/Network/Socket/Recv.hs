{-# LANGUAGE FlexibleContexts #-}

module Foundation.Network.Socket.Recv
    ( recv
    , RecvError(..)
    ) where

import GHC.Conc (threadWaitRead)

import Foreign.C.Types

import Foundation.Array.Unboxed (withPtr, UArray)
import Foundation.Array.Unboxed.Mutable (newPinned)
import Foundation.Class.Storable
import Foundation.Collection
import Foundation.Internal.Base
import Foundation.Internal.Types
import Foundation.Internal.Proxy
import Foundation.Numerical
import Foundation.Primitive

import qualified Foundation.Network.Socket.Internal as I
import Foundation.Network.Socket.Internal
    ( Socket, Flag
    , retryWith
    )

-- | error that can be thrown by the command @connect@
--
data RecvError
    = RecvError_Other I.SocketError
        -- ^ If a new protocol family is defined, the socreate process is free
        -- to return any desired error code. The socket() system call will pass
        -- this error code along (even if it is undefined).
  deriving (Eq, Typeable)
instance Show RecvError where
    show (RecvError_Other errno)
        = "RecvError_Other: " <> show errno
instance Exception RecvError

recvErrorFromSocketError :: I.SocketError -> RecvError
recvErrorFromSocketError = RecvError_Other
{-# INLINE recvErrorFromSocketError #-}


recv :: (Show ty, PrimType ty)
     => Socket f
     -> Flag
     -> Size ty
     -> IO (UArray ty)
recv s flag num = do
    -- TODO use mutable ByteArray
    --      fix the bug in withMutableByteArrayPtr  which copies a temporary
    --      ByteArray#
    array <- newPinned num >>= unsafeFreeze
    CInt sz <- withPtr array $ \ptr ->
                 retryWith s (throwIO . recvErrorFromSocketError) threadWaitRead $ \fd ->
                    let numBytes = fromInteger $ toInteger $ num `scale` primSizeInBytes (toProxy array)
                     in I.recv fd (castPtr ptr) numBytes flag
    let sz' = fromInteger $ toInteger sz
    return $ take sz' array
  where
    toProxy :: PrimType ty => UArray ty -> Proxy ty
    toProxy _ = Proxy
