{-# LANGUAGE FlexibleContexts #-}

module Foundation.Network.Socket.Recv
    ( recv
    , RecvError(..)
    ) where

import Control.Monad (when)
import GHC.Conc (threadWaitRead)

import Foreign.C.Error hiding (throwErrno)
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
    = RecvError_PermissionError
        -- ^ The destination address is a broadcast address and the socket
        -- option SO_BROADCAST is not set.
    | RecvError_AddressInUse
        -- ^ The address is already in use.
    | RecvError_AddressNotAvailable
        -- ^ The specified address is not available on this machine.
    | RecvError_AddressNotCompatibleWithFamily
        -- ^ Addresses in the specified address family cannot be used with this
        -- socket.
    | RecvError_Other Errno
        -- ^ If a new protocol family is defined, the socreate process is free
        -- to return any desired error code. The socket() system call will pass
        -- this error code along (even if it is undefined).
  deriving (Eq, Typeable)
instance Show RecvError where
    show RecvError_PermissionError
        = "The destination address is a broadcast address and the socket\
          \ option SO_BROADCAST is not set."
    show RecvError_AddressInUse
        = "The address is already in use."
    show RecvError_AddressNotAvailable
        = "The specified address is not available on this machine."
    show RecvError_AddressNotCompatibleWithFamily
        = "Addresses in the specified address family cannot be used with this\
          \ socket."
    show (RecvError_Other errno)
        = "RecvError_Other: " <> show (I.SocketError errno)
instance Exception RecvError

recvErrorFromErrno :: Errno -> RecvError
recvErrorFromErrno err
    | err == eACCES          = RecvError_PermissionError
    | err == eADDRINUSE      = RecvError_AddressInUse
    | err == eADDRNOTAVAIL   = RecvError_AddressNotAvailable
    | err == eAFNOSUPPORT    = RecvError_AddressNotCompatibleWithFamily
    | otherwise              = RecvError_Other err
{-# INLINE recvErrorFromErrno #-}

recv :: (Show ty, PrimType ty)
     => Socket f t p
     -> Flag
     -> Size ty
     -> IO (UArray ty)
recv s flag num = do
    -- TODO use mutable ByteArray
    --      fix the bug in withMutableByteArrayPtr  which copies a temporary
    --      ByteArray#
    array <- newPinned num >>= unsafeFreeze
    CInt sz <- withPtr array $ \ptr ->
                 retryWith s (throwIO . recvErrorFromErrno) threadWaitRead $ \fd -> do
                    when (fd < I.Fd 0) (I.throwErrno eBADF)
                    let numBytes = fromInteger $ toInteger $ num `scale` primSizeInBytes (toProxy array)
                    I.recv fd (castPtr ptr) numBytes flag
    let sz' = fromInteger $ toInteger sz
    return $ take sz' array
  where
    toProxy :: PrimType ty => UArray ty -> Proxy ty
    toProxy _ = Proxy
