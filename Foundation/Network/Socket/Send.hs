{-# LANGUAGE FlexibleContexts #-}

module Foundation.Network.Socket.Send
    ( send
    , SendError(..)
    ) where

import Control.Monad (when)
import GHC.Conc (threadWaitWrite)

import Foreign.C.Error hiding (throwErrno)
import Foreign.C.Types

import Foundation.Array.Unboxed (withPtr, UArray)
import Foundation.Class.Storable
import Foundation.Collection (length)
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
data SendError
    = SendError_PermissionError
        -- ^ The destination address is a broadcast address and the socket
        -- option SO_BROADCAST is not set.
    | SendError_AddressInUse
        -- ^ The address is already in use.
    | SendError_AddressNotAvailable
        -- ^ The specified address is not available on this machine.
    | SendError_AddressNotCompatibleWithFamily
        -- ^ Addresses in the specified address family cannot be used with this
        -- socket.
    | SendError_Other Errno
        -- ^ If a new protocol family is defined, the socreate process is free
        -- to return any desired error code. The socket() system call will pass
        -- this error code along (even if it is undefined).
  deriving (Eq, Typeable)
instance Show SendError where
    show SendError_PermissionError
        = "The destination address is a broadcast address and the socket\
          \ option SO_BROADCAST is not set."
    show SendError_AddressInUse
        = "The address is already in use."
    show SendError_AddressNotAvailable
        = "The specified address is not available on this machine."
    show SendError_AddressNotCompatibleWithFamily
        = "Addresses in the specified address family cannot be used with this\
          \ socket."
    show (SendError_Other errno)
        = "SendError_Other: " <> show (I.SocketError errno)
instance Exception SendError

sendErrorFromErrno :: Errno -> SendError
sendErrorFromErrno err
    | err == eACCES          = SendError_PermissionError
    | err == eADDRINUSE      = SendError_AddressInUse
    | err == eADDRNOTAVAIL   = SendError_AddressNotAvailable
    | err == eAFNOSUPPORT    = SendError_AddressNotCompatibleWithFamily
    | otherwise              = SendError_Other err
{-# INLINE sendErrorFromErrno #-}

send :: PrimType ty
     => Socket f t p
     -> Flag
     -> UArray ty
     -> IO (Size ty)
send s flag array = do
    (CInt i) <- withPtr array $ \ptr ->
        retryWith s (throwIO . sendErrorFromErrno) threadWaitWrite $ \fd -> do
            when (fd < I.Fd 0) (I.throwErrno eBADF)
            I.send fd (castPtr ptr) (fromInteger $ toInteger $ num `scale` sz) flag
    return $ Size $ fromInteger $ toInteger i
  where
    num :: Size ty
    num = Size $ length array
    sz = primSizeInBytes (toProxy array)
    toProxy :: PrimType ty => UArray ty -> Proxy ty
    toProxy _ = Proxy
