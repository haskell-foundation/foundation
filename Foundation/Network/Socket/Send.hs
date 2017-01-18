{-# LANGUAGE FlexibleContexts #-}

module Foundation.Network.Socket.Send
    ( send
    , SendError(..)
    ) where

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
    , retryWith, socketWaitWrite
    )

-- | error that can be thrown by the command @connect@
--
data SendError
    = SendError_Other I.SocketError
        -- ^ If a new protocol family is defined, the socreate process is free
        -- to return any desired error code. The socket() system call will pass
        -- this error code along (even if it is undefined).
  deriving (Eq, Typeable)
instance Show SendError where
    show (SendError_Other errno)
        = "SendError_Other: " <> show errno
instance Exception SendError

sendErrorFromSocketError :: I.SocketError -> SendError
sendErrorFromSocketError = SendError_Other
{-# INLINE sendErrorFromSocketError #-}

send :: PrimType ty
     => Socket f
     -> Flag
     -> UArray ty
     -> IO (Size ty)
send s flag array = do
    (CInt i) <- withPtr array $ \ptr ->
        retryWith s (throwIO . sendErrorFromSocketError) socketWaitWrite $ \fd ->
            I.send fd (castPtr ptr) (fromInteger $ toInteger $ num `scale` sz) flag
    return $ Size $ fromInteger $ toInteger i
  where
    num :: Size ty
    num = Size $ length array
    sz = primSizeInBytes (toProxy array)
    toProxy :: PrimType ty => UArray ty -> Proxy ty
    toProxy _ = Proxy
