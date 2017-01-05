{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Foundation.Network.Socket.Internal.Unix
    ( CSockAddr
    , CSockLen

      -- * Error
    , SocketError(..), throwErrno
    , Errno

      -- * Socket
    , Fd(..)
    , socket
    , close
    , connect
    , shutdown
    , bind
    , listen
    , accept
    , send
    , sendto
    , recv
    , recvfrom
    ) where

#include <sys/socket.h>

import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Error hiding (throwErrno)
import System.Posix.Types (Fd(..))
import System.IO.Unsafe (unsafePerformIO)

import Foundation.Internal.Base

newtype SocketError = SocketError Errno
    deriving (Eq,Typeable)
instance Show SocketError where
    show (SocketError (Errno errno)) = unsafePerformIO $ c_strerrno errno >>= peekCString
instance Exception SocketError
foreign import ccall unsafe "strerror"
    c_strerrno :: CInt -> IO CString

throwErrno :: Errno -> IO a
throwErrno = throwIO . SocketError

checkRet :: (Integral ret, Eq ret)
         => (ret -> a) -> IO ret -> IO (Either Errno a)
checkRet mapper action = do
    r <- action
    if r == fromInteger (-1)
        then Left <$> getErrno
        else return $ Right $ mapper r

type CSockAddr = Ptr Word8
type CSockLen  = CInt

socket :: CInt -> CInt -> CInt -> IO (Either Errno Fd)
socket f t p =
    checkRet
        Fd
        (c_socket f t p)

close :: Fd -> IO (Either Errno ())
close fd =
    checkRet
        (const ())
        (c_close fd)

connect :: Fd -> CSockAddr -> CSockLen -> IO (Either Errno ())
connect fd addr len =
    checkRet
        (const ())
        (c_connect fd addr len)

shutdown :: Fd -> CInt -> IO (Either Errno ())
shutdown fd how =
    checkRet
        (const ())
        (c_shutdown fd how)

bind :: Fd -> CSockAddr -> CSockLen -> IO (Either Errno ())
bind fd addr len =
    checkRet
        (const ())
        (c_bind fd addr len)

listen :: Fd -> CInt -> IO (Either Errno ())
listen fd backlog =
    checkRet
        (const ())
        (c_listen fd backlog)

accept :: Fd -> CSockAddr -> Ptr CSockLen -> IO (Either Errno Fd)
accept fd addr lenptr =
    checkRet
        Fd
        (c_accept fd addr lenptr)

recv :: Fd -> Ptr Word8 -> CSize -> CInt -> IO (Either Errno CInt)
recv fd buf sz flags =
    checkRet
        id
        (c_recv fd buf sz (#const MSG_WAITALL))--flags)

recvfrom :: Fd
         -> Ptr Word8 -> CSize
         -> CInt
         -> CSockAddr -> CSockLen
         -> IO (Either Errno CInt)
recvfrom fd buf sz flags add len =
    checkRet
        id
        (c_recvfrom fd buf sz flags add len)

send :: Fd -> Ptr Word8 -> CSize -> CInt -> IO (Either Errno CInt)
send fd addr size flags =
    checkRet
        id
        (c_send fd addr size flags)

sendto :: Fd -> Ptr Word8 -> CSize -> CInt -> CSockAddr -> CSockLen -> IO (Either Errno CInt)
sendto fd buf sz flags addr len =
    checkRet
        id
        (c_sendto fd buf sz flags addr len)

foreign import ccall unsafe "socket"
    c_socket :: CInt -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "close"
    c_close :: Fd -> IO CInt

foreign import ccall unsafe "bind"
    c_bind :: Fd -> CSockAddr -> CSockLen -> IO CInt

foreign import ccall unsafe "connect"
    c_connect :: Fd -> CSockAddr -> CSockLen -> IO CInt

foreign import ccall unsafe "accept"
    c_accept :: Fd -> CSockAddr -> Ptr CSockLen -> IO CInt

foreign import ccall unsafe "listen"
    c_listen :: Fd -> CInt -> IO CInt

foreign import ccall unsafe "send"
    c_send :: Fd -> Ptr a -> CSize -> CInt -> IO CInt -- == CSSize

foreign import ccall unsafe "shutdown"
    c_shutdown :: Fd -> CInt -> IO CInt

foreign import ccall unsafe "sendto"
    c_sendto :: Fd -> Ptr Word8 -> CSize -> CInt -> CSockAddr -> CSockLen -> IO CInt

foreign import ccall unsafe "recv"
    c_recv :: Fd -> Ptr Word8 -> CSize -> CInt -> IO CInt

foreign import ccall unsafe "recvfrom"
    c_recvfrom :: Fd -> Ptr Word8 -> CSize -> CInt -> CSockAddr -> CSockLen -> IO CInt
