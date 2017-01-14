module Foundation.Network.Socket.Internal
    ( Socket(..)
    , retryWith

    , CSockAddr
    , CSockLen

      -- * Error
    , module E

      -- * Flags
    , Flag
    , flagWaitAll, flagOutOfBand, flagNoSignal, flagEndOfRecord

      -- * Socket
    , Fd(..)
    , socket
    , close
    , connect
    , bind
    , listen
    , accept
    , send
    , sendto
    , recv
    , recvfrom

    , module X
    , socketWaitRead
    , socketWaitWrite
    , socketWaitConnect
    ) where

#include "foundation_network.h"

import Control.Monad (when)
import Control.Concurrent.MVar
import Data.Bits ((.|.))
import Foreign.C.Types
import System.Posix.Types (Fd(..))

import Foundation.Internal.Base
import Foundation.Network.Socket.Internal.Protocol as X
import Foundation.Network.Socket.Internal.Type as X
import Foundation.Network.Socket.Internal.Family as X
import Foundation.Network.Socket.Internal.Error as E

#if defined(FOUNDATION_SYSTEM_WINDOWS)
import Foreign.C.Types (CInt(..))
import Control.Concurrent (threadDelay)
#elif defined(FOUNDATION_SYSTEM_UNIX)
import GHC.Conc (threadWaitRead, threadWaitWrite)
#endif

newtype Socket s = Socket (MVar Fd)

data Return a = Error SocketError | Retry (IO ()) | Ok a

retryWith :: Socket s
          -> (SocketError -> IO a)
          -> (Fd -> IO ())
          -> (Fd -> IO (Either SocketError a))
          -> IO a
retryWith s@(Socket mvar) handleErr waitFunction action = do
    e <- withMVar mvar $ \fd -> do
            when (fd < Fd 0) (E.throwSocketError E.eBadFileDescriptor)
            e <- action fd
            return $ case e of
                Left err | err == eAgain       -> Retry (waitFunction fd)
                         | err == eWouldBlock  -> Retry (waitFunction fd)
                         | err == eInterrupted -> Retry (waitFunction fd)
                         | otherwise -> Error err
                Right a -> Ok a
    case e of
        Retry wait -> wait >> retryWith s handleErr waitFunction action
        Error err  -> handleErr err
        Ok a       -> return a

type CSockAddr = Ptr Word8
type CSockLen  = CInt

newtype Flag = Flag CInt
  deriving (Show, Eq, Ord, Typeable)
instance Monoid Flag where
    mempty = Flag 0
    mappend (Flag c1) (Flag c2) = Flag $ c1 .|. c2

flagWaitAll :: Flag
flagWaitAll = Flag (#const MSG_WAITALL)

flagOutOfBand :: Flag
flagOutOfBand = Flag (#const MSG_OOB)

flagNoSignal :: Flag
flagNoSignal = Flag (#const MSG_NOSIGNAL)

flagEndOfRecord :: Flag
flagEndOfRecord = Flag (#const MSG_EOR)

socket :: CInt -> CInt -> CInt -> IO (Either SocketError Fd)
socket f t p =
    checkRet
        Fd
        (c_socket f t p)

close :: Fd -> IO (Either SocketError ())
close fd =
    checkRet
        (const ())
        (c_close fd)

connect :: Fd -> CSockAddr -> CSockLen -> IO (Either SocketError ())
connect fd addr len =
    checkRet
        (const ())
        (c_connect fd addr len)

bind :: Fd -> CSockAddr -> CSockLen -> IO (Either SocketError ())
bind fd addr len =
    checkRet
        (const ())
        (c_bind fd addr len)

listen :: Fd -> CInt -> IO (Either SocketError ())
listen fd backlog =
    checkRet
        (const ())
        (c_listen fd backlog)

accept :: Fd -> CSockAddr -> Ptr CSockLen -> IO (Either SocketError Fd)
accept fd addr lenptr =
    checkRet
        Fd
        (c_accept fd addr lenptr)

recv :: Fd -> Ptr Word8 -> CSize -> Flag -> IO (Either SocketError CInt)
recv fd buf sz (Flag flags) =
    checkRet
        id
        (c_recv fd buf sz flags)

recvfrom :: Fd
         -> Ptr Word8 -> CSize
         -> Flag
         -> CSockAddr -> CSockLen
         -> IO (Either SocketError CInt)
recvfrom fd buf sz (Flag flags) add len =
    checkRet
        id
        (c_recvfrom fd buf sz flags add len)

send :: Fd -> Ptr Word8 -> CSize -> Flag -> IO (Either SocketError CInt)
send fd addr size (Flag flags) =
    checkRet
        id
        (c_send fd addr size flags)

sendto :: Fd -> Ptr Word8 -> CSize -> Flag -> CSockAddr -> CSockLen -> IO (Either SocketError CInt)
sendto fd buf sz (Flag flags) addr len =
    checkRet
        id
        (c_sendto fd buf sz flags addr len)

socketWaitRead :: Fd -> IO ()
#if defined(FOUNDATION_SYSTEM_WINDOWS)
socketWaitRead _ = threadDelay 1
#elif defined(FOUNDATION_SYSTEM_UNIX)
socketWaitRead = threadWaitRead
#endif

socketWaitWrite :: Fd -> IO ()
#if defined(FOUNDATION_SYSTEM_WINDOWS)
socketWaitWrite _ = threadDelay 1
#elif defined(FOUNDATION_SYSTEM_UNIX)
socketWaitWrite = threadWaitWrite
#endif

socketWaitConnect :: Fd -> IO ()
#if defined(FOUNDATION_SYSTEM_WINDOWS)
socketWaitConnect fd = do
    st <- c_connect_status fd
    case st of
        0 -> return () -- connected
        1 -> threadDelay 100 >> socketWaitConnect fd
        _ -> socketErrno >>= throwSocketError
#elif defined(FOUNDATION_SYSTEM_UNIX)
socketWaitConnect = threadWaitWrite
#endif

foreign import ccall unsafe "hs_socket"
    c_socket :: CInt -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "hs_close"
    c_close :: Fd -> IO CInt

foreign import ccall unsafe "hs_bind"
    c_bind :: Fd -> CSockAddr -> CSockLen -> IO CInt

foreign import ccall unsafe "hs_connect"
    c_connect :: Fd -> CSockAddr -> CSockLen -> IO CInt

foreign import ccall unsafe "hs_accept"
    c_accept :: Fd -> CSockAddr -> Ptr CSockLen -> IO CInt

foreign import ccall unsafe "hs_listen"
    c_listen :: Fd -> CInt -> IO CInt

foreign import ccall unsafe "hs_send"
    c_send :: Fd -> Ptr a -> CSize -> CInt -> IO CInt -- == CSSize

foreign import ccall unsafe "hs_sendto"
    c_sendto :: Fd -> Ptr Word8 -> CSize -> CInt -> CSockAddr -> CSockLen -> IO CInt

foreign import ccall unsafe "hs_recv"
    c_recv :: Fd -> Ptr Word8 -> CSize -> CInt -> IO CInt

foreign import ccall unsafe "hs_recvfrom"
    c_recvfrom :: Fd -> Ptr Word8 -> CSize -> CInt -> CSockAddr -> CSockLen -> IO CInt

#if defined(FOUNDATION_SYSTEM_WINDOWS)
foreign import ccall unsafe "hs_connect_status"
    c_connect_status :: Fd -> IO CInt
#endif // ! defined(FOUNDATION_SYSTEM_WINDOWS)
