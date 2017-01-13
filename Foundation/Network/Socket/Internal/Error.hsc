module Foundation.Network.Socket.Internal.Error where

import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)

import Foundation.Internal.Base

#include "foundation_network.h"

newtype SocketError = SocketError CInt
    deriving (Eq,Typeable)
instance Show SocketError where
    show (SocketError errno) = unsafePerformIO $ c_sockstrerror errno >>= peekCString
instance Exception SocketError

throwSocketError :: SocketError -> IO a
throwSocketError = throwIO

checkRet :: (Integral ret, Eq ret)
         => (ret -> a) -> IO ret -> IO (Either SocketError a)
checkRet mapper action = do
    r <- action
    if r == fromInteger (-1)
        then Left . SocketError <$> c_sockerrno
        else return $ Right $ mapper r

eOk                         :: SocketError
eOk                          = SocketError (#const SEOK)

eInterrupted                :: SocketError
eInterrupted                 = SocketError (#const SEINTR)

eBadFileDescriptor          :: SocketError
eBadFileDescriptor           = SocketError (#const SEBADF)

eInvalid                    :: SocketError
eInvalid                     = SocketError (#const SEINVAL)

ePipe                       :: SocketError
ePipe                        = SocketError (#const SEPIPE)

eWouldBlock                 :: SocketError
eWouldBlock                  = SocketError (#const SEWOULDBLOCK)

eAgain                      :: SocketError
eAgain                       = SocketError (#const SEAGAIN)

eNotSocket                  :: SocketError
eNotSocket                   = SocketError (#const SENOTSOCK)

eDestinationAddressRequired :: SocketError
eDestinationAddressRequired  = SocketError (#const SEDESTADDRREQ)

eMessageSize                :: SocketError
eMessageSize                 = SocketError (#const SEMSGSIZE)

eProtocolType               :: SocketError
eProtocolType                = SocketError (#const SEPROTOTYPE)

eNoProtocolOption           :: SocketError
eNoProtocolOption            = SocketError (#const SENOPROTOOPT)

eProtocolNotSupported       :: SocketError
eProtocolNotSupported        = SocketError (#const SEPROTONOSUPPORT)

eSocketTypeNotSupported     :: SocketError
eSocketTypeNotSupported      = SocketError (#const SESOCKTNOSUPPORT)

eOperationNotSupported      :: SocketError
eOperationNotSupported       = SocketError (#const SEOPNOTSUPP)

eProtocolFamilyNotSupported :: SocketError
eProtocolFamilyNotSupported  = SocketError (#const SEPFNOSUPPORT)

eAddressFamilyNotSupported  :: SocketError
eAddressFamilyNotSupported   = SocketError (#const SEAFNOSUPPORT)

eAddressInUse               :: SocketError
eAddressInUse                = SocketError (#const SEADDRINUSE)

eAddressNotAvailable        :: SocketError
eAddressNotAvailable         = SocketError (#const SEADDRNOTAVAIL)

eNetworkDown                :: SocketError
eNetworkDown                 = SocketError (#const SENETDOWN)

eNetworkUnreachable         :: SocketError
eNetworkUnreachable          = SocketError (#const SENETUNREACH)

eNetworkReset               :: SocketError
eNetworkReset                = SocketError (#const SENETRESET)

eConnectionAborted          :: SocketError
eConnectionAborted           = SocketError (#const SECONNABORTED)

eConnectionReset            :: SocketError
eConnectionReset             = SocketError (#const SECONNRESET)

eNoBufferSpace              :: SocketError
eNoBufferSpace               = SocketError (#const SENOBUFS)

eIsConnected                :: SocketError
eIsConnected                 = SocketError (#const SEISCONN)

eNotConnected               :: SocketError
eNotConnected                = SocketError (#const SENOTCONN)

eShutdown                   :: SocketError
eShutdown                    = SocketError (#const SESHUTDOWN)

eTooManyReferences          :: SocketError
eTooManyReferences           = SocketError (#const SETOOMANYREFS)

eTimedOut                   :: SocketError
eTimedOut                    = SocketError (#const SETIMEDOUT)

eConnectionRefused          :: SocketError
eConnectionRefused           = SocketError (#const SECONNREFUSED)

eHostDown                   :: SocketError
eHostDown                    = SocketError (#const SEHOSTDOWN)

eHostUnreachable            :: SocketError
eHostUnreachable             = SocketError (#const SEHOSTUNREACH)

eAlready                    :: SocketError
eAlready                     = SocketError (#const SEALREADY)

eInProgress                 :: SocketError
eInProgress = SocketError (#const SEINPROGRESS)


foreign import ccall unsafe "hs_sockerrno"
    c_sockerrno :: IO CInt
foreign import ccall unsafe "hs_sockstrerror"
    c_sockstrerror :: CInt -> IO (Ptr CChar)
