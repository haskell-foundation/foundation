#include "foundation_system.h"

module Foundation.System.Bindings.Posix2008
  ( -- * Network
    -- ** Address Info
    CAddrInfo
  , sysPosixGetAddrInfo
  , sysPosixFreeAddrInfo
    -- *** errors
  , CAddrInfoError
  , sysPosix_EAI_ADDRFAMILY
  , sysPosix_EAI_AGAIN
  , sysPosix_EAI_BADFLAGS
  , sysPosix_EAI_FAIL
  , sysPosix_EAI_FAMILY
  , sysPosix_EAI_MEMORY
  , sysPosix_EAI_NODATA
  , sysPosix_EAI_NONAME
  , sysPosix_EAI_SERVICE
  , sysPosix_EAI_SOCKTYPE
  , sysPosix_EAI_SYSTEM

    -- ** Service
  , CServEnt
  , cservent_name_ptr
  , cservent_aliases_ptr
  , cservent_port_ptr
  , cservent_proto_ptr

  , sysPosixSetServEnt
  , sysPosixGetServEnt
  , sysPosixEndServEnt
  , sysPosixGetServByName
  , sysPosixGetServByPort
  )
  where

import Foundation.Internal.Base
import Foundation.Primitive.Endianness
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String (CString)

#if defined(FOUNDATION_SYSTEM_WINDOWS)
# include <winsock2.h>
# include <netdb.h>
#else
# include <sys/types.h>
# include <sys/socket.h>
# include <netdb.h>
#endif

newtype CAddrInfoError = CAddrInfoError CInt
  deriving (Show, Eq, Ord)

-- | The specified network host does not have any network addresses
-- in the requested address family.
sysPosix_EAI_ADDRFAMILY :: CAddrInfoError
#ifdef EAI_ADDRFAMILY
sysPosix_EAI_ADDRFAMILY = CAddrInfoError (#const EAI_ADDRFAMILY)
#else
sysPosix_EAI_ADDRFAMILY = sysPosix_EAI_SYSTEM
#endif

-- | The name server returned a temporary failure indication.  Try
-- again later.
sysPosix_EAI_AGAIN :: CAddrInfoError
sysPosix_EAI_AGAIN = CAddrInfoError (#const EAI_AGAIN)

-- | hints.ai_flags contains invalid flags; or, hints.ai_flags
-- included AI_CANONNAME and name was NULL.
sysPosix_EAI_BADFLAGS :: CAddrInfoError
sysPosix_EAI_BADFLAGS = CAddrInfoError (#const EAI_BADFLAGS)
-- | The name server returned a permanent failure indication.
sysPosix_EAI_FAIL :: CAddrInfoError
sysPosix_EAI_FAIL = CAddrInfoError (#const EAI_FAIL)

-- | The requested address family is not supported.
sysPosix_EAI_FAMILY :: CAddrInfoError
sysPosix_EAI_FAMILY = CAddrInfoError (#const EAI_FAMILY)

-- | Out of memory.
sysPosix_EAI_MEMORY :: CAddrInfoError
sysPosix_EAI_MEMORY = CAddrInfoError (#const EAI_MEMORY)

-- | The specified network host exists, but does not have any
-- network addresses defined.
sysPosix_EAI_NODATA :: CAddrInfoError
#ifdef EAI_NODATA
sysPosix_EAI_NODATA = CAddrInfoError (#const EAI_NODATA)
#else
sysPosix_EAI_NODATA = sysPosix_EAI_SYSTEM
#endif

-- | The node or service is not known; or both node and service are
-- NULL; or AI_NUMERICSERV was specified in hints.ai_flags and
-- service was not a numeric port-number string.
sysPosix_EAI_NONAME :: CAddrInfoError
sysPosix_EAI_NONAME = CAddrInfoError (#const EAI_NONAME)

-- | The requested service is not available for the requested
-- socket type.  It may be available through another socket type.
-- For example, this error could occur if service was "shell" (a
-- service available only on stream sockets), and either
-- hints.ai_protocol was IPPROTO_UDP, or hints.ai_socktype was
-- SOCK_DGRAM; or the error could occur if service was not NULL,
-- and hints.ai_socktype was SOCK_RAW (a socket type that does
-- not support the concept of services).
sysPosix_EAI_SERVICE :: CAddrInfoError
sysPosix_EAI_SERVICE = CAddrInfoError (#const EAI_SERVICE)

-- | The requested socket type is not supported.  This could occur,
-- for example, if hints.ai_socktype and hints.ai_protocol are
-- inconsistent (e.g., SOCK_DGRAM and IPPROTO_TCP, respectively).
sysPosix_EAI_SOCKTYPE :: CAddrInfoError
sysPosix_EAI_SOCKTYPE = CAddrInfoError (#const EAI_SOCKTYPE)

-- | Other system error, check errno for details.
sysPosix_EAI_SYSTEM :: CAddrInfoError
sysPosix_EAI_SYSTEM = CAddrInfoError (#const EAI_SYSTEM)

-- | The addrinfo structure used by getaddrinfo() contains the following
-- fields:
--
--     struct addrinfo {
--         int              ai_flags;
--         int              ai_family;
--         int              ai_socktype;
--         int              ai_protocol;
--         socklen_t        ai_addrlen;
--         struct sockaddr *ai_addr;
--         char            *ai_canonname;
--         struct addrinfo *ai_next;
--     };
--
-- The hints argument points to an addrinfo structure that specifies
-- criteria for selecting the socket address structures returned in the
-- list pointed to by res.  If hints is not NULL it points to an
-- addrinfo structure whose ai_family, ai_socktype, and ai_protocol
-- specify criteria that limit the set of socket addresses returned by
-- getaddrinfo(), as follows:
--
-- ai_family   This field specifies the desired address family for the
--             returned addresses.  Valid values for this field include
--             AF_INET and AF_INET6.  The value AF_UNSPEC indicates that
--             getaddrinfo() should return socket addresses for any
--             address family (either IPv4 or IPv6, for example) that
--             can be used with node and service.
--
-- ai_socktype This field specifies the preferred socket type, for
--             example SOCK_STREAM or SOCK_DGRAM.  Specifying 0 in this
--             field indicates that socket addresses of any type can be
--             returned by getaddrinfo().
--
-- ai_protocol This field specifies the protocol for the returned socket
--             addresses.  Specifying 0 in this field indicates that
--             socket addresses with any protocol can be returned by
--             getaddrinfo().
--
-- ai_flags    This field specifies additional options, described below.
--             Multiple flags are specified by bitwise OR-ing them
--             together.
data CAddrInfo

foreign import ccall unsafe "getaddrinfo"
    sysPosixGetAddrInfo
        :: CString
            -- ^ The node Name
        -> CString
            -- ^ the requested service
        -> Ptr CAddrInfo
            -- ^ a hint
        -> Ptr (Ptr CAddrInfo)
            -- ^ Ptr for the array of result to be allocated
        -> IO CInt

foreign import ccall unsafe "freeaddrinfo"
    sysPosixFreeAddrInfo :: Ptr CAddrInfo
                         -> IO ()

data CServEnt

-- | official service name
cservent_name_ptr :: Ptr CServEnt -> Ptr (Ptr a)
cservent_name_ptr = (#ptr struct servent, s_name)
-- | list of aliases for the service
cservent_aliases_ptr :: Ptr CServEnt -> Ptr (Ptr (Ptr a))
cservent_aliases_ptr = (#ptr struct servent, s_aliases)
-- | port number
cservent_port_ptr :: Ptr CServEnt -> Ptr a
cservent_port_ptr = (#ptr struct servent, s_port)
-- | protocol to use
cservent_proto_ptr :: Ptr CServEnt -> Ptr (Ptr a)
cservent_proto_ptr = (#ptr struct servent, s_proto)

foreign import ccall unsafe "setservent"
    sysPosixSetServEnt :: CInt -> IO ()
foreign import ccall unsafe "endservent"
    sysPosixEndServEnt :: IO ()
foreign import ccall unsafe "getservent"
    sysPosixGetServEnt :: IO (Ptr CServEnt)
foreign import ccall unsafe "getservbyname"
    sysPosixGetServByName :: CString -> CString -> IO (Ptr CServEnt)
foreign import ccall unsafe "getservbyport"
    sysPosixGetServByPort :: BE Word16 -> CString -> IO (Ptr CServEnt)
