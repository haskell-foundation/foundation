/*
 * Copyright (C) 2015-2017 Nicolas Di Prima <nicolas@di-prima.fr>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef CBITS_FOUNDATION_NETWORK_H_
# define CBITS_FOUNDATION_NETWORK_H_

// get the current os information
# include "foundation_system.h"

# if defined(FOUNDATION_SYSTEM_WINDOWS)

#  include <stdint.h>
#  include <stdio.h>
#  include <winsock2.h>

#  ifndef _WIN32_WINNT
#   define _WIN32_WINNT 0x0501
#  endif
#  if (_WIN32_WINNT < 0x0501)
#   undef _WIN32_WINNT
#   define _WIN32_WINNT 0x0501
#  endif

#  include <ws2tcpip.h>

/* Values for this flags taken from
   http://sourceforge.net/p/mingw-w64/mailman/message/33056995/.
   According to MSDN documentation they are supported on
   Windows Vista or higher. This definitions may be removed
   when MinGW finally ships them.
*/

#  ifndef AI_PASSIVE
#   define AI_PASSIVE                  0x00000001
#  endif
#  ifndef AI_CANONNAME
#   define AI_CANONNAME                0x00000002
#  endif
#  ifndef AI_NUMERICHOST
#   define AI_NUMERICHOST              0x00000004
#  endif
#  ifndef AI_NUMERICSERV
#   define AI_NUMERICSERV              0x00000008
#  endif
#  ifndef AI_ALL
#   define AI_ALL                      0x00000100
#  endif
#  ifndef AI_ADDRCONFIG
#   define AI_ADDRCONFIG               0x00000400
#  endif
#  ifndef AI_V4MAPPED
#   define AI_V4MAPPED                 0x00000800
#  endif
#  ifndef AI_NON_AUTHORITATIVE
#   define AI_NON_AUTHORITATIVE        0x00004000
#  endif
#  ifndef AI_SECURE
#   define AI_SECURE                   0x00008000
#  endif
#  ifndef AI_RETURN_PREFERRED_NAMES
#   define AI_RETURN_PREFERRED_NAMES   0x00010000
#  endif

#  define SEOK                   0
#  define SEINTR                 WSAEINTR
#  define SEAGAIN                WSATRY_AGAIN
#  define SEWOULDBLOCK           WSAEWOULDBLOCK
#  define SEBADF                 WSAEBADF
#  define SEINVAL                WSAEINVAL
#  define SEINPROGRESS           WSAEINPROGRESS
#  define SEPROTONOSUPPORT       WSAEPROTONOSUPPORT
#  define SECONNREFUSED          WSAECONNREFUSED
#  define SENETUNREACH           WSAENETUNREACH
#  define SENOTCONN              WSAENOTCONN
#  define SEALREADY              WSAEALREADY
#  define SEISCONN               WSAEISCONN
#  define SETIMEDOUT             WSAETIMEDOUT
#  define SEPIPE                 WSAECONNABORTED
#  define SEOPNOTSUPP            WSAEOPNOTSUPP
#  define SENOTSOCK              WSAENOTSOCK
#  define SEHOSTUNREACH          WSAEHOSTUNREACH
#  define SEHOSTDOWN             WSAEHOSTDOWN
#  define SETOOMANYREFS          WSAETOOMANYREFS
#  define SESHUTDOWN             WSAESHUTDOWN
#  define SENOBUFS               WSAENOBUFS
#  define SENETRESET             WSAENETRESET
#  define SENETDOWN              WSAENETDOWN
#  define SECONNABORTED          WSAECONNABORTED
#  define SECONNRESET            WSAECONNRESET
#  define SEADDRNOTAVAIL         WSAEADDRNOTAVAIL
#  define SEADDRINUSE            WSAEADDRINUSE
#  define SEAFNOSUPPORT          WSAEAFNOSUPPORT
#  define SEPFNOSUPPORT          WSAEPFNOSUPPORT
#  define SESOCKTNOSUPPORT       WSAESOCKTNOSUPPORT
#  define SENOPROTOOPT           WSAENOPROTOOPT
#  define SEPROTOTYPE            WSAEPROTOTYPE
#  define SEMSGSIZE              WSAEMSGSIZE
#  define SEDESTADDRREQ          WSAEDESTADDRREQ

# elif defined(FOUNDATION_SYSTEM_UNIX)

#  include <stdint.h>
#  include <unistd.h>
#  include <fcntl.h>
#  include <errno.h>
#  include <string.h>

#  include "sys/types.h"
#  include "sys/socket.h"
#  include "sys/un.h"
#  include "netinet/in.h"
#  include "netdb.h"

#  define SEOK                   0
#  define SEINTR                 EINTR
#  define SEAGAIN                EAGAIN
#  define SEWOULDBLOCK           EWOULDBLOCK
#  define SEBADF                 EBADF
#  define SEINVAL                EINVAL
#  define SEINPROGRESS           EINPROGRESS
#  define SEPROTONOSUPPORT       EPROTONOSUPPORT
#  define SECONNREFUSED          ECONNREFUSED
#  define SENETUNREACH           ENETUNREACH
#  define SENOTCONN              ENOTCONN
#  define SEALREADY              EALREADY
#  define SEISCONN               EISCONN
#  define SETIMEDOUT             ETIMEDOUT
#  define SEPIPE                 EPIPE
#  define SEOPNOTSUPP            EOPNOTSUPP
#  define SENOTSOCK              ENOTSOCK
#  define SEDESTADDRREQ          EDESTADDRREQ
#  define SEMSGSIZE              EMSGSIZE
#  define SEPROTOTYPE            EPROTOTYPE
#  define SENOPROTOOPT           ENOPROTOOPT
#  define SESOCKTNOSUPPORT       ESOCKTNOSUPPORT
#  define SEPFNOSUPPORT          EPFNOSUPPORT
#  define SEAFNOSUPPORT          EAFNOSUPPORT
#  define SEADDRINUSE            EADDRINUSE
#  define SEADDRNOTAVAIL         EADDRNOTAVAIL
#  define SENETDOWN              ENETDOWN
#  define SENETRESET             ENETRESET
#  define SECONNABORTED          ECONNABORTED
#  define SECONNRESET            ECONNRESET
#  define SENOBUFS               ENOBUFS
#  define SESHUTDOWN             ESHUTDOWN
#  define SETOOMANYREFS          ETOOMANYREFS
#  define SEHOSTDOWN             EHOSTDOWN
#  define SEHOSTUNREACH          EHOSTUNREACH

# endif // !defined(FOUNDATION_SYSTEM_UNIX)

# ifndef MSG_EOR
#  define MSG_EOR 0
# endif

# ifndef MSG_NOSIGNAL
#  define MSG_NOSIGNAL 0
# endif

# ifndef MSG_WAITALL
#  define MSG_WAITALL 0
# endif

# ifndef EAI_SYSTEM
#  define EAI_SYSTEM 0
# endif

# ifndef MSG_NOSIGNAL
#  define MSG_NOSIGNAL           0
# endif

/* Value for this flag taken from the former Haskell network
   library. Remove if MinGW fixed this.
*/
# ifndef IPV6_V6ONLY
#  define IPV6_V6ONLY 27
# endif

int hs_socket( int  domain
             , int  type
             , int  protocol
             );
int hs_connect( int                    fd
              , const struct sockaddr* name
              , int                    namelen
              );
int hs_bind( int                    fd
           , const struct sockaddr* name
           , int                    namelen
           );
int hs_listen( int  fd
             , int  backlog
             );
int hs_accept( int              fd
             , struct sockaddr* addr
             , socklen_t*       addrlen
             );
int hs_close( int  fd
            );

int hs_send( int         fd
           , const void* buf
           , size_t      len
           , int         flags
           );
int hs_recv( int    fd
           , void*  buf
           , size_t len
           , int    flags
           );
int hs_sendto( int                    fd
             , const void*            buf
             , size_t                 len
             , int                    flags
             , const struct sockaddr* dest_addr
             , int                    addrlen
             );
int hs_recvfrom( int              fd
               , void*            buf
               , size_t           len
               , int              flags
               , struct sockaddr* src_addr
               , socklen_t*       addrlen
               );

int hs_getsockopt( int        fd
                 , int        level
                 , int        option_name
                 , void*      option_value
                 , socklen_t* option_len
                 );
int hs_setsockopt( int         fd
                 , int         level
                 , int         option_name
                 , const void* option_value
                 , socklen_t   option_len
                 );

int hs_sockerrno();

char* hs_sockstrerror(int);


int foundation_network_socket_setup();


#endif // ! CBITS_FOUNDATION_NETWORK_H_
