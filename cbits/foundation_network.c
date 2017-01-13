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

#include "foundation_network.h"

#if defined(FOUNDATION_SYSTEM_WINDOWS)

int foundation_network_socket_setup()
{
    static int has_already_been_initialised = 0;

    if (!has_already_been_initialised) {
        WSADATA wsaData;

        int ini = WSAStartup(MAKEWORD(2,2), &wsaData);
        if (ini != NO_ERROR) {
            WSACleanup();
            return -1;
        } else {
            has_already_been_initialised = 1;
        }
    }
    return 0;
}

int hs_socket( int  domain
             , int  type
             , int  protocol
             )
{
    int fd = 1;
    if (!foundation_network_socket_setup()) {
        fd = socket(domain, type, protocol);
        if (fd >= 0) {
            u_long iMode = 1;
            if (!ioctlsocket(fd, FIONBIO, &iMode)) {
                return fd;
            } else {
                hs_close(fd);
                fd = -1;
            }
        }
    }
    return fd;
};

/*
 * Determine a sockets connection status.
 *
 * Return values:
 *   0: connection established
 *   1: connection pending
 *   2: connection failed
 *   3: select or getsockopt failed
 *
 * The operation will block for the least possible time interval (1 micro second)
 * and is not used as it is supposed to be used as we don't want to block.
 * Haskell's RTS is used to wait and poll this operation from time to time.
 */
int hs_connect_status(int fd)
{
    int errlen = sizeof(int);
    int err;
    struct timeval timeout = {0,1};
    fd_set writefds;
    fd_set exceptfds;

    FD_ZERO(&writefds);
    FD_ZERO(&exceptfds);
    FD_SET(fd, &writefds);
    FD_SET(fd, &exceptfds);

    switch (select(fd, NULL, &writefds, &exceptfds, &timeout))
    {
        case 0: return 1;   // Connection pending.
        case 1:
            if (FD_ISSET(fd, &writefds))
            {
                return 0; // Connection established.
            }
            if ( FD_ISSET(fd, &exceptfds) &&
                 !getsockopt(fd, SOL_SOCKET, SO_ERROR, (char*) &err, &errlen)
               )
            {
                return 2; // Connection failed.
            }
        default:
            return -1; // select or getsockopt failed.
    }
}

int hs_accept( int              fd
             , struct sockaddr* addr
             , socklen_t*       addrlen
             )
{
    int ft = accept(fd, addr, addrlen);
    if (ft >= 0) {
        u_long iMode = 1;
        if (!ioctlsocket(ft, FIONBIO, &iMode)) {
            return ft;
        } else {
            hs_close(ft);
            return -1;
        }
    }
    return ft;
}

#elif defined(FOUNDATION_SYSTEM_UNIX)

int foundation_network_socket_setup()
{
    return 0;
}
int hs_socket( int  domain
             , int  type
             , int  protocol
             )
{
#if defined(__USE_GNU)
    // On Linux, there is an optimized way to set a socket non-blocking
    return socket(domain, type | SOCK_NONBLOCK, protocol);
#else // ! defined(__USE_GNU)
    // This is the regular way via fcntl
    int fd = socket(domain, type, protocol);
    if (fd >= 0) {
        int flags = fcntl(fd, F_GETFL, 0);
        if (flags >= 0 && !fcntl(fd, F_SETFL, flags | O_NONBLOCK)) {
            return fd;
        } else {
            close(fd);
            fd = -1;
        }
    }
    return fd;
#endif
}

int hs_accept( int              fd
             , struct sockaddr* addr
             , socklen_t*       addrlen
             )
{
# if defined(__USE_GNU)
    // On Linux, there is an optimized way to set a socket non-blocking
    return accept4(fd, addr, addrlen, SOCK_NONBLOCK);
# else // !defined(__USE_GNU)
    // This is the canonical way in absence of accept4
    int fd_accepted = accept(fd, addr, addrlen);
    if (fd_accepted >= 0) {
        int flags = fcntl(fd_accepted, F_GETFL, 0);
        if (flags >= 0 && !fcntl(fd_accepted, F_SETFL, flags | O_NONBLOCK)) {
            return fd_accepted;
        } else {
            close(fd_accepted);
            fd_accepted = -1;
        }
    }
    return fd_accepted;
# endif
}

#endif // ! defined(FOUNDATION_SYSTEM_UNIX)

int hs_connect( int                    fd
              , const struct sockaddr* name
              , int                    namelen
              )
{
    return connect(fd, name, namelen);
}

int hs_bind( int                    fd
           , const struct sockaddr* name
           , int                    namelen
           )
{
    return bind(fd, name, namelen);
}

int hs_listen( int  fd
             , int  backlog
             )
{
    return listen(fd, backlog);
}

int hs_close(int fd)
{
#if defined(FOUNDATION_SYSTEM_WINDOWS)
  return closesocket(fd);
#elif defined(FOUNDATION_SYSTEM_UNIX)
  return close(fd);
#endif // ! defined(FOUNDATION_SYSTEM_UNIX)
}

int hs_send( int         fd
           , const void* buf
           , size_t      len
           , int         flags
           )
{
  return send(fd, buf, len, flags);
}

int hs_recv( int    fd
           , void*  buf
           , size_t len
           , int    flags
           )
{
  return recv(fd, buf, len, flags);
}

int hs_sendto( int                    fd
             , const void*            buf
             , size_t                 len
             , int                    flags
             , const struct sockaddr* dest_addr
             , int                    addrlen
             )
{
  return sendto(fd, buf, len, flags, dest_addr, addrlen);
}

int hs_recvfrom( int              fd
               , void*            buf
               , size_t           len
               , int              flags
               , struct sockaddr* src_addr
               , socklen_t*       addrlen
               )
{
  return recvfrom(fd, buf, len, flags, src_addr, addrlen);
}

int hs_getsockopt( int        fd
                 , int        level
                 , int        option_name
                 , void*      option_value
                 , socklen_t* option_len
                 )
{
  return getsockopt(fd, level, option_name, option_value, option_len);
}

int hs_setsockopt( int         fd
                 , int         level
                 , int         option_name
                 , const void* option_value
                 , socklen_t   option_len
                 )
{
  return setsockopt(fd, level, option_name, option_value, option_len);
}

int hs_sockerrno()
{
#if defined(FOUNDATION_SYSTEM_WINDOWS)
    return WSAGetLastError();
#elif defined(FOUNDATION_SYSTEM_UNIX)
    return errno;
#endif // ! defined(FOUNDATION_SYSTEM_UNIX)
}

char* hs_sockstrerror(int user_errno)
{
#if defined(FOUNDATION_SYSTEM_WINDOWS)
    // TODO
    user_errno = user_errno;
    return "";
#elif defined(FOUNDATION_SYSTEM_UNIX)
    return strerror(user_errno);
#endif // ! defined(FOUNDATION_SYSTEM_UNIX)
}
