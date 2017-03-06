-----------------------------------------------------------------------------
-- |
-- Module      :  Foundation.System.Bindings.Posix
-- Copyright   :  (c) Vincent Hanquez 2014-2017
-- License     :  BSD-style
--
-- Maintainer  :  Vincent Hanquez
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- Functions defined by the POSIX standards
--
-----------------------------------------------------------------------------

module Foundation.System.Bindings.Posix
   where

import Foundation.Internal.Base
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.Error
import Data.Bits
import System.Posix.Types (COff(..), CMode(..))

#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>

type CFd = CInt
type CMemProtFlags = CInt
type CMemMappingFlags = CInt
type CMemAdvice = CInt
type CMemSyncFlags = CInt
type CSysconfName = CInt
type COpenFlags = CInt

data CDir
data CDirent

sysPosix_O_RDONLY
    , sysPosix_O_WRONLY
    , sysPosix_O_RDWR
    , sysPosix_O_NONBLOCK
    , sysPosix_O_APPEND
    , sysPosix_O_CREAT
    , sysPosix_O_TRUNC
    , sysPosix_O_EXCL
    , sysPosix_O_SHLOCK
    , sysPosix_O_EXLOCK
    , sysPosix_O_NOFOLLOW
    , sysPosix_O_SYMLINK
    , sysPosix_O_EVTONLY
    , sysPosix_O_CLOEXEC :: COpenFlags
sysPosix_O_RDONLY   = (#const O_RDONLY)
sysPosix_O_WRONLY   = (#const O_WRONLY)
sysPosix_O_RDWR     = ((#const O_RDONLY) .|. (#const O_WRONLY))
sysPosix_O_NONBLOCK = (#const O_NONBLOCK)
sysPosix_O_APPEND   = (#const O_APPEND)
sysPosix_O_CREAT    = (#const O_CREAT)
sysPosix_O_TRUNC    = (#const O_TRUNC)
sysPosix_O_EXCL     = (#const O_EXCL)
sysPosix_O_SHLOCK   = (#const O_SHLOCK)
sysPosix_O_EXLOCK   = (#const O_EXLOCK)
sysPosix_O_NOFOLLOW = (#const O_NOFOLLOW)
sysPosix_O_SYMLINK  = (#const O_SYMLINK)
sysPosix_O_EVTONLY  = (#const O_EVTONLY)
sysPosix_O_CLOEXEC  = (#const O_CLOEXEC)

sysPosix_PROT_NONE
    , sysPosix_PROT_READ
    , sysPosix_PROT_WRITE
    , sysPosix_PROT_EXEC :: CMemProtFlags
sysPosix_PROT_NONE  = (#const PROT_NONE)
sysPosix_PROT_READ  = (#const PROT_READ)
sysPosix_PROT_WRITE = (#const PROT_WRITE)
sysPosix_PROT_EXEC  = (#const PROT_EXEC)

sysPosix_MAP_SHARED
    , sysPosix_MAP_PRIVATE
    , sysPosix_MAP_FIXED
    , sysPosix_MAP_ANONYMOUS :: CMemMappingFlags
sysPosix_MAP_SHARED    = (#const MAP_SHARED)
sysPosix_MAP_PRIVATE   = (#const MAP_PRIVATE)
sysPosix_MAP_FIXED     = (#const MAP_FIXED)
#ifdef __APPLE__
sysPosix_MAP_ANONYMOUS = (#const MAP_ANON)
#else
sysPosix_MAP_ANONYMOUS = (#const MAP_ANONYMOUS)
#endif

sysPosix_MADV_NORMAL
    , sysPosix_MADV_RANDOM
    , sysPosix_MADV_SEQUENTIAL
    , sysPosix_MADV_WILLNEED
    , sysPosix_MADV_DONTNEED :: CMemAdvice
#if defined(POSIX_MADV_NORMAL)
sysPosix_MADV_NORMAL     = (#const POSIX_MADV_NORMAL)
sysPosix_MADV_RANDOM     = (#const POSIX_MADV_RANDOM)
sysPosix_MADV_SEQUENTIAL = (#const POSIX_MADV_SEQUENTIAL)
sysPosix_MADV_WILLNEED   = (#const POSIX_MADV_WILLNEED)
sysPosix_MADV_DONTNEED   = (#const POSIX_MADV_DONTNEED)
#else
sysPosix_MADV_NORMAL     = (#const MADV_NORMAL)
sysPosix_MADV_RANDOM     = (#const MADV_RANDOM)
sysPosix_MADV_SEQUENTIAL = (#const MADV_SEQUENTIAL)
sysPosix_MADV_WILLNEED   = (#const MADV_WILLNEED)
sysPosix_MADV_DONTNEED   = (#const MADV_DONTNEED)
#endif

sysPosix_MS_ASYNC
    , sysPosix_MS_SYNC
    , sysPosix_MS_INVALIDATE :: CMemSyncFlags
sysPosix_MS_ASYNC      = (#const MS_ASYNC)
sysPosix_MS_SYNC       = (#const MS_SYNC)
sysPosix_MS_INVALIDATE = (#const MS_INVALIDATE)

foreign import ccall unsafe "mmap"
    sysPosixMmap :: Ptr a -> CSize -> CMemProtFlags -> CMemMappingFlags -> CFd -> COff -> IO (Ptr a)

foreign import ccall unsafe "munmap"
    sysPosixMunmap :: Ptr a -> CSize -> IO CInt

#if defined(POSIX_MADV_NORMAL)
foreign import ccall unsafe "posix_madvise"
    sysPosixMadvise :: Ptr a -> CSize -> CMemAdvice -> IO CInt
#else
foreign import ccall unsafe "madvise"
    sysPosixMadvise :: Ptr a -> CSize -> CMemAdvice -> IO CInt
#endif

foreign import ccall unsafe "msync"
    sysPosixMsync :: Ptr a -> CSize -> CMemSyncFlags -> IO CInt

foreign import ccall unsafe "mprotect"
    sysPosixMprotect :: Ptr a -> CSize -> CMemProtFlags -> IO CInt

#ifndef __HAIKU__
foreign import ccall unsafe "mlock"
    sysPosixMlock :: Ptr a -> CSize -> IO CInt
#else
sysPosixMlock :: Ptr a -> CSize -> IO CInt
sysPosixMlock _ _ = return (-1)
#endif

#ifndef __HAIKU__
foreign import ccall unsafe "munlock"
    sysPosixMunlock :: Ptr a -> CSize -> IO CInt
#else
sysPosixMunlock :: Ptr a -> CSize -> IO CInt
sysPosixMunlock _ _ = return (-1)
#endif

sysPosix_SC_PAGESIZE :: CSysconfName
sysPosix_SC_PAGESIZE = (#const _SC_PAGESIZE)

foreign import ccall unsafe "sysconf"
    sysPosixSysconf :: CSysconfName -> CLong
--------------------------------------------------------------------------------
-- files
--------------------------------------------------------------------------------
foreign import ccall unsafe "open"
    sysPosixOpen :: Ptr CChar -> COpenFlags -> CMode -> IO CFd
foreign import ccall unsafe "openat"
    sysPosixOpenAt :: CFd -> Ptr CChar -> COpenFlags -> CMode -> IO CFd
foreign import ccall unsafe "close"
    sysPosixClose :: CFd -> IO CInt

foreign import ccall unsafe "fcntl"
    sysPosixFnctlNoArg :: CFd -> CInt -> IO CInt
foreign import ccall unsafe "fcntl"
    sysPosixFnctlPtr :: CFd -> CInt -> Ptr a -> IO CInt

foreign import ccall unsafe "ftruncate"
    sysPosixFtruncate :: CFd -> COff -> IO CInt

--------------------------------------------------------------------------------
-- directories
--------------------------------------------------------------------------------

foreign import ccall unsafe "opendir"
    sysPosixOpendir :: Ptr CChar -> IO (Ptr CDir)
foreign import ccall unsafe "fdopendir"
    sysPosixFdopendir :: CFd -> IO (Ptr CDir)
foreign import ccall unsafe "readdir"
    sysPosixReaddir :: Ptr CDir -> IO (Ptr CDirent)
foreign import ccall unsafe "readdir_r"
    sysPosixReaddirR :: Ptr CDir -> Ptr CDirent -> Ptr (Ptr CDirent) -> IO CInt
foreign import ccall unsafe "telldir"
    sysPosixTelldir :: Ptr CDir -> IO CLong
foreign import ccall unsafe "seekdir"
    sysPosixSeekdir :: Ptr CDir -> CLong -> IO ()
foreign import ccall unsafe "rewinddir"
    sysPosixRewinddir :: Ptr CDir -> IO ()
foreign import ccall unsafe "closedir"
    sysPosixClosedir :: Ptr CDir -> IO CInt
foreign import ccall unsafe "dirfd"
    sysPosixDirfd :: Ptr CDir -> IO CFd
