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
import System.Posix.Types (COff(..))

#include <sys/mman.h>
#include <unistd.h>

type CFd = CInt
type CMemProtFlags = CInt
type CMemMappingFlags = CInt
type CMemAdvice = CInt
type CMemSyncFlags = CInt
type CSysconfName = CInt

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

foreign import ccall unsafe "sysconf"
    sysPosixSysconf :: CSysconfName -> CLong
