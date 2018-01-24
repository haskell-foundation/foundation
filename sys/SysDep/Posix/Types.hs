{-# OPTIONS_HADDOCK hide #-}
module SysDep.Posix.Types
    ( CErrno
    , CFd
    , CMemProtFlags
    , CMemMappingFlags
    , CMemAdvice
    , CMemSyncFlags
    , CSysconfName
    , COpenFlags
    , COff(..)
    , CMode(..)
    , CDir
    , CDirent
    ) where

import Basement.Compat.C.Types

type CErrno = CInt
type CFd = CInt
type CMemProtFlags = CInt
type CMemMappingFlags = CInt
type CMemAdvice = CInt
type CMemSyncFlags = CInt
type CSysconfName = CInt
type COpenFlags = CInt

-- | C DIR type
data CDir

-- | C struct dirent type
data CDirent
