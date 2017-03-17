
module Foundation.System.Bindings.PosixDef
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
    ) where

import Foreign.C.Types
import System.Posix.Types (COff(..), CMode(..))

type CErrno = CInt
type CFd = CInt
type CMemProtFlags = CInt
type CMemMappingFlags = CInt
type CMemAdvice = CInt
type CMemSyncFlags = CInt
type CSysconfName = CInt
type COpenFlags = CInt
