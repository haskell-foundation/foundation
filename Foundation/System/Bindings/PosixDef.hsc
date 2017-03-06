
module Foundation.System.Bindings.PosixDef
    (
      CFd
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

type CFd = CInt
type CMemProtFlags = CInt
type CMemMappingFlags = CInt
type CMemAdvice = CInt
type CMemSyncFlags = CInt
type CSysconfName = CInt
type COpenFlags = CInt
