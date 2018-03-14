{-# OPTIONS_HADDOCK hide #-}
module SysDep.MacOS.Constants
    where

import Basement.Compat.Base
import Basement.Compat.C.Types
import Foundation.System.Bindings.PosixDef
import Basement.Types.OffsetSize

#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>

openflag_SHLOCK
    , openflag_EXLOCK
    , openflag_SYMLINK
    , openflag_EVTONLY :: COpenFlags
openflag_SHLOCK   = (#const O_SHLOCK)
openflag_EXLOCK   = (#const O_EXLOCK)
openflag_SYMLINK  = (#const O_SYMLINK)
openflag_EVTONLY  = (#const O_EVTONLY)
