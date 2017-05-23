{-# OPTIONS_HADDOCK hide #-}
module Foundation.System.Bindings.Macos
    where

import Foundation.Internal.Base
import Foundation.System.Bindings.PosixDef

#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>

sysMacos_O_SHLOCK
    , sysMacos_O_EXLOCK
    , sysMacos_O_SYMLINK
    , sysMacos_O_EVTONLY :: COpenFlags
sysMacos_O_SHLOCK   = (#const O_SHLOCK)
sysMacos_O_EXLOCK   = (#const O_EXLOCK)
sysMacos_O_SYMLINK  = (#const O_SYMLINK)
sysMacos_O_EVTONLY  = (#const O_EVTONLY)
