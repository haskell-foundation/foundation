{-# OPTIONS_HADDOCK hide #-}
module Foundation.System.Bindings.Hs
    where

import GHC.IO
import Foreign.C.Types

foreign import ccall unsafe "HsBase.h __hscore_get_errno" sysHsCoreGetErrno :: IO CInt
