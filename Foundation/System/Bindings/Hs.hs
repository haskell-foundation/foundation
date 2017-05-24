{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Foundation.System.Bindings.Hs
    where

import GHC.IO
import GHC.Types
import GHC.Prim
import Foreign.C.Types

foreign import ccall unsafe "HsBase.h __hscore_get_errno" sysHsCoreGetErrno :: IO CInt

foreign import ccall unsafe "_foundation_memcmp" sysHsMemcmp ::
    ByteArray# -> CSize -> ByteArray# -> CSize -> CSize -> IO CInt
