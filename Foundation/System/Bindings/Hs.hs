{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Foundation.System.Bindings.Hs
    where

import GHC.IO
import GHC.Prim
import GHC.Word
import Foreign.C.Types
import Foreign.Ptr
import Foundation.Primitive.Types.OffsetSize

foreign import ccall unsafe "HsBase.h __hscore_get_errno" sysHsCoreGetErrno :: IO CInt
