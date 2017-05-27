{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Foundation.System.Bindings.Hs
    where

import GHC.IO
import GHC.Prim
import Foreign.C.Types
import Foreign.Ptr

foreign import ccall unsafe "HsBase.h __hscore_get_errno" sysHsCoreGetErrno :: IO CInt

foreign import ccall unsafe "_foundation_memcmp" sysHsMemcmpBaBa ::
    ByteArray# -> CSize -> ByteArray# -> CSize -> CSize -> IO CInt

foreign import ccall unsafe "_foundation_memcmp" sysHsMemcmpBaPtr ::
    ByteArray# -> CSize -> Ptr a -> CSize -> CSize -> IO CInt

foreign import ccall unsafe "_foundation_memcmp" sysHsMemcmpPtrBa ::
    Ptr a -> CSize -> ByteArray# -> CSize -> CSize -> IO CInt

foreign import ccall unsafe "_foundation_memcmp" sysHsMemcmpPtrPtr ::
    Ptr a -> CSize -> Ptr b -> CSize -> CSize -> IO CInt
