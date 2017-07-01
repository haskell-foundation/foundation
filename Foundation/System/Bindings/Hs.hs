{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Foundation.System.Bindings.Hs
    where

import GHC.IO
import GHC.Prim
import GHC.Word
import Prelude (Char)
import Foreign.C.Types
import Foreign.Ptr
import Foundation.Primitive.Types.OffsetSize

foreign import ccall unsafe "HsBase.h __hscore_get_errno" sysHsCoreGetErrno :: IO CInt

foreign import ccall unsafe "_foundation_memcmp" sysHsMemcmpBaBa ::
    ByteArray# -> Offset Word8 -> ByteArray# -> Offset Word8 -> CountOf Word8 -> IO CInt

foreign import ccall unsafe "_foundation_memcmp" sysHsMemcmpBaPtr ::
    ByteArray# -> Offset Word8 -> Ptr a -> Offset Word8 -> CountOf Word8 -> IO CInt

foreign import ccall unsafe "_foundation_memcmp" sysHsMemcmpPtrBa ::
    Ptr a -> Offset Word8 -> ByteArray# -> Offset Word8 -> CountOf Word8 -> IO CInt

foreign import ccall unsafe "_foundation_memcmp" sysHsMemcmpPtrPtr ::
    Ptr a -> Offset Word8 -> Ptr b -> Offset Word8 -> CountOf Word8 -> IO CInt

foreign import ccall unsafe "_foundation_mem_findbyte" sysHsMemFindByteBa ::
