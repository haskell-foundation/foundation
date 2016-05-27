{-# LANGUAGE CPP #-}
module Core.Foreign.MemoryMap
    (
    ) where

import Core.Internal.Base
import Core.Collection.Indexed
import Foreign.ForeignPtr

#ifndef __WIN32__
import Core.Foreign.MemoryMap.Posix
#else
import Core.Foreign.MemoryMap.Windows
#endif

data FileMap = FileMap
    { fptr :: ForeignPtr Word8
    }

{-
fileMap :: Fd -> Int -> IO FileMap
fileMap = undefined
-}
