{-# LANGUAGE CPP #-}
module Core.Foreign.MemoryMap
    ( fileMapRead
    ) where

#ifdef __WIN32__
import Core.Foreign.MemoryMap.Windows
#else
import Core.Foreign.MemoryMap.Posix
#endif

{-
fileMap :: Fd -> Int -> IO FileMap
fileMap = undefined
-}
