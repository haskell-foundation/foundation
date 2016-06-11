module Core.Foreign.MemoryMap.Windows
    ( fileMapRead
    ) where

import System.Win32.Mem
import System.Win32.File
import System.Win32.FileMapping
import Control.Exception hiding (handle)

import Core.Internal.Types
import Core.Internal.Base
import Core.Primitive.FinalPtr
import Core.VFS
import Core.Foreign.MemoryMap.Types

fileMapRead :: FileMapReadF
fileMapRead path = bracket doOpen closeHandle doMapping
  where
    doOpen           = createFile (filePathToLString path) gENERIC_READ fILE_SHARE_READ Nothing oPEN_EXISTING fILE_ATTRIBUTE_NORMAL Nothing
    doMapping handle = bracket (createFileMapping (Just handle) pAGE_READONLY 0 Nothing)
                               closeHandle
                               (getSizeAndMap handle)
    getSizeAndMap handle filemap = do
        fileInfo <- getFileInformationByHandle handle
        fp <- mask_ $ do
            ptr <- mapViewOfFile filemap fILE_MAP_READ 0 0
            toFinalPtr ptr unmapViewOfFile
        return (fp, FileSize $ bhfiSize fileInfo) -- bhfiSize DDWord=Word64
