module Core.Foreign.MemoryMap.Windows
    ( mapViewOfFile
    ) where

import System.Win32.Types   ( HANDLE, DWORD, BOOL, SIZE_T, LPCTSTR, withTString
                            , failIf, failIfNull, DDWORD, ddwordToDwords
                            , iNVALID_HANDLE_VALUE )
import System.Win32.Mem
import System.Win32.File
import System.Win32.FileMapping
import System.Win32.Info
import Control.Exception

import Core.Internal.Types
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
        fileInfo <- getFileInformationByHandle fh
        fp <- mask_ $ do
            ptr <- mapViewOfFile filemap fILE_MAP_READ 0 0
            toFinalPtr c_UnmapViewOfFileFinaliser ptr
        return (fp, fromIntegral $ bhfiSize fileInfo) -- bhfiSize DDWord=Word64
