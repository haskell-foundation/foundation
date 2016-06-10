-- |
-- Module      : Core.IO
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- IO Routine
module Core.IO
    ( Handle
    , IOMode(..)
    , openFile
    , closeFile
    , withFile
    , hGet
    , readFile
    ) where

import           System.IO (Handle, IOMode)
import qualified System.IO as S
import           Core
import           Core.Collection
import           Core.VFS
import qualified Core.Vector.Unboxed as V
import           Control.Exception (bracket)
import           Foreign.Ptr (plusPtr)

-- | Open a new handle on the file
openFile :: FilePath -> IOMode -> IO Handle
openFile filepath mode = do
    S.openBinaryFile (filePathToLString filepath) mode

-- | Close a handle
closeFile :: Handle -> IO ()
closeFile = S.hClose

-- | Read some data from the handle
hGet :: Handle -> Int -> IO ByteArray
hGet handle n = do
    mv <- V.newPinned n
    r <- V.withMutablePtr mv $ \ptr -> loop n ptr
    if r < n
        then V.unsafeFreezeShrink mv r
        else unsafeFreeze mv
  where
    loop left dst
        | left == 0 = return 0
        | otherwise = do
            let toRead = min blockSize left
            r <- S.hGetBuf handle dst toRead
            if r > 0 && r <= toRead
                then loop (left - r) (dst `plusPtr` r)
                else
                    if r == 0
                        then return left
                        else error "readFile: " -- turn into proper error

-- | @'withFile' filepath mode act@ opens a file using the mode@
-- and run act@. the by-product handle will be closed when act finish,
-- either normally or through an exception.
--
-- The value returned is the result of act@
withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile fp mode act = bracket (openFile fp mode) closeFile act

-- | Read a binary file and return the whole content in one contiguous buffer.
readFile :: FilePath -> IO ByteArray
readFile fp = withFile fp S.ReadMode $ \h -> do
    -- TODO filesize is an integer (whyyy ?!), and transforming to Int is probably the wrong thing to do here..
    sz <- S.hFileSize h
    mv <- V.newPinned (fromIntegral sz)
    V.withMutablePtr mv $ loop h (fromIntegral sz)
    unsafeFreeze mv
  where
    loop h left dst
        | left == 0 = return ()
        | otherwise = do
            let toRead = min blockSize left
            r <- S.hGetBuf h dst toRead
            if r > 0 && r <= toRead
                then loop h (left - r) (dst `plusPtr` r)
                else error "readFile: " -- turn into proper error

blockSize :: Int
blockSize = 4096
