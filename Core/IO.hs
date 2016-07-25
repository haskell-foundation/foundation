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
    , foldTextFile

      -- * Directory
    , getDirectory
    ) where

import           System.IO (Handle, IOMode)
import qualified System.IO as S
import           Core
import           Core.Collection
import           Core.VFS
import           Core.Internal.Types
import qualified Core.Array.Unboxed.Mutable as V
import qualified Core.Array.Unboxed as V
import qualified Core.String.UTF8 as S
import           Control.Exception (bracket)
import           Foreign.Ptr (plusPtr)

-- | list the file name in the given FilePath directory
--
-- TODO: error manegement and not implemented yet
getDirectory :: FilePath -> IO [FileName]
getDirectory = undefined

-- | Open a new handle on the file
openFile :: FilePath -> IOMode -> IO Handle
openFile filepath mode = do
    S.openBinaryFile (filePathToLString filepath) mode

-- | Close a handle
closeFile :: Handle -> IO ()
closeFile = S.hClose

-- | Read some data from the handle
hGet :: Handle -> Int -> IO (UArray Word8)
hGet handle n = do
    mv <- V.newPinned (Size n)
    r <- V.withMutablePtr mv $ \ptr -> loop n ptr
    if r < n
        then V.unsafeFreezeShrink mv (Size $ n - r)
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
readFile :: FilePath -> IO (UArray Word8)
readFile fp = withFile fp S.ReadMode $ \h -> do
    -- TODO filesize is an integer (whyyy ?!), and transforming to Int using
    -- fromIntegral is probably the wrong thing to do here..
    sz <- S.hFileSize h
    mv <- V.newPinned (Size $ fromIntegral sz)
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

foldTextFile :: (String -> a -> IO a) -> a -> FilePath -> IO a
foldTextFile chunkf ini fp = do
    buf <- V.newPinned (Size blockSize)
    V.withMutablePtr buf $ \ptr ->
        withFile fp S.ReadMode $ doFold buf ptr
  where
    doFold mv ptr handle = loop 0 ini
      where
        loop absPos acc = do
            r <- S.hGetBuf handle ptr blockSize
            if r > 0 && r <= blockSize
                then do
                    (pos, validateRet) <- S.mutableValidate mv 0 r
                    s <- case validateRet of
                        Nothing -> S.fromBytesUnsafe <$> V.freezeShrink mv r
                        Just S.MissingByte -> do
                            sRet <- S.fromBytesUnsafe <$> V.freezeShrink mv pos
                            V.unsafeSlide mv pos r
                            return sRet
                        Just _ ->
                            error ("foldTextFile: invalid UTF8 sequence: byte position: " <> show (absPos + pos))
                    chunkf s acc >>= loop (absPos + r)
                else error ("foldTextFile: read failed") -- FIXME

blockSize :: Int
blockSize = 4096
