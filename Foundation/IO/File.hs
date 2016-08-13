-- |
-- Module      : Foundation.IO.File
-- License     : BSD-style
-- Maintainer  : Foundation
-- Stability   : experimental
-- Portability : portable
--
module Foundation.IO.File
    ( openFile
    , closeFile
    , IOMode(..)
    , withFile
    , hGet
    , hGetNonBlocking
    , hGetSome
    , readFile
    , foldTextFile
    ) where

import           System.IO (Handle, IOMode)
import           System.IO.Error
import qualified System.IO as S
import           Foundation.Collection
import           Foundation.VFS
import           Foundation.Internal.Types
import           Foundation.Internal.Base
import           Foundation.String
import           Foundation.Array
import           Foundation.Number
import qualified Foundation.Array.Unboxed.Mutable as V
import qualified Foundation.Array.Unboxed as V
import qualified Foundation.String.UTF8 as S
import           Control.Exception (bracket)
import           Foreign.Ptr (plusPtr)

-- | list the file name in the given FilePath directory
--
-- TODO: error management and not implemented yet
--getDirectory :: FilePath -> IO [FileName]
--getDirectory = undefined

-- | Open a new handle on the file
openFile :: FilePath -> IOMode -> IO Handle
openFile filepath mode = do
    S.openBinaryFile (filePathToLString filepath) mode

-- | Close a handle
closeFile :: Handle -> IO ()
closeFile = S.hClose

-- | Read binary data directly from the specified 'Handle'.
--
-- First argument is the Handle to read from, and the second is the number of bytes to read.
-- It returns the bytes read, up to the specified size, or an empty array if EOF has been reached.
--
-- 'hGet' is implemented in terms of 'hGetBuf'.
hGet :: Handle -> Int -> IO (UArray Word8)
hGet h size
    | size < 0   = invalidBufferSize "hGet" h size
    | otherwise  = V.createFromIO size $ \p -> S.hGetBuf h p size

-- | hGetNonBlocking is similar to 'hGet', except that it will never block
-- waiting for data to become available, instead it returns only whatever data
-- is available.  If there is no data available to be read, 'hGetNonBlocking'
-- returns an empty array.
--
-- Note: on Windows, this function behaves identically to 'hGet'.
hGetNonBlocking :: Handle -> Int -> IO (UArray Word8)
hGetNonBlocking h size
    | size < 0  = invalidBufferSize "hGetNonBlocking" h size
    | otherwise = V.createFromIO size $ \p -> S.hGetBufNonBlocking h p size

-- | Like 'hGet', except that a shorter array may be returned
-- if there are not enough bytes immediately available to satisfy the
-- whole request.  'hGetSome' only blocks if there is no data
-- available, and EOF has not yet been reached.
--
hGetSome :: Handle -> Int -> IO (UArray Word8)
hGetSome h size
    | size < 0  = invalidBufferSize "hGetSome" h size
    | otherwise = V.createFromIO size $ \p -> S.hGetBufSome h p size

invalidBufferSize :: [Char] -> Handle -> Int -> IO a
invalidBufferSize functionName handle size =
    ioError $ mkIOError illegalOperationErrorType
                        (functionName <> " invalid array size: " <> show size)
                        (Just handle)
                        Nothing

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
    mv <- V.newPinned (Size $ fromInteger sz)
    V.withMutablePtr mv $ loop h (fromInteger sz)
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

-- | Fold over chunks file calling the callback function for each chunks
-- read from the file, until the end of file.
foldTextFile :: (String -> a -> IO a) -- ^ Fold callback function
             -> a                     -- ^ initial accumulator
             -> FilePath              -- ^ File to read
             -> IO a
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
                        Nothing -> S.fromBytesUnsafe `fmap` V.freezeShrink mv r
                        Just S.MissingByte -> do
                            sRet <- S.fromBytesUnsafe `fmap` V.freezeShrink mv pos
                            V.unsafeSlide mv pos r
                            return sRet
                        Just _ ->
                            error ("foldTextFile: invalid UTF8 sequence: byte position: " <> show (absPos + pos))
                    chunkf s acc >>= loop (absPos + r)
                else error ("foldTextFile: read failed") -- FIXME

blockSize :: Int
blockSize = 4096
