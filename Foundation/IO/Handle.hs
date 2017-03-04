module Foundation.IO.Handle
    ( Handle
    , openFile
    , close
    , hGetBuf
    , hGet
    , hPut
    , hSeek
    , SeekParam(..)
    ) where

import           GHC.MVar
import           Control.Monad (when)
import           Control.Concurrent.MVar (withMVar, modifyMVar, readMVar)
import           Foreign.C.Types
import           Foreign.C.Error hiding (throwErrno)
import           System.Posix.Internals hiding (FD)
import           System.Posix.Types (CSsize(..), CMode(..))
import           Foundation.Internal.Base
import           Foundation.Internal.Types
import           Foundation.Primitive.Monad
import           Foundation.Numerical
import qualified Foundation.Array.Unboxed as UA
import           Foundation.Array.Unboxed
import           Foundation.Array.Unboxed.Mutable
import           Foundation.String.UTF8
import           Foundation.VFS.FilePath

type FD = CInt

invalidFD :: FD
invalidFD = -1

-- we don't want CSsize to be HasNegative because technically this is just 1 value.
minus1SSize :: CSsize
minus1SSize = CSsize (-1)

newtype Handle = Handle (MVar FD)

data HandleClosed = HandleClosed
    deriving (Show,Eq,Typeable)

instance Exception HandleClosed

data Permission = Read | Write | ReadWrite

-- FIXME
throwErrno err = error "errno error"

openFile :: FilePath
         -> Permission
         -> IO Handle
openFile fp iom = do
    cfd <- withCFilePath fp $ \cfp -> c_open cfp oflag omode
    mvar <- newMVar cfd
    addMVarFinalizer mvar $ do
        x <- readMVar mvar
        _ <- c_close x
        return ()
    pure $ Handle mvar
  where
    omode = CMode 0
    oflag = 0

close :: Handle -> IO ()
close (Handle mv) =
    modifyMVar mv (\cfd -> c_close cfd >> return (invalidFD, ()))

withHandle :: Handle -> (FD -> IO a) -> IO a
withHandle (Handle handle) f = withMVar handle $ \cfd -> do
    when (cfd == invalidFD) $ throwIO HandleClosed
    f cfd

ioPtrRetryLoop :: (Ptr Word8 -> CSize -> IO CSsize)
               -> Ptr Word8
               -> Size Word8
               -> IO (Size Word8)
ioPtrRetryLoop ioFct ptr sz = loop ptr sz
  where
    loop !p !remaining
        | remaining == 0   = return sz
        | otherwise        = do
            ssz <- ioFct p (csizeOfSize remaining)
            if ssz == minus1SSize
                then do
                    err <- getErrno
                    case err of
                        _ | err == eAGAIN      -> loop p remaining
                          | err == eINTR       -> loop p remaining
                          | err == eWOULDBLOCK -> loop p remaining
                          | otherwise          -> throwErrno err
                else if ssz == CSsize 0
                        then return (sz - remaining)
                        else
                            let got = sizeOfCSSize ssz
                             in loop (p `plusPtrSize` got) (remaining - got)

hGetBuf :: Handle -> MUArray Word8 (PrimState IO) -> Size Word8 -> IO (Size Word8)
hGetBuf handle buf sz =
    -- check out of bounds
    withHandle handle $ \cfd ->
    withMutablePtr buf $ \ptr ->
        ioPtrRetryLoop (c_read cfd) ptr sz

hGet :: Handle -> Size Word8 -> IO (UArray Word8)
hGet handle sz =
    withHandle handle $ \cfd ->
    createFromIO sz $ \ptr -> do
        ioPtrRetryLoop (c_read cfd) ptr sz

hPut :: Handle -> UArray Word8 -> IO ()
hPut handle ba =
    withHandle handle $ \cfd ->
    withPtr ba        $ \ptr -> do
        r <- ioPtrRetryLoop (c_write cfd) ptr totalSize
        -- TODO check returned sized
        return ()
  where
    totalSize = Size $ UA.length ba

data SeekParam = SeekFromBeginning
               | SeekFromEnd
               | SeekFromCurrent

hSeek :: Handle -> SeekParam -> Offset Word8 -> IO ()
hSeek _ _ _ = return ()
