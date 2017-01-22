module Foundation.IO.Handle
    ( Handle
    , openFile
    , close
    , hGet
    , hPut
    , hSeek
    , SeekParam(..)
    ) where

import           GHC.MVar
import           Control.Monad (when)
import           Control.Concurrent.MVar (withMVar, modifyMVar)
import           Foreign.C.Types
import           Foreign.C.Error hiding (throwErrno)
import           System.Posix.Internals hiding (FD)
import           System.Posix.Types (CSsize(..), CMode(..))
import           Foundation.Internal.Base
import           Foundation.Internal.Types
import           Foundation.Numerical
import qualified Foundation.Array.Unboxed as UA
import           Foundation.Array.Unboxed
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

hGet :: Handle -> Size Word8 -> IO (UArray Word8)
hGet handle sz =
    withHandle handle $ \cfd ->
    createFromIO sz $ \ptr -> do
        loop cfd c_read ptr sz
  where
    loop cfd !readFct !p !remaining
        | remaining == 0   = return sz
        | otherwise        = do
            ssz <- readFct cfd p (csizeOfSize remaining)
            if ssz == minus1SSize
                then do
                    err <- getErrno
                    case err of
                        _ | err == eAGAIN      -> loop cfd readFct p remaining
                          | err == eINTR       -> loop cfd readFct p remaining
                          | err == eWOULDBLOCK -> loop cfd readFct p remaining
                          | otherwise          -> throwErrno err
                else if ssz == CSsize 0
                        then return (sz - remaining)
                        else
                            let read = sizeOfCSSize ssz
                             in loop cfd readFct (p `plusPtrSize` read) (remaining - read)

hPut :: Handle -> UArray Word8 -> IO ()
hPut handle ba =
    withHandle handle $ \cfd ->
    withPtr ba        $ \ptr ->
        loop cfd ptr (Size $ UA.length ba)
  where
    loop !cfd !p !remaining
        | remaining == 0 = return ()
        | otherwise      = do
            ssz <- c_write cfd p (csizeOfSize remaining)
            if ssz == minus1SSize
                then do
                    err <- getErrno
                    case err of
                        _ | err == eAGAIN      -> loop cfd p remaining
                          | err == eINTR       -> loop cfd p remaining
                          | err == eWOULDBLOCK -> loop cfd p remaining
                          | otherwise          -> throwErrno err
                else
                    let written = sizeOfCSSize ssz
                     in loop cfd (p `plusPtrSize` written) (remaining - written)

data SeekParam = SeekFromBeginning
               | SeekFromEnd
               | SeekFromCurrent

hSeek :: Handle -> SeekParam -> Offset Word8 -> IO ()
hSeek _ _ _ = return ()
