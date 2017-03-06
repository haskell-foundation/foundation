{-# LANGUAGE OverloadedStrings #-}
module Foundation.IO.Handle.Posix
    ( ioPtrRetryLoop
    , ioRead
    , ioWrite
    ) where

import           Foundation.Internal.Base
import           Foundation.Internal.Types
import           Foundation.Numerical
import           Foreign.C.Types
import           Foreign.C.Error
import           Foundation.String
import           System.Posix.Internals hiding (FD)
import           System.Posix.Types (CSsize(..), CMode(..))
import           Foundation.System.Bindings.Hs (sysHsCoreGetErrno)
import           Foundation.System.Bindings.Posix

data HandleError = HandleError String CInt
    deriving (Show,Eq,Typeable)
instance Exception HandleError

-- we don't want CSsize to be HasNegative because technically this is just 1 value.
minus1SSize :: CSsize
minus1SSize = CSsize (-1)

ioRead :: CInt -> Ptr Word8 -> CSize -> IO CSsize
ioRead = c_read

ioWrite :: CInt -> Ptr Word8 -> CSize -> IO CSsize
ioWrite = c_write

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
                    err <- sysHsCoreGetErrno
                    case err of
                        _ | err == sysPosix_EAGAIN      -> loop p remaining
                          | err == sysPosix_EINTR       -> loop p remaining
                          | err == sysPosix_EWOULDBLOCK -> loop p remaining
                          | otherwise                   -> throwIO (HandleError "io" err)
                else if ssz == CSsize 0
                        then return (sz - remaining)
                        else
                            let got = sizeOfCSSize ssz
                             in loop (p `plusPtrSize` got) (remaining - got)
