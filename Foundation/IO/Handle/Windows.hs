module Foundation.IO.Handle.Windows
    ( ioPtrRetryLoop
    , ioRead
    , ioWrite
    ) where

import           Foundation.Internal.Base
import           Foreign.C.Types
import           System.Posix.Internals hiding (FD)
import           System.Posix.Types (CSsize(..), CMode(..))
import qualified Prelude (fromIntegral)

type BytesToRead = CUInt
type BytesRead = CInt

ioRead :: CInt -> Ptr Word8 -> BytesToRead -> IO BytesRead
ioRead = c_read

ioWrite :: CInt -> Ptr Word8 -> BytesToRead -> IO BytesRead
ioWrite = c_write

ioPtrRetryLoop :: (Ptr Word8 -> BytesToRead -> IO BytesRead)
               -> Ptr Word8
               -> Size Word8
               -> IO (Size Word8)
ioPtrRetryLoop ioFct ptr sz = loop ptr sz
  where
    csizeOfSize (Size r) = r

    loop !p !remaining
        | remaining == 0   = return sz
        | otherwise        = do
            ssz <- ioFct p (csizeOfSize remaining)
            if ssz == -1
                then do
                    err <- getErrno
                    case err of
                        _ | err == eAGAIN      -> loop p remaining
                          | err == eINTR       -> loop p remaining
                          | err == eWOULDBLOCK -> loop p remaining
                          | otherwise          -> throwErrno err
                else if ssz == 0
                        then return (sz - remaining)
                        else
                            let got = Size ssz
                             in loop (p `plusPtrSize` got) (remaining - got)
