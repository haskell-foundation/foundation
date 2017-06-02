{-# LANGUAGE CPP #-}
module Foundation.Time.Bindings
    ( measuringNanoSeconds
    , getMonotonicTime
    ) where

import Foundation.Primitive.Imports
import Foundation.Primitive.Types.OffsetSize
import Foundation.System.Bindings.Time
import Foundation.Time.Types
import qualified Foreign.Marshal.Alloc as A (allocaBytes)
import Foreign.Storable
import Foreign.Ptr

allocaBytes :: Size Word8 -> (Ptr a -> IO b) -> IO b
allocaBytes (Size i) f = A.allocaBytes i f

measuringNanoSeconds :: IO a -> IO (a, NanoSeconds)
measuringNanoSeconds f =
    allocaBytes (sizeOfCSize size_CTimeSpec) $ \t1 ->
    allocaBytes (sizeOfCSize size_CTimeSpec) $ \t2 -> do
        _err1 <- sysTimeClockGetTime sysTime_CLOCK_MONOTONIC t1
        r <- f
        _err2 <- sysTimeClockGetTime sysTime_CLOCK_MONOTONIC t2
        return (r, NanoSeconds 0)

getMonotonicTime :: IO (Seconds, NanoSeconds)
getMonotonicTime =
    allocaBytes (sizeOfCSize size_CTimeSpec) $ \tspec -> do
        _err1 <- sysTimeClockGetTime sysTime_CLOCK_MONOTONIC tspec
        s  <- Seconds     <$> peek (castPtr (tspec `plusPtr` ofs_CTimeSpec_Seconds))
        ns <- NanoSeconds <$> peek (castPtr (tspec `plusPtr` ofs_CTimeSpec_NanoSeconds))
        return (s,ns)
