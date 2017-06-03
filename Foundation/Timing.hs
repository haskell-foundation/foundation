-- |
-- Module      : Foundation.Timing
-- License     : BSD-style
-- Maintainer  : Foundation maintainers
--
-- An implementation of a timing framework
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Foundation.Timing
    ( Timing(..)
    , Measure(..)
    , stopWatch
    , measure
    ) where

import           Foundation.Primitive.Imports
import           Foundation.Primitive.IntegralConv
import           Foundation.Primitive.Monad
-- import           Foundation.Array.Unboxed hiding (unsafeFreeze)
import           Foundation.Array.Unboxed.Mutable (MUArray)
import           Foundation.Collection
import           Foundation.Time.Types
import           Foundation.Numerical
import           Foundation.Time.Bindings
import           Control.Exception (evaluate)
import           System.Mem (performGC)
import           Data.Function (on)
import qualified GHC.Stats as GHC


data Timing = Timing
    { timeDiff           :: !NanoSeconds
    , timeBytesAllocated :: !(Maybe Int64)
    }

data Measure = Measure
    { measurements :: UArray NanoSeconds
    , iters        :: Word
    }

getGCStats :: IO (Maybe GHC.GCStats)
getGCStats = do
    r <- GHC.getGCStatsEnabled
    if r then pure Nothing else Just <$> GHC.getGCStats

-- | Simple one-time measurement of time & other metrics spent in a function
stopWatch :: (a -> b) -> a -> IO Timing
stopWatch f !a = do
    performGC
    gc1 <- getGCStats
    (_, ns) <- measuringNanoSeconds (evaluate $ f a)
    gc2 <- getGCStats
    return $ Timing ns (((-) `on` GHC.bytesAllocated) <$> gc2 <*> gc1)

-- | In depth timing & other metrics analysis of a function
measure :: Word -> (a -> b) -> a -> IO Measure
measure nbIters f a = do
    d <- mutNew (integralCast nbIters) :: IO (MUArray NanoSeconds (PrimState IO))
    loop d 0
    Measure <$> unsafeFreeze d
            <*> pure nbIters
  where
    loop d !i
        | i == nbIters = return ()
        | otherwise    = do
            (_, r) <- measuringNanoSeconds (evaluate $ f a)
            mutUnsafeWrite d (integralCast i) r
            loop d (i+1)
