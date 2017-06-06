{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
module Main where

import Control.Monad
import Foundation
import Foundation.Monad
import Foundation.Numerical
import Foundation.Time.Bindings
import Foundation.Time.Types
import Foundation.Time.StopWatch
import Foundation.IO.Terminal
import Foundation.Bits
import Data.Monoid
import Data.Semigroup (Max(..), Min(..))
import Control.Concurrent
import Control.Exception

#if USE_CRITERION
import qualified Criterion.Measurement as C
#endif

data Monotonic = Monotonic
    { maxDiff      :: Max NanoSeconds
    , minDiff      :: Min NanoSeconds
    , notMonotonic :: Sum Word
    } deriving (Show,Eq)

instance Monoid Monotonic where
    mempty = Monotonic mempty mempty mempty
    mappend m1 m2 = Monotonic { maxDiff      = maxDiff m1 `mappend` maxDiff m2
                              , minDiff      = minDiff m1 `mappend` minDiff m2
                              , notMonotonic = notMonotonic m1 `mappend` notMonotonic m2
                              }

monotonicTest :: Int -> IO ()
monotonicTest nbVals
    | nbVals < 2 = error "monotonic time test: invalid number"
    | otherwise  = do
        r <- replicateM nbVals getMonotonicTime
        let diffs = case r of
                    []     -> error "monotonic time: cannot create differential"
                    (x:xs) -> createDifferential x xs
        putStrLn $ show $ mconcat $ fmap toMonotonicStat diffs
        --mapM_ (putStrLn . show) diffs
  where
    toMonotonicStat s@(NanoSeconds ns)
        | isNegative ns =
            Monotonic { maxDiff = Max minBound -- "neutral element"
                      , minDiff = Min maxBound -- "neutral element"
                      , notMonotonic = Sum 1
                      }
        | otherwise =
            Monotonic { maxDiff = Max s
                      , minDiff = Min s
                      , notMonotonic = Sum 0
                      }

    isNegative ns = testBit ns 63
    createDifferential x []        = error "monotonic time: cannot create differential"
    createDifferential x [lastVal] =
        [timeDiff x lastVal]
    createDifferential x (next:xs) =
        timeDiff x next : createDifferential next xs

    timeDiff ((Seconds oldS), (NanoSeconds oldNs)) ((Seconds nextS), nextNs) = do
        let s  = Seconds (nextS - oldS)
            (NanoSeconds nsNextTotal) = secondsToNanoSeconds s + nextNs
            diffNs = NanoSeconds (nsNextTotal - oldNs)
         in diffNs

    secondsToNanoSeconds (Seconds s) = NanoSeconds (s * 1000000000)

stopWatchTest :: IO ()
stopWatchTest = do
    criterionInit

    let msDelay = 100000 -- in microseconds
    putStrLn "waiting 100 ms"
    ns <- do
            stopWatch <- startPrecise
            threadDelay msDelay
            stopPrecise stopWatch
    putStrLn $ "expected: " <> show (msDelay * 1000)
    putStrLn $ show ns
    putStrLn . criterionSec =<< criterionDelay msDelay

    putStrLn "======================================="

    putStrLn "waiting 0 us"

    ns2 <- do
            stopWatch <- startPrecise
            stopPrecise stopWatch
    putStrLn $ show ns2
    putStrLn . criterionSec =<< criterionNothing

    putStrLn "======================================="

    putStrLn "waiting summing number from 0 to 1000"
    ns3 <- do
            stopWatch <- startPrecise
            !_ <- evaluate $ foldl' (\x y -> x + y) 0 [0 :: Int ..1000]
            stopPrecise stopWatch
    putStrLn $ show ns3
    putStrLn . criterionSec =<< criterionDiff (foldl' (\x y -> x + y) 0) [0 :: Int ..1000]
  where
#if USE_CRITERION
    criterionInit = C.initializeTime
    criterionNothing = do
        t1 <- C.getTime
        t2 <- C.getTime
        pure (t2 - t1)
    criterionDelay ms = do
        t1 <- C.getTime
        threadDelay ms
        t2 <- C.getTime
        pure (t2 - t1)
    criterionDiff f a = do
        t1 <- C.getTime
        !_ <- evaluate (f a)
        t2 <- C.getTime
        pure (t2 - t1)
    criterionSec = fromList . C.secs
#else
    criterionInit = pure ()
    criterionNothing = pure (1/0)
    criterionDelay _ = pure (1/0)
    criterionDiff _ _ = pure (1/0)
    criterionSec = show
#endif

main :: IO ()
main = do
    monotonicTest 10000
    putStrLn "============ stop watch test ==========="
    stopWatchTest
