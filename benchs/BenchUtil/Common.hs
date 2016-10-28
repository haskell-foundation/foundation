{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module BenchUtil.Common
    ( defaultMain
    , Benchmark
    , Benchmarkable
    , bgroup
    , bench
    , fbench
    , whnf
    , whnfIO
    , nf
    ) where

import           Criterion.Main hiding (bgroup, bench)
import qualified Criterion.Main as C
import           Foundation

fbench = bench "foundation"

bgroup :: String -> [Benchmark] -> Benchmark
bgroup n f = C.bgroup (toList n) f

bench :: String -> Benchmarkable -> Benchmark
bench n f = C.bench (toList n) f
