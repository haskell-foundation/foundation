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

import Criterion.Main

fbench = bench "foundation"
