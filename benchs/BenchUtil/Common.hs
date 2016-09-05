module BenchUtil.Common
    ( defaultMain
    , Benchmark
    , Benchmarkable
    , bgroup
    , bench
    , fbench
    , whnf
    , nf
    ) where

import Criterion.Main

fbench = bench "foundation"
