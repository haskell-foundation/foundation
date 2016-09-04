module BenchUtil.Common
    ( defaultMain
    , Benchmark
    , bgroup
    , bench
    , fbench
    , whnf
    , nf
    ) where

import Criterion.Main

fbench = bench "foundation"
