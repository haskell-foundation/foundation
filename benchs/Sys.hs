{-# LANGUAGE NoImplicitPrelude #-}
module Sys ( benchSys ) where

import Foundation
import Foundation.Collection
import BenchUtil.Common
import BenchUtil.RefData

import Foundation.System.Entropy

benchSys =
    [ bgroup "Random"
        [ bench "Entropy-1"    $ whnfIO $ getEntropy 1
        , bench "Entropy-16"   $ whnfIO $ getEntropy 16
        , bench "Entropy-1024" $ whnfIO $ getEntropy 1024
        ]
    ]
