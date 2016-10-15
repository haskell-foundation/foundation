{-# LANGUAGE NoImplicitPrelude #-}
module Sys ( benchSys ) where

import Foundation
import Foundation.Collection
import BenchUtil.Common
import BenchUtil.RefData

import Foundation.System.Entropy
import Foundation.Random

import qualified Prelude

data NullRandom = NullRandom

instance RandomGen NullRandom where
    randomNew        = return NullRandom
    randomGenerate n r = (fromList (Prelude.replicate n 0), r)

benchSys =
    [ bgroup "Random"
        [ bench "Entropy-1"    $ whnfIO $ getEntropy 1
        , bench "Entropy-16"   $ whnfIO $ getEntropy 16
        , bench "Entropy-1024" $ whnfIO $ getEntropy 1024
        ]
    , bgroup "RNGv1"
        [ bench "Entropy-1"    $ benchRandom 1 randomNew (Proxy :: Proxy RNGv1)
        , bench "Entropy-1024"    $ benchRandom 1024 randomNew (Proxy :: Proxy RNGv1)
        , bench "Entropy-1M"    $ benchRandom (1024 * 1024) randomNew (Proxy :: Proxy RNGv1)
        ]
    ]

benchRandom :: RandomGen rng => Int -> MonadRandomState NullRandom rng -> Proxy rng -> Benchmarkable
benchRandom n rNew _ = whnf (fst . randomGenerate n) (fst $ withRandomGenerator NullRandom rNew)
