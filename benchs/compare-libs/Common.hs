module Common
    ( defaultMain
    , bgroup
    , bench
    , fbench
    , whnf
    , nf
    ) where

import Criterion.Main

fbench = bench "foundation"
