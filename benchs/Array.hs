module Main (main) where

import Foundation
import Foundation.Collection as F
import Criterion.Main

main = defaultMain
    [ bgroup "Uarray"
        [ bench "fromList [Word8]" $ whnf (fromList :: [Word8] -> UArray Word8) [1..255]
        , bench "fromList [Word16]" $ whnf (fromList :: [Word16] -> UArray Word16) [1..1024]
        , bench "break" $ whnf (F.break (== 255)) input
        ]
    ]
  where
    input :: UArray Word8
    input = fromList ([1..255] <> [1..255])
