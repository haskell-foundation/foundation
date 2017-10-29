module Main (main) where

import Foundation
import Foundation.Random
import Foundation.Collection as F
import Gauge.Main
import qualified Prelude as P

main = do
    rndInput <- getRandomBytes (CountOf n)
    defaultMain [ bgroup "Uarray"
        [ bench "fromList [Word8]" $ whnf (fromList :: [Word8] -> UArray Word8) [1..255]
        , bench "fromList [Word16]" $ whnf (fromList :: [Word16] -> UArray Word16) [1..1024]
        , bench "break" $ whnf (F.break (== 255)) input
        , bench "sort random"  $ whnf sort rndInput
        , bench "sort sorted"  $ whnf sort (sort rndInput)
        , bench "sort reverse" $ whnf sort (reverse.sort $ rndInput)
        , bench "sort cyclic"  $ whnf sort inputLong
        ]
      ]
  where
    n = 100000
    input, inputLong :: UArray Word8
    input = fromList ([1..255] <> [1..255])
    inputLong = fromList . P.take n . P.cycle $ [1..255]

    sort = F.sortBy compare
