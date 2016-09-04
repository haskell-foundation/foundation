{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Foundation
import Foundation.Collection
import BenchUtil.Common
import BenchUtil.RefData

#ifdef BENCH_ALL
import qualified Data.Text as Text

textLength = Text.length
textSplitAt = Text.splitAt
#else
type BOGUS = ()

textLength = ()
textSplitAt = ((), ())
#endif

--------------------------------------------------------------------------

benchsString = bgroup "String"
    [ benchLength
    , benchSplitAt
    -- , bgroup "SplitAt"
    ]
  where
    diffTextString :: (String -> a)
#ifdef BENCH_ALL
                   -> (Text.Text   -> b)
#else 
                   -> BOGUS
#endif
                   -> [Char]
                   -> [Benchmark]
    diffTextString foundationBench textBench dat =
        [ bench "String" $ whnf foundationBench s
#ifdef BENCH_ALL
        , bench "Text"   $ whnf textBench t
#endif
        ]
      where
        s = fromList dat
#ifdef BENCH_ALL
        t = Text.pack dat
#endif

{-
    diffTextStringParam1 foundationBench textBench p1 dat =
        [ bgroup ("param1=" <> show p1) $
            [ bench "String" $ whnf (foundationBench p1) dat
    #ifdef BENCH_ALL
            , bench "Text"   $ whnf (textBench p1) t
    #endif
            ]
        ]
-}

{-
    benchSplitAt =
    diffTextStringParam1 (\p1 -> fst . F.splitAt p1)
                         (\p1 -> fst . textSplitAt p1)
            [ bench "splitAt 10" $ whnf (fst . F.splitAt 10) s
            , bench "splitAt 100" $ whnf (fst . F.splitAt 100) s
            , bench ("splitAt " ++ show l) $ whnf (fst . F.splitAt l) s
            ]
-}

    benchLength = bgroup "Length" $
        fmap (\(n, dat) -> bgroup n $ diffTextString length textLength dat)
            [ ("ascii", rdFoundationEn)
            , ("mascii", rdFoundationHun)
            , ("uni1" ,rdFoundationJap)
            , ("uni2" ,rdFoundationZh)
            ]
    benchSplitAt = bgroup "SplitAt" $
        mconcat $ fmap (\p ->
        fmap (\(n, dat) -> bgroup n $ diffTextString (fst . splitAt p) (fst . textSplitAt p) dat)
            [ ("ascii-" <> show p, rdFoundationEn)
            , ("mascii-" <> show p, rdFoundationHun)
            , ("uni1-" <> show p,rdFoundationJap)
            , ("uni2-" <> show p,rdFoundationZh)
            ]) [ 10, 100, 800 ]

--------------------------------------------------------------------------

benchsTypes = bgroup "types"
    [ benchsString
    ]

main = defaultMain
    [ benchsTypes ]
