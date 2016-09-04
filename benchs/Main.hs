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
textTake    = Text.take
#else
type BOGUS = ()

textLength = ()
textSplitAt = ((), ())
textTake    = ()
#endif

--------------------------------------------------------------------------

benchsString = bgroup "String"
    [ benchLength
    , benchTake
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
    benchLength = bgroup "Length" $
        fmap (\(n, dat) -> bgroup n $ diffTextString length textLength dat)
            [ ("ascii", rdFoundationEn)
            , ("mascii", rdFoundationHun)
            , ("uni1" ,rdFoundationJap)
            , ("uni2" ,rdFoundationZh)
            ]
    benchTake = bgroup "Take" $
        mconcat $ fmap (\p ->
        fmap (\(n, dat) -> bgroup n $ diffTextString (take p) (textTake p) dat)
            [ ("ascii-" <> show p, rdFoundationEn)
            , ("mascii-" <> show p, rdFoundationHun)
            , ("uni1-" <> show p,rdFoundationJap)
            , ("uni2-" <> show p,rdFoundationZh)
            ]) [ 10, 100, 800 ]
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
