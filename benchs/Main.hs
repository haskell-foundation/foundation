{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import qualified Prelude
import GHC.ST

import Foundation
import Foundation.Collection
import BenchUtil.Common
import BenchUtil.RefData

#ifdef BENCH_ALL
import qualified Data.Text as Text
type TextText = Text.Text

textPack = Text.pack
textLength = Text.length
textSplitAt = Text.splitAt
textTake    = Text.take
textAny     = Text.any
#else
data TextText = Text

textPack _ = Text
textLength = undefined
textSplitAt _ _ = (undefined, undefined)
textTake    = undefined
textAny     = undefined
#endif

--------------------------------------------------------------------------

benchsString = bgroup "String"
    [ benchLength
    , benchElem
    , benchTake
    , benchSplitAt
    , benchBuildable
    ]
  where
    diffTextString :: (String -> a)
                   -> (TextText -> b)
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
        t = textPack dat

    benchLength = bgroup "Length" $
        fmap (\(n, dat) -> bgroup n $ diffTextString length textLength dat)
            [ ("ascii", rdFoundationEn)
            , ("mascii", rdFoundationHun)
            , ("uni1" ,rdFoundationJap)
            , ("uni2" ,rdFoundationZh)
            ]
    benchElem = bgroup "Elem" $
        fmap (\(n, dat) -> bgroup n $ diffTextString (elem '.') (textAny (== '.')) dat)
            [ ("ascii" , rdFoundationEn)
            , ("mascii", rdFoundationHun)
            , ("uni1"  , rdFoundationJap)
            , ("uni2"  , rdFoundationZh)
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

    benchBuildable = bgroup "Buildable" $
        fmap (\(n, dat) -> bench n $ toString (\es -> runST $ build 128 $ Prelude.mapM_ append es) dat)
            [ ("ascii" , rdFoundationEn)
            , ("mascii", rdFoundationHun)
            , ("uni1"  , rdFoundationJap)
            , ("uni2"  , rdFoundationZh)
            ]

    toString :: ([Char] -> String) -> [Char] -> Benchmarkable
    toString = whnf

--------------------------------------------------------------------------

benchsTypes = bgroup "types"
    [ benchsString
    ]

main = defaultMain
    [ benchsTypes ]
