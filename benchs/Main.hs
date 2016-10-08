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
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
#else
import qualified Fake.ByteString as ByteString
import qualified Fake.Text as Text
#endif

--------------------------------------------------------------------------

benchsString = bgroup "String"
    [ benchLength
    , benchElem
    , benchTake
    , benchSplitAt
    , benchBuildable
    , benchReverse
    , benchFilter
    ]
  where
    diffTextString :: (String -> a)
                   -> (Text.Text -> b)
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
        t = Text.pack dat

    allDat = [ ("ascii", rdFoundationEn)
             , ("mascii", rdFoundationHun)
             , ("uni1" ,rdFoundationJap)
             , ("uni2" ,rdFoundationZh)
             ]
    allDatSuffix s = fmap (first (\x -> x <> "-" <> s)) allDat

    benchLength = bgroup "Length" $
        fmap (\(n, dat) -> bgroup n $ diffTextString length Text.length dat)
            allDat
    benchElem = bgroup "Elem" $
        fmap (\(n, dat) -> bgroup n $ diffTextString (elem '.') (Text.any (== '.')) dat)
            allDat
    benchTake = bgroup "Take" $ mconcat $ fmap (\p ->
        fmap (\(n, dat) -> bgroup n $ diffTextString (take p) (Text.take p) dat)
                $ allDatSuffix (show p)
            ) [ 10, 100, 800 ]
    benchSplitAt = bgroup "SplitAt" $ mconcat $ fmap (\p ->
        fmap (\(n, dat) -> bgroup n $ diffTextString (fst . splitAt p) (fst . Text.splitAt p) dat)
                $ allDatSuffix (show p)
            ) [ 10, 100, 800 ]

    benchBuildable = bgroup "Buildable" $
        fmap (\(n, dat) -> bench n $ toString (\es -> runST $ build 128 $ Prelude.mapM_ append es) dat)
            allDat

    benchReverse = bgroup "Reverse" $
        fmap (\(n, dat) -> bgroup n $ diffTextString reverse Text.reverse dat)
            allDat

    benchFilter = bgroup "Filter" $
        fmap (\(n, dat) -> bgroup n $ diffTextString (filter (> 'b')) (Text.filter (> 'b')) dat)
            allDat

    toString :: ([Char] -> String) -> [Char] -> Benchmarkable
    toString = whnf

--------------------------------------------------------------------------
benchsByteArray = bgroup "ByteArray"
    [ benchLength
    , benchTake
    , benchBreakElem
    , benchReverse
    , benchFilter
    --, benchSplitAt
    ]
  where
    diffByteString :: (UArray Word8 -> a)
                   -> (ByteString.ByteString -> b)
                   -> [Word8]
                   -> [Benchmark]
    diffByteString foundationBench textBench dat =
        [ bench "UArray_W8" $ whnf foundationBench s
#ifdef BENCH_ALL
        , bench "ByteString" $ whnf textBench t
#endif
        ]
      where
        s = fromList dat
        t = ByteString.pack dat

    allDat =
        [ ("bs20", rdBytes20)
        , ("bs200", rdBytes200)
        , ("bs2000", rdBytes2000)
        ]
    allDatSuffix s = fmap (first (\x -> x <> "-" <> s)) allDat

    benchLength = bgroup "Length" $
        fmap (\(n, dat) -> bgroup n $ diffByteString length ByteString.length dat) allDat

    benchTake = bgroup "Take" $ mconcat $ fmap (\p ->
        fmap (\(n, dat) -> bgroup n $ diffByteString (take p) (ByteString.take p) dat)
            $ allDatSuffix (show p)
        ) [ 0, 10, 100 ]

    benchBreakElem = bgroup "BreakElem" $ mconcat $ fmap (\p ->
        fmap (\(n, dat) -> bgroup n $ diffByteString (fst . breakElem p) (fst . ByteString.break (== p)) dat)
                $ allDatSuffix (show p)
        ) [ 19, 199, 0 ]

    benchReverse = bgroup "Reverse" $
        fmap (\(n, dat) -> bgroup n $ diffByteString reverse ByteString.reverse dat) allDat

    benchFilter = bgroup "Filter" $
        fmap (\(n, dat) -> bgroup n $ diffByteString (filter (> 100)) (ByteString.filter (> 100)) dat) allDat
--------------------------------------------------------------------------

benchsTypes = bgroup "types"
    [ benchsString
    , benchsByteArray
    ]

main = defaultMain
    [ benchsTypes ]
