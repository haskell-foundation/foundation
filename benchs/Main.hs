{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import qualified Prelude
import GHC.ST

import Foundation
import Foundation.Collection
import Foundation.String.Read
import BenchUtil.Common
import BenchUtil.RefData

import Sys

#ifdef BENCH_ALL
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString (readInt, readInteger)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
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
    , benchRead
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

    diffBsTextString :: (String -> a)
                   -> (Text.Text -> b)
                   -> (ByteString.ByteString -> c)
                   -> [Char]
                   -> [Benchmark]
    diffBsTextString foundationBench textBench bytestringBench dat =
        [ bench "String" $ whnf foundationBench s
#ifdef BENCH_ALL
        , bench "Text"   $ whnf textBench t
        , bench "ByteString" $ whnf bytestringBench b
#endif
        ]
      where
        s = fromList dat
        t = Text.pack dat
        b = ByteString.pack $ Prelude.map (fromIntegral . fromEnum) dat

    allDat :: [(String, [Char])]
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
        fmap (\(n, dat) -> bgroup n $ diffTextString (take (CountOf p)) (Text.take p) dat)
                $ allDatSuffix (show p)
            ) [ 10, 100, 800 ]
    benchSplitAt = bgroup "SplitAt" $ mconcat $ fmap (\p ->
        fmap (\(n, dat) -> bgroup n $ diffTextString (fst . splitAt (CountOf p)) (fst . Text.splitAt p) dat)
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

    benchRead = bgroup "Read"
        [ bgroup "Integer"
            [ bgroup "10000" (diffTextString stringReadInteger textReadInteger (toList $ show 10000))
            , bgroup "1234567891234567890" (diffTextString stringReadInteger textReadInteger (toList $ show 1234567891234567890))
            ]
        , bgroup "Int"
            [ bgroup "12345" (diffBsTextString stringReadInt textReadInt bsReadInt (toList $ show 12345))
            ]
        , bgroup "Double"
            [ bgroup "100.56e23" (diffTextString (maybe undefined id . readDouble) (either undefined fst . Text.double) (toList $ show 100.56e23))
            , bgroup "-123.1247" (diffTextString (maybe undefined id . readDouble) (either undefined fst . Text.double) (toList $ show (-123.1247)))
            ]
        ]
      where
        bsReadInt :: ByteString.ByteString -> Int
        bsReadInt = maybe undefined fst . ByteString.readInt
        textReadInt :: Text.Text -> Int
        textReadInt = either undefined fst . Text.decimal
        stringReadInt :: String -> Int
        stringReadInt = maybe undefined id . readIntegral

        bsReadInteger :: ByteString.ByteString -> Integer
        bsReadInteger = maybe undefined fst . ByteString.readInteger
        textReadInteger :: Text.Text -> Integer
        textReadInteger = either undefined fst . Text.decimal
        stringReadInteger :: String -> Integer
        stringReadInteger = maybe undefined id . readIntegral


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
        fmap (\(n, dat) -> bgroup n $ diffByteString (take (CountOf p)) (ByteString.take p) dat)
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
    [ benchsTypes
    , bgroup "Sys" benchSys
    ]
