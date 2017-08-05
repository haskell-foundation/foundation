{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import qualified Prelude
import GHC.ST

import Foundation
import Foundation.Collection
import Basement.Block (Block)
import Foundation.String.Read
import Foundation.String
import BenchUtil.Common
import BenchUtil.RefData

import Sys

#ifdef BENCH_ALL
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString (readInt, readInteger)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector.Unboxed as Vector
#else
import qualified Fake.ByteString as ByteString
import qualified Fake.Text as Text
import qualified Fake.Vector as Vector
#endif

--------------------------------------------------------------------------

benchsString = bgroup "String"
    [ benchLength
    , benchUnpack
    , benchElem
    , benchTake
    , benchSplitAt
    , benchBuildable
    , benchReverse
    , benchFilter
    , benchRead
    , benchFromUTF8Bytes
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

    diffToTextString :: (UArray Word8 -> String)
                     -> (ByteString.ByteString -> Text.Text)
                     -> [Word8]
                     -> [Benchmark]
    diffToTextString foundationBench textBench dat =
        [ bench "String" $ whnf foundationBench s
#ifdef BENCH_ALL
        , bench "Text"   $ whnf textBench t
#endif
        ]
      where
        s = fromList dat
        t = ByteString.pack dat


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
    benchUnpack = bgroup "Unpack" $
        fmap (\(n, dat) -> bgroup n $ diffTextString (length . toList) (length . Text.unpack) dat)
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
        fmap (\(n, dat) -> bench n $ toString (\es -> runST $ build_ 128 $ Prelude.mapM_ append es) dat)
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

    benchFromUTF8Bytes = bgroup "FromUTF8" $
        fmap (\(n, dat) -> bgroup n $ diffToTextString (fst . fromBytes UTF8) (Text.decodeUtf8) dat)
             (fmap (second (toList . toBytes UTF8 . fromList)) allDat)

    toString :: ([Char] -> String) -> [Char] -> Benchmarkable
    toString = whnf

--------------------------------------------------------------------------
benchsByteArray = bgroup "ByteArray"
    [ benchLength
    , benchTake
    , benchSplitAt
    , benchBreakElem
    , benchTakeWhile
    , benchFoldl
    , benchFoldl1
    , benchFoldr
    , benchReverse
    , benchFilter
    , benchAll
    ]
  where
    diffByteArray :: (UArray Word8 -> a)
                   -> (Block Word8 -> b)
                   -> (ByteString.ByteString -> c)
                   -> (Vector.Vector Word8 -> d)
                   -> [Word8]
                   -> [Benchmark]
    diffByteArray uarrayBench blockBench bsBench vectorBench dat =
        [ bench "UArray_W8" $ whnf uarrayBench s
        , bench "Block_W8" $ whnf blockBench s'
#ifdef BENCH_ALL
        , bench "ByteString" $ whnf bsBench t
        , bench "Vector_W8" $ whnf vectorBench v
#endif
        ]
      where
        s = fromList dat
        s' = fromList dat
        t = ByteString.pack dat
        v = Vector.fromList dat

    allDat =
        [ ("bs20", rdBytes20)
        , ("bs200", rdBytes200)
        , ("bs2000", rdBytes2000)
        ]
    allDatSuffix s = fmap (first (\x -> x <> "-" <> s)) allDat

    benchLength = bgroup "Length" $
        fmap (\(n, dat) -> bgroup n $ diffByteArray length length ByteString.length Vector.length dat) allDat

    benchTake = bgroup "Take" $ mconcat $ fmap (\p ->
        fmap (\(n, dat) -> bgroup n $ diffByteArray (take (CountOf p)) (take (CountOf p))
                                                    (ByteString.take p) (Vector.take p) dat)
            $ allDatSuffix (show p)
        ) [ 0, 10, 100 ]

    benchSplitAt = bgroup "SplitAt" $ mconcat $ fmap (\p ->
        fmap (\(n, dat) -> bgroup n $ diffByteArray (fst . splitAt (CountOf p)) (fst . splitAt (CountOf p))
                                                    (fst . ByteString.splitAt p) (fst . Vector.splitAt p) dat)
                $ allDatSuffix (show p)
        ) [ 19, 199, 0 ]

    benchBreakElem = bgroup "BreakElem" $ mconcat $ fmap (\p ->
        fmap (\(n, dat) -> bgroup n $ diffByteArray (fst . breakElem p) (fst . breakElem p)
                                                    (fst . ByteString.break (== p)) (fst . Vector.break (== p)) dat)
                $ allDatSuffix (show p)
        ) [ 19, 199, 0 ]

    benchTakeWhile = bgroup "TakeWhile" $ fmap (\(n, dat) ->
            bgroup n $ diffByteArray (takeWhile (< 0x80)) (takeWhile (< 0x80))
                                     (ByteString.takeWhile (< 0x80)) (Vector.takeWhile (< 0x80)) dat)
                $ allDat

    benchFoldl = bgroup "Foldl" $ fmap (\(n, dat) ->
            bgroup n $ diffByteArray (foldl' (+) 0) (foldl' (+) 0)
                                     (ByteString.foldl' (+) 0) (Vector.foldl' (+) 0) dat)
                $ allDat

    benchFoldl1 = bgroup "Foldl1" $ fmap (\(n, dat) ->
            bgroup n $ diffByteArray (foldl1' (+) . nonEmpty_) (foldl1' (+) . nonEmpty_)
                                     (ByteString.foldl1' (+)) (Vector.foldl1' (+)) dat)
                $ allDat

    benchFoldr = bgroup "Foldr" $ fmap (\(n, dat) ->
            bgroup n $ diffByteArray (foldr (+) 1) (foldr (+) 1)
                                     (ByteString.foldr (+) 1) (Vector.foldr (+) 1) dat)
                $ allDat

    benchAll = bgroup "All" $ fmap (\(n, dat) ->
            bgroup n $ diffByteArray (all (> 0)) (all (> 0))
                                     (ByteString.all (> 0)) (Vector.all (> 0)) dat)
                $ allDat

    benchAny = bgroup "Any" $ fmap (\(n, dat) ->
            bgroup n $ diffByteArray (any (== 80)) (any (== 80))
                                     (ByteString.any (== 80)) (Vector.any (== 80)) dat)
                $ allDat

    benchReverse = bgroup "Reverse" $
        fmap (\(n, dat) -> bgroup n $ diffByteArray reverse reverse ByteString.reverse Vector.reverse dat) allDat

    benchFilter = bgroup "Filter" $
        fmap (\(n, dat) -> bgroup n $ diffByteArray (filter (> 100)) (filter (> 100))
                                                    (ByteString.filter (> 100))
                                                    (Vector.filter (> 100)) dat) allDat

--------------------------------------------------------------------------

benchsTypes = bgroup "types"
    [ benchsString
    , benchsByteArray
    ]

main = defaultMain
    [ benchsTypes
    , bgroup "Sys" benchSys
    ]
