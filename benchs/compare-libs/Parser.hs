{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Foundation as F
import Foundation.String as F
import Foundation.Primitive
import qualified Foundation.Parser as F
import qualified Foundation.Collection as F hiding (take)
import Criterion.Main

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString.Internal as B
import qualified Prelude

refBufStr :: [Char]
refBufStr = "Foundation, the new hope"
refBufW8 :: [Word8]
refBufW8 = fmap (B.c2w) refBufStr
refBufStrLarge :: [Char]
refBufStrLarge = F.intercalate " " $ Prelude.replicate 100 refBufStr
refBufW8Large :: [Word8]
refBufW8Large = fmap B.c2w refBufStrLarge

foundationBufStr :: F.String
foundationBufStr = F.fromList refBufStr
foundationBufW8 :: F.UArray Word8
foundationBufW8 = F.fromList refBufW8
foundationBufStrLarge :: F.String
foundationBufStrLarge = F.fromList refBufStrLarge
foundationBufW8Large :: F.UArray Word8
foundationBufW8Large = F.fromList refBufW8Large

foundationEl :: F.UArray Word8
foundationEl = F.fromList $ fmap B.c2w "Foundation"

foundationParserStr :: F.Parser F.String F.String
foundationParserStr = do
    F.string "Foundation"
    F.element ',' >> F.element ' '
    F.take 12
foundationParserStrLarge :: F.Parser F.String [F.String]
foundationParserStrLarge = do
    x <- foundationParserStr
    (:) x <$> F.some (F.element ' ' >> foundationParserStr)
foundationParserW8 :: F.Parser (F.UArray Word8) (F.UArray Word8)
foundationParserW8 = do
    F.elements foundationEl
    F.element 0x2C >> F.element 0x20
    F.take 12
foundationParserW8Large :: F.Parser (F.UArray Word8) [F.UArray Word8]
foundationParserW8Large = do
    x <- foundationParserW8
    (:) x <$> F.some (F.element 0x20 >> foundationParserW8)

forceParserStop p b = case F.parse p b of
    F.ParseMore f -> f Nothing
    a             -> a

refText :: T.Text
refText = T.pack refBufStr
refTextLarge :: T.Text
refTextLarge = T.pack refBufStrLarge

textParser :: A.Parser T.Text
textParser = do
    A.string "Foundation"
    A.char ',' >> A.char ' '
    A.take 12
textParserLarge :: A.Parser [T.Text]
textParserLarge = do
    x <- textParser
    (:) x <$> A.many1 (A.char ' ' >> textParser)

main = defaultMain
    [ bgroup "foundation"
        [ bench "string:small" $ whnf (forceParserStop foundationParserStr) foundationBufStr
        , bench "string:large" $ whnf (forceParserStop foundationParserStrLarge) foundationBufStrLarge
        , bench "UArray(W8):small" $ whnf (forceParserStop foundationParserW8) foundationBufW8
        , bench "UArray(W8):large" $ whnf (forceParserStop foundationParserW8Large) foundationBufW8Large
        ]
    , bgroup "Attoparsec"
        [ bench "text:small" $ whnf (either error id . A.parseOnly textParser) refText
        , bench "text:large" $ whnf (either error id . A.parseOnly textParserLarge) refTextLarge
        ]
    ]
