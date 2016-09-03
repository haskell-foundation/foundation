{-# LANGUAGE OverloadedStrings #-}

module Main where

import Foundation as F
import Foundation.String as F
import Foundation.Primitive
import qualified Foundation.Parser as F
import qualified Foundation.Collection as F hiding (take)
import Criterion.Main

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as A

refBufStr :: [Char]
refBufStr = "Foundation, the new hope"
refBufStrLarge :: [Char]
refBufStrLarge = F.intercalate " " $ replicate 100 refBufStr

foundationBufStr :: F.String
foundationBufStr = F.fromList refBufStr
foundationBufStrLarge :: F.String
foundationBufStrLarge = F.fromList refBufStrLarge

foundationParserStr :: F.Parser F.String F.String
foundationParserStr = do
    F.string "Foundation"
    F.element ',' >> F.element ' '
    F.take 12
foundationParserStrLarge :: F.Parser F.String [F.String]
foundationParserStrLarge = do
    x <- foundationParserStr
    (:) x <$> F.some (F.element ' ' >> foundationParserStr)

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
        ]
    , bgroup "Attoparsec"
        [ bench "text:small" $ whnf (A.parseOnly textParser) refText
        , bench "text:large" $ whnf (A.parseOnly textParserLarge) refTextLarge
        ]
    ]
