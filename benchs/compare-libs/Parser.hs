{-# LANGUAGE OverloadedStrings #-}

module Main where

import Foundation as F
import qualified Foundation.Parser as P
import Criterion.Main

dat :: F.String
dat = "EHLO sub.smtp.domain.mail.com"

newtype Domain = Domain [F.String]

authorisedNodeElem :: [Char]
authorisedNodeElem = ['a'..'z'] <> ['A'..'Z'] <> "-_" <> ['0' .. '9']

parseNode :: P.Parser F.String F.String
parseNode = P.takeWhile $ \x -> x `elem` authorisedNodeElem

parseNodes :: P.Parser F.String [F.String]
parseNodes = (parseNode >>= \x -> parseNodes_ >>= \xs -> return (x : xs)) <|> return []
parseNodes_ :: P.Parser F.String [F.String]
parseNodes_ = P.element '.' >> parseNodes

parseDomain :: P.Parser F.String Domain
parseDomain = Domain <$> parseNodes

parseEHLO :: P.Parser F.String Domain
parseEHLO = do
    P.elements "EHLO" <|> P.elements "HELO"
    P.element ' '
    parseDomain

main = defaultMain
    [ bgroup "parse"
        [ bench "EHLO" $ whnf (P.parse parseEHLO) dat
        ]
    ]
