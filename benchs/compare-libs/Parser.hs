{-# LANGUAGE OverloadedStrings #-}

module Main where

import Foundation as F
import Foundation.String as F
import Foundation.Primitive
import qualified Foundation.Parser as P
import Criterion.Main

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BC
import qualified Data.Attoparsec.ByteString as Atto

dat :: F.UArray Word8
dat = F.toBytes F.UTF8 "EHLO sub.smtp.domain.mail.com "

datbs :: B.ByteString
datbs = B.pack $ fmap BC.c2w "EHLO sub.smtp.domain.mail.com "

newtype DomainBS = DomainBS [B.ByteString]
    deriving Show

newtype Domain = Domain [F.UArray Word8]
    deriving Show

authorisedNodeElem :: [Word8]
authorisedNodeElem = [0x61..0x7a]

dot :: Word8
dot = 0x2E
space :: Word8
space = 0x20

parseNode :: P.Parser (F.UArray Word8) (F.UArray Word8)
parseNode = P.takeWhile $ \x -> x `elem` authorisedNodeElem

parseNodes :: P.Parser (F.UArray Word8) [F.UArray Word8]
parseNodes = (parseNode >>= \x -> parseNodes_ >>= \xs -> return (x : xs)) <|> return []

parseNodesBS :: Atto.Parser [B.ByteString]
parseNodesBS = do
    x <- parseNodeBS
    xs <- Atto.many' $ Atto.word8 dot *> parseNodeBS
    return $ x:xs
parseNodeBS :: Atto.Parser B.ByteString
parseNodeBS = Atto.takeWhile (\x -> x `elem` authorisedNodeElem)

parseNodes_ :: P.Parser (F.UArray Word8) [F.UArray Word8]
parseNodes_ = P.element dot >> parseNodes

parseDomain :: P.Parser (F.UArray Word8) Domain
parseDomain = Domain <$> parseNodes
parseDomainBS :: Atto.Parser DomainBS
parseDomainBS = DomainBS <$> parseNodesBS

heloF :: F.UArray Word8
heloF = fromList [0x48, 0x45, 0x4C, 0x4F]
ehloF :: F.UArray Word8
ehloF = fromList [0x45, 0x48, 0x4C, 0x4F]
heloBS :: B.ByteString
heloBS = B.pack [0x48, 0x45, 0x4C, 0x4F]
ehloBS :: B.ByteString
ehloBS = B.pack [0x45, 0x48, 0x4C, 0x4F]

parseEHLO :: P.Parser (F.UArray Word8) Domain
parseEHLO = do
    P.elements ehloF <|> P.elements heloF
    P.element space
    parseDomain
parseEHLOBS :: Atto.Parser DomainBS
parseEHLOBS = do
    Atto.string ehloBS <|> Atto.string heloBS
    Atto.word8 space
    parseDomainBS

main = do
  print $ P.parse parseEHLO dat
  print $ Atto.parse parseEHLOBS datbs
  defaultMain
    [ bgroup "parse"
        [ bench "foundation:EHLO" $ whnf (P.parse parseEHLO) dat
        , bench "attoparsec:EHLO" $ whnf (Atto.parse parseEHLOBS) datbs
        ]
    ]
