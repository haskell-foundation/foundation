#!/usr/bin/env stack
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Foundation
import           Foundation.Conduit
import           Foundation.Conduit.Textual
import qualified Foundation.String as S
import           Foundation.IO
import           Foundation.JSON.Types
import           Foundation.JSON.Parse (jsonParse, Result(..), jsonParseContextNew, defaultParseConfiguration, JsonParseContext)
import           Foundation.String (String, Encoding(..))
import           Foundation.Collection

-- | to json events
toEvent :: Monad m => JsonParseContext -> Conduit String JsonEvent m ()
toEvent argPc = loop (jsonParse argPc)
  where
    loop pc = await >>= maybe (jsonFinish pc) (jsonNext pc)
    jsonFinish _pc = return ()
    jsonNext parseString s = do
        case parseString s of
            ParseFail err      -> error (show err)
            ParseMore evs cont -> mapM_ yield evs >> loop (\x -> cont (S.toBytes S.UTF8 x))
            ParseOK   _ evs () -> mapM_ yield evs >> return ()

mapConduit :: Monad m => (t -> o) -> Conduit t o m ()
mapConduit f = await >>= maybe (pure ()) (\x -> yield (f x) >> mapConduit f)

chunk :: (Sequential o, Monad m) => Int -> Conduit o o m ()
chunk n = loop
  where
    loop = await >>= maybe (pure ()) splitN
    splitN x
        | null x    = loop
        | otherwise = do
            let (b1,b2) = splitAt n x
            yield b1
            splitN b2

main :: IO ()
main = runConduit $ sourceHandle stdin
                 .| chunk 34
                 .| fromBytes UTF8
                 .| toEvent (jsonParseContextNew defaultParseConfiguration)
                 .| mapConduit (\x -> show x <> "\n")
                 .| toBytes UTF8
                 .| sinkHandle stdout
