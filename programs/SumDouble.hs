#! /usr/bin/env stack
-- stack resolver lts-8.15 --install-ghc script --package foundation
{-# LANGUAGE RebindableSyntax #-}
import Foundation
import Foundation.Conduit
import Foundation.Conduit.Textual
import Foundation.String (Encoding(UTF8))
import Foundation.String.Read (readDouble)
import Foundation.IO

main :: IO ()
main =
        runConduit (sourceHandle stdin .| fromBytes UTF8 .| words .| sinkList)
    >>= putStrLn . show . foldl' (+) 0.0 . catMaybes . fmap readDouble
