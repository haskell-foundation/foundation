#!/usr/bin/env stack
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Foundation
import Foundation.Conduit
import Foundation.Conduit.Textual
import Foundation.IO
import Foundation.String (String, Encoding(..))
import Foundation.Collection

-- | Capitalize all 'a' and returns all other character as is
remap = await >>= maybe (return ()) (\s -> yield (capitalizeA s <> "\n") >> remap)
  where
    capitalizeA :: String -> String
    capitalizeA s = flip imap s $ \c ->
        case c of
            'a' -> 'A'
            _   -> c

main = runConduit $ sourceHandle stdin
                 .| fromBytes UTF8 .| lines
                 .| remap
                 .| toBytes UTF8
                 .| sinkHandle stdout
