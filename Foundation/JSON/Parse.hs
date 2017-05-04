-- |
-- Module      : Foundation.JSON.Parse
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
--
-- JSON Parser
--
{-# LANGUAGE EmptyDataDecls #-}
module Foundation.JSON.Parse
    ( JsonParseError(..)
    , JsonParseNesting
    , JsonParseConfiguration(..)
    , defaultParseConfiguration
    ) where

import Foundation.Primitive.Imports

data JsonParseError =
      BadChar Word8
    | PopEmpty
    | PopUnexpectedMode
    | NestingLimit
    | DataLimit
    | CommentNotAllowed
    | UnexpectedChar Word8
    | UnicodeMissingLowSurrogate
    | UnicodeUnexpectedLowSurrogate
    | CommaOutOfStructure
    | Callback
    | UTF8
    | EndOfStream
    deriving (Show,Eq)

data JsonParseNesting

data JsonParseConfiguration = JsonParseConfiguration
    { maxNesting       :: Size JsonParseNesting -- ^ maximum level of nesting
    , maxData          :: Size Word8            -- ^ size of data in bytes
    , allowCommentC    :: Bool                  -- ^ allow C style comment
    , allowCommentYaml :: Bool                  -- ^ allow Yaml/Python style comment
    } deriving (Show,Eq)

defaultParseConfiguration :: JsonParseConfiguration
defaultParseConfiguration = JsonParseConfiguration
    { maxNesting       = 64
    , maxData          = 104857 -- 1 mb
    , allowCommentC    = False
    , allowCommentYaml = False
    }
