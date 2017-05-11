-- |
-- Module      : Foundation.JSON.Parse
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
--
-- JSON Parser
--
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE OverloadedStrings #-}
module Foundation.JSON.Parse
    ( JsonParseError(..)
    , JsonParseNesting
    , JsonParseConfiguration(..)
    , defaultParseConfiguration
    , JsonParseContext
    , jsonParseContextNew
    , jsonParse
    , Result(..)
    ) where

import           Foundation.Primitive.Imports
import           Foundation.JSON.Parse.Types
import           Foundation.JSON.Parse.Internal
import           Foundation.String.UTF8
import qualified Data.List as L (reverse)

jsonParse :: JsonParseContext -> String -> Result ()
jsonParse pc s = runParser pGo pc (toBytes UTF8 s) [] $ \_ s' evs -> ParseOK s' (L.reverse evs)
