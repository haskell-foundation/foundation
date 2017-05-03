-- |
-- Module      : Foundation.Internal.Environment
-- License     : BSD-style
-- Maintainer  : foundation
--
-- environment variable compat

{-# LANGUAGE CPP #-}

module Foundation.Internal.Environment
    ( lookupEnv, readMaybe
    ) where

#if MIN_VERSION_base(4,6,0)
import           System.Environment (lookupEnv)
import           Text.Read (readMaybe)
#else
import           Foundation.Internal.Base
import           System.Environment (getEnvironment)
import           Data.List (lookup)
import           Text.Read (Read, minPrec, readPrec, lift)
import           Text.ParserCombinators.ReadP as P
import           Text.ParserCombinators.ReadPrec (readPrec_to_S)
#endif

#if !MIN_VERSION_base(4,6,0)
lookupEnv :: [Char] -> IO (Maybe [Char])
lookupEnv envName = lookup envName <$> getEnvironment

readEither :: Read a => [Char] -> Either [Char] a
readEither s =
  case [ x | (x,"") <- readPrec_to_S read' minPrec s ] of
    [x] -> Right x
    []  -> Left "Prelude.read: no parse"
    _   -> Left "Prelude.read: ambiguous parse"
 where
  read' =
    do x <- readPrec
       lift P.skipSpaces
       return x

readMaybe :: Read a => [Char] -> Maybe a
readMaybe s = case readEither s of
    Left _  -> Nothing
    Right a -> Just a

#endif
