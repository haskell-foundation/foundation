-- |
-- Module      : Foundation.Internal.Environment
-- License     : BSD-style
-- Maintainer  : foundation
-- Stability   : experimental
-- Portability : portable
--
-- Global configuration environment

{-# LANGUAGE CPP #-}

module Foundation.Internal.Environment
    ( unsafeUArrayUnpinnedMaxSize
    ) where

import           Foundation.Internal.Base
import           Foundation.Internal.Types
import           System.IO.Unsafe          (unsafePerformIO)

#if MIN_VERSION_base(4,6,0)
import           System.Environment (lookupEnv)
import           Text.Read (readMaybe)
#else
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

-- | Defines the maximum size in bytes of unpinned arrays.
--
-- You can change this value by setting the environment variable
-- @HS_FOUNDATION_UARRAY_UNPINNED_MAX@ to an unsigned integer number.
--
-- Note: We use 'unsafePerformIO' here. If the environment variable
-- changes during runtime and the runtime system decides to recompute
-- this value, referential transparency is violated (like the First
-- Order violated the Galactic Concordance!).
--
-- TODO The default value of 1024 bytes is arbitrarily chosen for now.
unsafeUArrayUnpinnedMaxSize :: Size8
unsafeUArrayUnpinnedMaxSize = unsafePerformIO $ do
    maxSize <- (>>= readMaybe) <$> lookupEnv "HS_FOUNDATION_UARRAY_UNPINNED_MAX"
    return $ maybe (Size 1024) Size maxSize
{-# NOINLINE unsafeUArrayUnpinnedMaxSize #-}
