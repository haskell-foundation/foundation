-- |
-- Module      : Foundation.Internal.Environment
-- License     : BSD-style
-- Maintainer  : foundation
-- Stability   : experimental
-- Portability : portable
--
-- Global configuration environment
module Foundation.Internal.Environment
    ( unsafeUArrayUnpinnedMaxSize
    ) where

import           Foundation.Internal.Base
import           Foundation.Internal.Types
import           System.Environment
import           System.IO.Unsafe          (unsafePerformIO)
import           Text.Read

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
unsafeUArrayUnpinnedMaxSize = maybe (Size 1024) Size maxSize
  where
    fromEnv = unsafePerformIO $ lookupEnv "HS_FOUNDATION_UARRAY_UNPINNED_MAX"
    maxSize = fromEnv >>= readMaybe
{-# NOINLINE unsafeUArrayUnpinnedMaxSize #-}
