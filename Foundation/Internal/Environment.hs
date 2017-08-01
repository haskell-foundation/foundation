-- |
-- Module      : Foundation.Internal.Environment
-- License     : BSD-style
-- Maintainer  : foundation
--
-- environment variable compat

module Foundation.Internal.Environment
    ( lookupEnv, readMaybe
    ) where

import           System.Environment (lookupEnv)
import           Text.Read (readMaybe)
