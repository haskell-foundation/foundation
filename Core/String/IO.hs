-- |
-- Module      : Core.String.IO
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
module Core.String.IO
    ( putStr
    , putStrLn
    ) where

import           Core.Internal.Base
import           Core.String.UTF8
import qualified Prelude

-- | Print a string to standard output
putStr :: String -> IO ()
putStr = Prelude.putStr . toList

-- | Print a string with a newline to standard output
putStrLn :: String -> IO ()
putStrLn = Prelude.putStrLn . toList
