-- |
-- Module      : Foundation.IO.Terminal
-- License     : BSD-style
-- Maintainer  : Foundation
-- Stability   : experimental
-- Portability : portable
--
module Foundation.IO.Terminal
    ( putStrLn
    , putStr
    , stdin
    , stdout
    ) where

import           Foundation.Primitive.Imports
import qualified Prelude
import           System.IO (stdin, stdout)

-- | Print a string to standard output
putStr :: String -> IO ()
putStr = Prelude.putStr . toList

-- | Print a string with a newline to standard output
putStrLn :: String -> IO ()
putStrLn = Prelude.putStrLn . toList
