-- |
-- Module      : Core.IO.Terminal
-- License     : BSD-style
-- Maintainer  : Foundation
-- Stability   : experimental
-- Portability : portable
--
module Core.IO.Terminal
    ( putStrLn
    , putStr
    ) where

import           System.IO (Handle, IOMode)
import qualified System.IO as S
import           Core.Collection
import           Core.VFS
import           Core.Internal.Types
import           Core.Internal.Base
import           Core.String
import           Core.Array
import           Core.Number
import qualified Core.Array.Unboxed.Mutable as V
import qualified Core.Array.Unboxed as V
import qualified Core.String.UTF8 as S
import           Control.Exception (bracket)
import           Foreign.Ptr (plusPtr)
import qualified Prelude

-- | Print a string to standard output
putStr :: String -> IO ()
putStr = Prelude.putStr . toList

-- | Print a string with a newline to standard output
putStrLn :: String -> IO ()
putStrLn = Prelude.putStrLn . toList
