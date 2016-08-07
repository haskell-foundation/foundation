-- |
-- Module      : Core.IO
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- IO Routine
module Core.IO
    (
    -- * Terminal
      Core.IO.Terminal.putStrLn
    , Core.IO.Terminal.putStr
    -- * File
    , Core.IO.File.IOMode(..)
    , Core.IO.File.openFile
    , Core.IO.File.closeFile
    , Core.IO.File.withFile
    , Core.IO.File.hGet
    , Core.IO.File.readFile
    , Core.IO.File.foldTextFile
    ) where

import qualified Core.IO.Terminal
import qualified Core.IO.File
