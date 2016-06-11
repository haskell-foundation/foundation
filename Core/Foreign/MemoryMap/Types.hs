-- |
-- Module      : Core.Foreign.MemoryMap.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
module Core.Foreign.MemoryMap.Types
    ( FileMapReadF
    ) where

import Core.Primitive.FinalPtr
import Core.Internal.Base
import Core.Internal.Types
import Core.VFS (FilePath)

type FileMapReadF = FilePath -> IO (FinalPtr Word8, FileSize)
