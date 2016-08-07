-- |
-- Module      : Foundation.Foreign.MemoryMap.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
module Foundation.Foreign.MemoryMap.Types
    ( FileMapReadF
    ) where

import Foundation.Primitive.FinalPtr
import Foundation.Internal.Base
import Foundation.Internal.Types
import Foundation.VFS (FilePath)

type FileMapReadF = FilePath -> IO (FinalPtr Word8, FileSize)
