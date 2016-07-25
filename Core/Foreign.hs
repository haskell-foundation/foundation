-- |
-- Module      : Core.Foreign
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
module Core.Foreign
    ( module Core.Primitive.FinalPtr
    , V.foreignMem
    , V.mutableForeignMem
    , fileMapRead
    ) where

import           Core.Internal.Types
import           Core.Internal.Base
import           Core.VFS (FilePath)
import           Core.Primitive.FinalPtr
import qualified Core.Array.Unboxed as V
import qualified Core.Array.Unboxed.Mutable as V
import qualified Core.Foreign.MemoryMap as I
import qualified Prelude

-- | Map in memory the whole content of a file
fileMapRead :: FilePath -> IO (V.UArray Word8)
fileMapRead fp = do
    (fptr, FileSize sz) <- I.fileMapRead fp
    return $ V.foreignMem fptr (Prelude.fromIntegral sz)
