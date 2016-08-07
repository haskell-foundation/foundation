-- |
-- Module      : Foundation.Foreign
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
module Foundation.Foreign
    ( module Foundation.Primitive.FinalPtr
    , V.foreignMem
    , V.mutableForeignMem
    , fileMapRead
    , fileMapReadWith
    ) where

import           Foundation.Internal.Types
import           Foundation.Internal.Base
import           Foundation.VFS (FilePath)
import           Foundation.Primitive.FinalPtr
import qualified Foundation.Array.Unboxed as V
import qualified Foundation.Array.Unboxed.Mutable as V
import qualified Foundation.Foreign.MemoryMap as I
import qualified Prelude

-- | Map in memory the whole content of a file
--
-- Note that the memory mapping is handled by the system, not at the haskell level.
-- The system can modify the content of the memory as any moment under your feet.
--
-- It also have the limitation of your system, no emulation or nice handling of all
-- those corners cases is attempted here.
--
-- for example mapping a large file (> 4G), on a 32 bits system is likely to just
-- fail or returns inconsistent result.
--
-- In doubt, use 'readFile' or other simple routine that brings
-- the content of the file in IO.
fileMapRead :: FilePath -> IO (V.UArray Word8)
fileMapRead fp = do
    (fptr, FileSize sz) <- I.fileMapRead fp
    return $ V.foreignMem fptr (Prelude.fromIntegral sz)

fileMapReadWith :: FilePath -> (V.UArray Word8 -> IO a) -> IO a
fileMapReadWith fp f = do
    (fptr, FileSize sz) <- I.fileMapRead fp
    f (V.foreignMem fptr (Prelude.fromIntegral sz))
