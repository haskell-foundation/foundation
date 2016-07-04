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

import           Core.Primitive.FinalPtr
import qualified Core.Array.Unboxed as V
import qualified Core.Array.Unboxed.Mutable as V
import           Core.Foreign.MemoryMap
