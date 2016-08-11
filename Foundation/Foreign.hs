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
    ) where

import           Foundation.Primitive.FinalPtr
import qualified Foundation.Array.Unboxed as V
import qualified Foundation.Array.Unboxed.Mutable as V
