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
    , module Foreign.C.Types
    ) where

import           Foundation.Primitive.FinalPtr
import qualified Foundation.Primitive.UArray as V
import qualified Foundation.Primitive.UArray.Mutable as V

import           Foreign.C.Types
