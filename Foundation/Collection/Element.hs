-- |
-- Module      : Foundation.Array.Element
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
module Foundation.Collection.Element
    ( Element
    ) where

import Foundation.Internal.Base
import Foundation.Primitive.Block (Block)
import Foundation.Array.Unboxed (UArray)
import Foundation.Array.Boxed (Array)
import Foundation.String.UTF8 (String)

-- | Element type of a collection
type family Element container
type instance Element [a] = a
type instance Element (Block ty) = ty
type instance Element (UArray ty) = ty
type instance Element (Array ty) = ty
type instance Element String = Char
