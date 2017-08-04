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

import Foundation.Primitive.Compat.Base
import Foundation.Primitive.Block (Block)
import Foundation.Primitive.UArray (UArray)
import Foundation.Primitive.BoxedArray (Array)
import Foundation.Primitive.String (String)
import Foundation.Primitive.Types.AsciiString (AsciiString)
import Foundation.Primitive.Types.Char7 (Char7)
import Foundation.Primitive.NonEmpty

-- | Element type of a collection
type family Element container
type instance Element [a] = a
type instance Element (Block ty) = ty
type instance Element (UArray ty) = ty
type instance Element (Array ty) = ty
type instance Element String = Char
type instance Element AsciiString = Char7
type instance Element (NonEmpty a) = Element a
