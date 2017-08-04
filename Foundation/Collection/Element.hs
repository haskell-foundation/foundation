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

import Basement.Compat.Base
import Basement.Block (Block)
import Basement.UArray (UArray)
import Basement.BoxedArray (Array)
import Basement.String (String)
import Basement.Types.AsciiString (AsciiString)
import Basement.Types.Char7 (Char7)
import Basement.NonEmpty

-- | Element type of a collection
type family Element container
type instance Element [a] = a
type instance Element (Block ty) = ty
type instance Element (UArray ty) = ty
type instance Element (Array ty) = ty
type instance Element String = Char
type instance Element AsciiString = Char7
type instance Element (NonEmpty a) = Element a
