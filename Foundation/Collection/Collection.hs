-- |
-- Module      : Foundation.Collection.Collection
-- License     : BSD-style
-- Maintainer  : Foundation
-- Stability   : experimental
-- Portability : portable
--
-- Provide basic collection information. It's difficult to provide a
-- unified interface to all sorts of collection, but when creating this
-- API we had the following types in mind:
--
-- * List (e.g [a])
-- * Array
-- * Collection of collection (e.g. deque)
-- * Hashtables, Trees
--
-- an API to rules them all, and in the darkness bind them.
--
module Foundation.Collection.Collection
    ( Collection(..)
    ) where

import           Foundation.Internal.Base
import           Foundation.Collection.Element
import qualified Data.List
import qualified Foundation.Array.Unboxed as UV

-- | A set of methods for ordered colection
class (IsList c, Item c ~ Element c, Monoid c) => Collection c where
    -- | Check if a collection is empty
    null :: c -> Bool
    -- | Length of a collection (number of Element c)
    length :: c -> Int

instance Collection [a] where
    null = Data.List.null
    length = Data.List.length

instance UV.PrimType ty => Collection (UV.UArray ty) where
    null = UV.null
    length = UV.length
