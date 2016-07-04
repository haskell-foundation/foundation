-- |
-- Module      : Core.Collection
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- Different collections (list, vector, string, ..) unified under 1 API.
-- an API to rules them all, and in the darkness bind them.
--
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
module Core.Collection
    ( Element
    , InnerFunctor(..)
    , Foldable(..)
    , SemiOrderedCollection(..)
    , Sequential(..)
    , MutableCollection(..)
    , IndexedCollection(..)
    , KeyedCollection(..)
    ) where

import           Core.Internal.Base
import           Core.Collection.Element
import           Core.Collection.InnerFunctor
import           Core.Collection.Keyed
import           Core.Collection.Foldable
import           Core.Collection.Sequential
import           Core.Collection.Indexed
import           Core.Collection.Mutable
import qualified Data.List
import qualified Core.Array.Unboxed as UV

-- | A set of methods on non empty ordered collection
--
-- note: Ordered does not mean sorted, it means the
-- elements have a given order that is retained by
-- the data structure.
--
-- TODO: to remove and streamline all those function in different classes.
class SemiOrderedCollection c where
    {-# MINIMAL snoc, cons, find, sortBy, length, singleton #-}
    -- | Prepend an element to an ordered collection
    snoc :: c -> Element c -> c
    -- | Append an element to an ordered collection
    cons :: Element c -> c -> c
    -- | Find an element in an ordered collection
    find :: (Element c -> Bool) -> c -> Maybe (Element c)
    -- | Sort an ordered collection using the specified order function
    sortBy :: (Element c -> Element c -> Ordering) -> c -> c
    -- | Length of a collection (number of Element c)
    length :: c -> Int
    -- | Create a collection with a single element
    singleton :: Element c -> c

instance SemiOrderedCollection [a] where
    snoc c e = c `mappend` [e]
    cons e c = e : c
    find = Data.List.find
    sortBy = Data.List.sortBy
    length = Data.List.length
    singleton = (:[])

instance UV.PrimType ty => SemiOrderedCollection (UV.UArray ty) where
    snoc = UV.snoc
    cons = UV.cons
    find = UV.find
    sortBy = UV.sortBy
    length = UV.length
    singleton = fromList . (:[])
--takeWhile p = fst . span p
