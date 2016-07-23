-- |
-- Module      : Core.Collection.Sequential
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- Different collections (list, vector, string, ..) unified under 1 API.
-- an API to rules them all, and in the darkness bind them.
--
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Core.Collection.Sequential
    ( Sequential(..)
    ) where

import           Core.Internal.Base
import           Core.Collection.Element
import qualified Core.Collection.List as ListExtra
import qualified Data.List
import qualified Core.Array.Unboxed as UV

-- | A set of methods for ordered colection
class (IsList c, Item c ~ Element c, Monoid c) => Sequential c where
    {-# MINIMAL null, ((take, drop) | splitAt)
              , ((revTake, revDrop) | revSplitAt)
              , splitOn
              , (break | span)
              , filter, reverse
              , uncons, unsnoc, snoc, cons
              , find, sortBy, length, singleton #-}

    -- | Check if a collection is empty
    null :: c -> Bool

    -- | Take the first @n elements of a collection
    take :: Int -> c -> c
    take n = fst . splitAt n

    -- | Take the last @n elements of a collection
    revTake :: Int -> c -> c
    revTake n = fst . revSplitAt n

    -- | Drop the first @n elements of a collection
    drop :: Int -> c -> c
    drop n = snd . splitAt n

    -- | Drop the last @n elements of a collection
    revDrop :: Int -> c -> c
    revDrop n = snd . revSplitAt n

    -- | Split the collection at the @n'th elements
    splitAt :: Int -> c -> (c,c)
    splitAt n c = (take n c, drop n c)

    -- | Split the collection at the @n'th elements from the end
    revSplitAt :: Int -> c -> (c,c)
    revSplitAt n c = (revTake n c, revDrop n c)

    -- | Split on a specific elements returning a list of colletion
    splitOn :: (Element c -> Bool) -> c -> [c]

    -- | Split a collection when the predicate return true
    break :: (Element c -> Bool) -> c -> (c,c)
    break predicate = span (not . predicate)

    -- | Split a collection while the predicate return true
    span :: (Element c -> Bool) -> c -> (c,c)
    span predicate = break (not . predicate)

    -- | Filter all the elements that satisfy the predicate
    filter :: (Element c -> Bool) -> c -> c

    -- | Reverse a collection
    reverse :: c -> c

    -- | Decompose a collection into its first element and the remaining collection.
    -- If the collection is empty, returns Nothing.
    uncons :: c -> Maybe (Element c, c)

    -- | Decompose a collection into a collection without its last element, and the last element
    -- If the collection is empty, returns Nothing.
    unsnoc :: c -> Maybe (c, Element c)

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

instance Sequential [a] where
    null = Data.List.null
    take = Data.List.take
    drop = Data.List.drop
    revTake = ListExtra.revTake
    revDrop = ListExtra.revDrop
    splitAt = Data.List.splitAt
    revSplitAt = ListExtra.revSplitAt
    splitOn = ListExtra.wordsWhen
    break = Data.List.break
    span = Data.List.span
    filter = Data.List.filter
    reverse = Data.List.reverse
    uncons = ListExtra.uncons
    unsnoc = ListExtra.unsnoc
    snoc c e = c `mappend` [e]
    cons e c = e : c
    find = Data.List.find
    sortBy = Data.List.sortBy
    length = Data.List.length
    singleton = (:[])

instance UV.PrimType ty => Sequential (UV.UArray ty) where
    null = UV.null
    take = UV.take
    revTake = UV.revTake
    drop = UV.drop
    revDrop = UV.revDrop
    splitAt = UV.splitAt
    revSplitAt = UV.revSplitAt
    splitOn = UV.splitOn
    break = UV.break
    span = UV.span
    filter = UV.filter
    reverse = UV.reverse
    uncons = UV.uncons
    unsnoc = UV.unsnoc
    snoc = UV.snoc
    cons = UV.cons
    find = UV.find
    sortBy = UV.sortBy
    length = UV.length
    singleton = fromList . (:[])
