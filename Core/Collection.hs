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
import qualified Core.Collection.List as ListExtra
import           Core.Collection.Element
import           Core.Collection.Keyed
import           Core.Collection.Foldable
import           Core.Collection.Indexed
import           Core.Collection.Mutable
import qualified Data.List

-- | A monomorphic functor that maps the inner values to values of the same type
class InnerFunctor c where
    imap :: (Element c -> Element c) -> c -> c
    default imap :: (Functor f, Element (f a) ~ a, f a ~ c) => (a -> a) -> f a -> f a
    imap = fmap

-- | A set of methods on non empty ordered collection
--
-- note: Ordered does not mean sorted, it means the
-- elements have a given order that is retained by
-- the data structure.
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

-- | A set of methods for ordered colection
class (IsList c, Item c ~ Element c, Monoid c) => Sequential c where
    {-# MINIMAL null, ((take, drop) | splitAt), ((revTake, revDrop) | revSplitAt), splitOn, (break | span), filter, reverse #-}
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

instance InnerFunctor [a]
instance SemiOrderedCollection [a] where
    snoc c e = c `mappend` [e]
    cons e c = e : c
    find = Data.List.find
    sortBy = Data.List.sortBy
    length = Data.List.length
    singleton = (:[])
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

--takeWhile p = fst . span p
