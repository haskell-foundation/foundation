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
    , OrderedCollection(..)
    , MutableCollection(..)
    , IndexedCollection(..)
    , KeyedCollection(..)
    ) where

import           Core.Internal.Base
import           Core.Collection.List
import           Core.Collection.Element
import           Core.Collection.Keyed
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
class InnerFunctor c => SemiOrderedCollection c where
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

class Foldable collection where
    -- | Left-associative fold of a structure.
    --
    -- In the case of lists, foldl, when applied to a binary operator, a starting value (typically the left-identity of the operator), and a list, reduces the list using the binary operator, from left to right:
    --
    -- > foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
    -- Note that to produce the outermost application of the operator the entire input list must be traversed. This means that foldl' will diverge if given an infinite list.
    --
    -- Also note that if you want an efficient left-fold, you probably want to use foldl' instead of foldl. The reason for this is that latter does not force the "inner" results (e.g. z f x1 in the above example) before applying them to the operator (e.g. to (f x2)). This results in a thunk chain O(n) elements long, which then must be evaluated from the outside-in.
    foldl :: (a -> Element collection -> a) -> a -> collection -> a


    -- | Left-associative fold of a structure but with strict application of the operator.
    foldl' :: (a -> Element collection -> a) -> a -> collection -> a

    -- | Right-associative fold of a structure.
    --
    -- > foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)
    foldr :: (Element collection -> a -> a) -> a -> collection -> a

    -- | Right-associative fold of a structure, but with strict application of the operator.
    foldr' :: (Element collection -> a -> a) -> a -> collection -> a
    foldr' f z0 xs = foldl f' id xs z0 where f' k x z = k $! f x z

-- | A set of methods for ordered colection
class (IsList c, Item c ~ Element c, Monoid c, SemiOrderedCollection c) => OrderedCollection c where
    {-# MINIMAL null, ((take, drop) | splitAt), splitOn, (break | span), filter, reverse #-}
    -- | Check if a collection is empty
    null :: c -> Bool

    -- | Take the first @n elements of a collection
    take :: Int -> c -> c
    take n = fst . splitAt n

    -- | Drop the first @n elements of a collection
    drop :: Int -> c -> c
    drop n = snd . splitAt n

    -- | Split the collection at the @n'th elements
    splitAt :: Int -> c -> (c,c)
    splitAt n c = (take n c, drop n c)

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
instance Foldable [a] where
    foldl = Data.List.foldl
    foldr = Data.List.foldr
    foldl' = Data.List.foldl'
instance SemiOrderedCollection [a] where
    snoc c e = c `mappend` [e]
    cons e c = e : c
    find = Data.List.find
    sortBy = Data.List.sortBy
    length = Data.List.length
    singleton = (:[])
instance OrderedCollection [a] where
    null = Data.List.null
    take = Data.List.take
    drop = Data.List.drop
    splitAt = Data.List.splitAt
    splitOn = wordsWhen
    break = Data.List.break
    span = Data.List.span
    filter = Data.List.filter
    reverse = Data.List.reverse

--takeWhile p = fst . span p
