-- |
-- Module      : Foundation.Collection.Sequential
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- Different collections (list, vector, string, ..) unified under 1 API.
-- an API to rules them all, and in the darkness bind them.
--
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}
module Foundation.Collection.Sequential
    ( Sequential(..)
    ) where

import           Foundation.Internal.Base
import           Foundation.Collection.Element
import qualified Foundation.Collection.List as ListExtra
import qualified Data.List
import qualified Foundation.Array.Unboxed as UV

-- | A set of methods for ordered colection
class (IsList c, Item c ~ Element c, Monoid c) => Sequential c where
    {-# MINIMAL null, ((take, drop) | splitAt)
              , ((revTake, revDrop) | revSplitAt)
              , splitOn
              , (break | span)
              , intersperse
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

    -- | Split a collection when the predicate return true
    breakElem :: Eq (Element c) => Element c -> c -> (c,c)
    breakElem c = break (== c)

    -- | The 'intersperse' function takes an element and a list and
    -- \`intersperses\' that element between the elements of the list.
    -- For example,
    --
    -- > intersperse ',' "abcde" == "a,b,c,d,e"
    intersperse :: Element c -> c -> c

    -- | 'intercalate' @xs xss@ is equivalent to @('mconcat' ('intersperse' xs xss))@.
    -- It inserts the list @xs@ in between the lists in @xss@ and concatenates the
    -- result.
    intercalate :: Monoid (Item c) => Element c -> c -> Element c
    intercalate xs xss = mconcatCollection (intersperse xs xss)

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

-- Temporary utility functions
mconcatCollection :: (Monoid (Item c), Sequential c) => c -> Element c
mconcatCollection c = mconcat (toList c)

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
    intersperse = Data.List.intersperse
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
    breakElem = UV.breakElem
    intersperse = UV.intersperse
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
