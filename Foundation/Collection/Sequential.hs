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
{-# LANGUAGE DefaultSignatures #-}
module Foundation.Collection.Sequential
    ( Sequential(..)
    ) where

import           Foundation.Internal.Base
import           Foundation.Numerical.Subtractive
import           Foundation.Primitive.Types.OffsetSize
import           Foundation.Collection.Element
import           Foundation.Collection.Collection
import qualified Foundation.Collection.List as ListExtra
import qualified Data.List
import qualified Foundation.Array.Unboxed as UV
import qualified Foundation.Primitive.Block as BLK
import qualified Foundation.Array.Boxed as BA
import qualified Foundation.String.UTF8 as S

-- | A set of methods for ordered colection
class (IsList c, Item c ~ Element c, Monoid c, Collection c) => Sequential c where
    {-# MINIMAL ((take, drop) | splitAt)
              , ((revTake, revDrop) | revSplitAt)
              , splitOn
              , (break | span)
              , intersperse
              , filter, reverse
              , uncons, unsnoc, snoc, cons
              , find, sortBy, singleton
              , replicate
              #-}

    -- | Take the first @n elements of a collection
    take :: CountOf (Element c) -> c -> c
    take n = fst . splitAt n

    -- | Take the last @n elements of a collection
    revTake :: CountOf (Element c) -> c -> c
    revTake n = fst . revSplitAt n

    -- | Drop the first @n elements of a collection
    drop :: CountOf (Element c) -> c -> c
    drop n = snd . splitAt n

    -- | Drop the last @n elements of a collection
    revDrop :: CountOf (Element c) -> c -> c
    revDrop n = snd . revSplitAt n

    -- | Split the collection at the @n'th elements
    splitAt :: CountOf (Element c) -> c -> (c,c)
    splitAt n c = (take n c, drop n c)

    -- | Split the collection at the @n'th elements from the end
    revSplitAt :: CountOf (Element c) -> c -> (c,c)
    revSplitAt n c = (revTake n c, revDrop n c)

    -- | Split on a specific elements returning a list of colletion
    splitOn :: (Element c -> Bool) -> c -> [c]

    -- | Split a collection when the predicate return true
    break :: (Element c -> Bool) -> c -> (c,c)
    break predicate = span (not . predicate)

    -- | Split a collection when the predicate return true
    breakElem :: Eq (Element c) => Element c -> c -> (c,c)
    breakElem c = break (== c)

    -- | Return the longest prefix in the collection that satisfy the predicate
    takeWhile :: (Element c -> Bool) -> c -> c
    takeWhile predicate = fst . span predicate

    -- | Return the longest prefix in the collection that satisfy the predicate
    dropWhile :: (Element c -> Bool) -> c -> c
    dropWhile predicate = snd . span predicate

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

    -- | Partition the elements thtat satisfy the predicate and those that don't
    partition :: (Element c -> Bool) -> c -> (c,c)
    partition predicate c = (filter predicate c, filter (not . predicate) c)

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

    -- | Create a collection with a single element
    singleton :: Element c -> c

    -- | get the first element of a non-empty collection
    head :: NonEmpty c -> Element c
    head nel = maybe (error "head") fst $ uncons (getNonEmpty nel)

    -- | get the last element of a non-empty collection
    last :: NonEmpty c -> Element c
    last nel = maybe (error "last") snd $ unsnoc (getNonEmpty nel)

    -- | Extract the elements after the first element of a non-empty collection.
    tail :: NonEmpty c -> c
    tail nel = maybe (error "tail") snd $ uncons (getNonEmpty nel)

    -- | Extract the elements before the last element of a non-empty collection.
    init :: NonEmpty c -> c
    init nel = maybe (error "init") fst $ unsnoc (getNonEmpty nel)

    -- | Create a collection where the element in parameter is repeated N time
    replicate :: CountOf (Element c) -> Element c -> c

    -- | Takes two collections and returns True iff the first collection is a prefix of the second.
    isPrefixOf :: Eq (Element c) => c -> c -> Bool
    default isPrefixOf :: Eq c => c -> c -> Bool
    isPrefixOf c1 c2
        | len1 > len2  = False
        | len1 == len2 = c1 == c2
        | otherwise    = c1 == take len1 c2
      where
        len1 = length c1
        len2 = length c2

    -- | Takes two collections and returns True iff the first collection is a suffix of the second.
    isSuffixOf :: Eq (Element c) => c -> c -> Bool
    default isSuffixOf :: Eq c => c -> c -> Bool
    isSuffixOf c1 c2
        | len1 > len2  = False
        | len1 == len2 = c1 == c2
        | otherwise    = c1 == revTake len1 c2
      where
        len1 = length c1
        len2 = length c2

    -- | Takes two collections and returns True iff the first collection is an infix of the second.
    isInfixOf :: Eq (Element c) => c -> c -> Bool
    default isInfixOf :: Eq c => c -> c -> Bool
    isInfixOf c1 c2
        | len1 > len2  = False
        | otherwise    = loop 0
      where
        endofs = len2 - len1
        len1 = length c1
        len2 = length c2

        loop i
            | i == endofs = c1 == c2Sub
            | c1 == c2Sub = True
            | otherwise   = loop (succ i)
          where c2Sub = take len1 $ drop i c2

-- Temporary utility functions
mconcatCollection :: (Monoid (Item c), Sequential c) => c -> Element c
mconcatCollection c = mconcat (toList c)

instance Sequential [a] where
    take (CountOf n) = Data.List.take n
    drop (CountOf n) = Data.List.drop n
    revTake (CountOf n) = ListExtra.revTake n
    revDrop (CountOf n) = ListExtra.revDrop n
    splitAt (CountOf n) = Data.List.splitAt n
    revSplitAt (CountOf n) = ListExtra.revSplitAt n
    splitOn = ListExtra.wordsWhen
    break = Data.List.break
    intersperse = Data.List.intersperse
    span = Data.List.span
    dropWhile = Data.List.dropWhile
    takeWhile = Data.List.takeWhile
    filter = Data.List.filter
    partition = Data.List.partition
    reverse = Data.List.reverse
    uncons = ListExtra.uncons
    unsnoc = ListExtra.unsnoc
    snoc c e = c `mappend` [e]
    cons e c = e : c
    find = Data.List.find
    sortBy = Data.List.sortBy
    singleton = (:[])
    replicate (CountOf i) = Data.List.replicate i
    isPrefixOf = Data.List.isPrefixOf
    isSuffixOf = Data.List.isSuffixOf

instance UV.PrimType ty => Sequential (BLK.Block ty) where
    splitAt n = BLK.splitAt n
    revSplitAt n = BLK.revSplitAt n
    splitOn = BLK.splitOn
    break = BLK.break
    intersperse = BLK.intersperse
    span = BLK.span
    filter = BLK.filter
    reverse = BLK.reverse
    uncons = BLK.uncons
    unsnoc = BLK.unsnoc
    snoc = BLK.snoc
    cons = BLK.cons
    find = BLK.find
    sortBy = BLK.sortBy
    singleton = BLK.singleton
    replicate = BLK.replicate

instance UV.PrimType ty => Sequential (UV.UArray ty) where
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
    singleton = fromList . (:[])
    replicate = UV.replicate
    isPrefixOf = UV.isPrefixOf
    isSuffixOf = UV.isSuffixOf

instance Sequential (BA.Array ty) where
    take = BA.take
    drop = BA.drop
    splitAt = BA.splitAt
    revTake = BA.revTake
    revDrop = BA.revDrop
    revSplitAt = BA.revSplitAt
    splitOn = BA.splitOn
    break = BA.break
    intersperse = BA.intersperse
    span = BA.span
    reverse = BA.reverse
    filter = BA.filter
    unsnoc = BA.unsnoc
    uncons = BA.uncons
    snoc = BA.snoc
    cons = BA.cons
    find = BA.find
    sortBy = BA.sortBy
    singleton = BA.singleton
    replicate = BA.replicate
    isSuffixOf = BA.isSuffixOf
    isPrefixOf = BA.isPrefixOf

instance Sequential S.String where
    take = S.take
    drop = S.drop
    splitAt = S.splitAt
    revTake = S.revTake
    revDrop = S.revDrop
    revSplitAt = S.revSplitAt
    splitOn = S.splitOn
    break = S.break
    breakElem = S.breakElem
    intersperse = S.intersperse
    span = S.span
    filter = S.filter
    reverse = S.reverse
    unsnoc = S.unsnoc
    uncons = S.uncons
    snoc = S.snoc
    cons = S.cons
    find = S.find
    sortBy = S.sortBy
    singleton = S.singleton
    replicate = S.replicate
    isSuffixOf = S.isSuffixOf
    isPrefixOf = S.isPrefixOf
    isInfixOf  = S.isInfixOf
