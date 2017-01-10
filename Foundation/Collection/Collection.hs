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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module Foundation.Collection.Collection
    ( Collection(..)
    -- * NonEmpty Property
    , NonEmpty
    , getNonEmpty
    , nonEmpty
    , nonEmpty_
    ) where

import           Foundation.Internal.Base
import           Foundation.Collection.Element
import qualified Data.List
import qualified Foundation.Array.Unboxed as UV

-- | NonEmpty property for any Collection
--
-- This can only be made, through the 'nonEmpty' smart contructor
newtype NonEmpty a = NonEmpty { getNonEmpty :: a }
    deriving (Show,Eq)

-- | Smart constructor to create a NonEmpty collection
--
-- If the collection is empty, then Nothing is returned
-- Otherwise, the collection is wrapped in the NonEmpty property
nonEmpty :: Collection c => c -> Maybe (NonEmpty c)
nonEmpty c
    | null c    = Nothing
    | otherwise = Just (NonEmpty c)

-- | same as 'nonEmpty', but assume that the collection is non empty,
-- and return an asynchronous error if it is.
nonEmpty_ :: Collection c => c -> NonEmpty c
nonEmpty_ c
    | null c    = error "nonEmpty_: assumption failed: collection is empty. consider using nonEmpty and adding proper cases"
    | otherwise = NonEmpty c

type instance Element (NonEmpty a) = Element a

instance Collection c => IsList (NonEmpty c) where
    type Item (NonEmpty c) = Item c
    toList   = toList . getNonEmpty
    fromList = nonEmpty_ . fromList

-- | A set of methods for ordered colection
class (IsList c, Item c ~ Element c) => Collection c where
    {-# MINIMAL null, length, (elem | notElem), minimum, maximum, all, any #-}
    -- | Check if a collection is empty
    null :: c -> Bool
    -- | Length of a collection (number of Element c)
    length :: c -> Int
    -- | Check if a collection contains a specific element
    --
    -- This is the inverse of `notElem`.
    elem :: forall a . (Eq a, a ~ Element c) => Element c -> c -> Bool
    elem e col = not $ e `notElem` col
    -- | Check if a collection does *not* contain a specific element
    --
    -- This is the inverse of `elem`.
    notElem :: forall a . (Eq a, a ~ Element c) => Element c -> c -> Bool
    notElem e col = not $ e `elem` col

    -- | Get the maximum element of a collection
    maximum :: forall a . (Ord a, a ~ Element c) => NonEmpty c -> Element c
    -- | Get the minimum element of a collection
    minimum :: forall a . (Ord a, a ~ Element c) => NonEmpty c -> Element c

    -- | Determine is any elements of the collection satisfy the predicate
    any :: (Element c -> Bool) -> c -> Bool

    -- | Determine is all elements of the collection satisfy the predicate
    all :: (Element c -> Bool) -> c -> Bool

instance Collection [a] where
    null = Data.List.null
    length = Data.List.length

    elem = Data.List.elem
    notElem = Data.List.notElem

    minimum = Data.List.minimum . getNonEmpty
    maximum = Data.List.maximum . getNonEmpty

    any = Data.List.any
    all = Data.List.all

instance UV.PrimType ty => Collection (UV.UArray ty) where
    null = UV.null
    length = UV.length
    elem = UV.elem
    minimum = Data.List.minimum . toList . getNonEmpty
    maximum = Data.List.maximum . toList . getNonEmpty
    all p = Data.List.all p . toList
    any p = Data.List.any p . toList

instance Collection c => Collection (NonEmpty c) where
    null _ = False
    length = length . getNonEmpty
    elem e = elem e . getNonEmpty
    maximum = maximum . getNonEmpty
    minimum = minimum . getNonEmpty
    all p = all p . getNonEmpty
    any p = any p . getNonEmpty
