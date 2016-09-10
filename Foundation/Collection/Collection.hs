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
    {-# MINIMAL null, length, minimum, maximum #-}
    -- | Check if a collection is empty
    null :: c -> Bool
    -- | Length of a collection (number of Element c)
    length :: c -> Int
    -- | Get the maximum element of a collection
    maximum :: Ord (Element c) => NonEmpty c -> Element c
    -- | Get the minimum element of a collection
    minimum :: Ord (Element c) => NonEmpty c -> Element c

instance Collection [a] where
    null = Data.List.null
    length = Data.List.length

    minimum = Data.List.minimum . getNonEmpty
    maximum = Data.List.maximum . getNonEmpty

instance UV.PrimType ty => Collection (UV.UArray ty) where
    null = UV.null
    length = UV.length
    minimum = Data.List.minimum . toList . getNonEmpty
    maximum = Data.List.maximum . toList . getNonEmpty

instance Collection c => Collection (NonEmpty c) where
    null _ = False
    length = length . getNonEmpty
    maximum = maximum . getNonEmpty
    minimum = minimum . getNonEmpty
