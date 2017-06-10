-- |
-- Module      : Foundation.Collection.NonEmpty
-- License     : BSD-style
-- Maintainer  : Foundation
-- Stability   : experimental
-- Portability : portable
--
-- A newtype wrapper around a non-empty Collection.

module Foundation.Collection.NonEmpty
    ( NonEmpty(..)
    ) where

import Foundation.Internal.Base

-- | NonEmpty property for any Collection
newtype NonEmpty a = NonEmpty { getNonEmpty :: a }
    deriving (Show,Eq)
