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
