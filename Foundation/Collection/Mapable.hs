-- |
-- Module      : Foundation.Primitive.Mapable
-- License     : BSD-style
-- Maintainer  : Nicolas Di Prima <nicolas@primetype.co.uk>
-- Stability   : experimental
-- Portability : portable
--
--
-- Class of data structures that can be traversed from left to right,
-- performing an action on each element.
--
module Foundation.Collection.Mapable
    ( Mapable(..)
    , traverse_
    , mapM_
    , forM
    , forM_
    ) where

import           Foundation.Internal.Base
import qualified Data.Traversable
import           Foundation.Array.Boxed (Array)

class Functor collection => Mapable collection where
    {-# MINIMAL traverse | sequenceA #-}

    -- | Map each element of a structure to an action, evaluate these actions
    -- from left to right, and collect the results. For a version that ignores
    -- the results see 'Foundation.Collection.traverse_'.
    traverse :: Applicative f => (a -> f b)
                              -> collection a
                              -> f (collection b)
    traverse f = sequenceA . fmap f
    sequenceA :: Applicative f => collection (f a)
                               -> f (collection a)
    sequenceA = traverse id

    mapM :: Monad m => (a -> m b) -> collection a -> m (collection b)
    mapM = traverse

    sequence :: Monad m => collection (m a) -> m (collection a)
    sequence = sequenceA

traverse_ :: (Mapable col, Applicative f) => (a -> f b) -> col a -> f ()
traverse_ f col = traverse f col *> pure ()

forM :: (Mapable col, Monad m) => col a -> (a -> m b) -> m (col b)
forM = flip mapM

forM_ :: (Mapable col, Monad m) => col a -> (a -> m b) -> m ()
forM_ = flip mapM_

mapM_ :: (Mapable col, Monad m) => (a -> m b) -> col a -> m ()
mapM_ = traverse_

----------------------------
-- Foldable instances
----------------------------

instance Mapable [] where
    {-# INLINE traverse #-}
    traverse = Data.Traversable.traverse

instance Mapable Array where
    -- | TODO: to optimise
    traverse f arr = fromList <$> traverse f (toList arr)
