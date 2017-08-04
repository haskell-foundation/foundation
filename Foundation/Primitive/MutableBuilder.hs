{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Foundation.Primitive.MutableBuilder
    ( Builder(..)
    , BuildingState(..)
    ) where

import           Foundation.Primitive.Compat.Base
import           Foundation.Primitive.Compat.MonadTrans
import           Foundation.Primitive.Types.OffsetSize
import           Foundation.Primitive.Monad

newtype Builder collection mutCollection step state err a = Builder
    { runBuilder :: State (Offset step, BuildingState collection mutCollection step (PrimState state), Maybe err) state a }
    deriving (Functor, Applicative, Monad)

-- | The in-progress state of a building operation.
--
-- The previous buffers are in reverse order, and
-- this contains the current buffer and the state of
-- progress packing the elements inside.
data BuildingState collection mutCollection step state = BuildingState
    { prevChunks     :: [collection]
    , prevChunksSize :: !(CountOf step)
    , curChunk       :: mutCollection state
    , chunkSize      :: !(CountOf step)
    }

instance Monad state => MonadFailure (Builder collection mutCollection step state err) where
    type Failure (Builder collection mutCollection step state err) = err
    mFail builderError = Builder $ State $ \(offset, bs, _)  ->
        return ((), (offset, bs, Just builderError))
