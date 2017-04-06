{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Foundation.Boot.Builder
    ( Builder(..)
    , BuildingState(..)
    ) where

import           Foundation.Internal.Base
import           Foundation.Internal.MonadTrans
import           Foundation.Primitive.Types.OffsetSize
import           Foundation.Primitive.Monad

newtype Builder collection mutCollection step state a = Builder
    { runBuilder :: State (Offset step, BuildingState collection mutCollection step (PrimState state)) state a }
    deriving (Functor, Applicative, Monad)

-- | The in-progress state of a building operation.
--
-- The previous buffers are in reverse order, and
-- this contains the current buffer and the state of
-- progress packing the elements inside.
data BuildingState collection mutCollection step state = BuildingState
    { prevChunks     :: [collection]
    , prevChunksSize :: !(Size step)
    , curChunk       :: mutCollection state
    , chunkSize      :: !(Size step)
    }
