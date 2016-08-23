{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Foundation.Collection.Buildable
    ( Buildable(..)
    , Builder(..)
    , BuildingState(..)
    , MutableCol
    ) where

import qualified Data.List
import           Foundation.Array.Unboxed
import           Foundation.Array.Unboxed.Mutable
import           Foundation.Collection.Element
import           Foundation.Internal.Base
import           Foundation.Internal.MonadTrans
import           Foundation.Internal.Types
import           Foundation.Number
import           Foundation.Primitive.Monad
import           Foundation.Primitive.Types

-- | Collections that can be built chunk by chunk
class Buildable col where
  {-# MINIMAL append, build #-}
  append :: (PrimMonad st) => Element col -> Builder col st ()
  build :: (PrimMonad st)
        => Int -- ^ size of chunks (elements)
        -> Builder col st ()
        -> st col

newtype Builder col st a = Builder
    { runBuilder :: State (BuildingState col (PrimState st)) st a }
    deriving (Functor,Applicative,Monad)

-- | The in progress state of a building operation
--
-- The previous buffers are in reverse order, and
-- this contains the current buffer and the state of
-- progress packing ty inside.
data BuildingState col st = BuildingState
    { prevBuffers   :: [col]
    , currentBuffer :: (MutableCol col) st
    , currentOffset :: !(Offset (Element col))
    , chunkSize     :: !(Size (Element col))
    }

type family MutableCol col :: * -> *
type instance MutableCol (UArray ty) = MUArray ty

instance PrimType ty => Buildable (UArray ty) where
  append v = Builder $ State $ \st ->
      if offsetAsSize (currentOffset st) == chunkSize st
          then do
              newChunk <- new (chunkSize st)
              cur <- unsafeFreeze (currentBuffer st)
              write newChunk 0 v
              return ((), st { prevBuffers   = cur : prevBuffers st
                             , currentOffset = Offset 1
                             , currentBuffer = newChunk
                             })
          else do
              let (Offset ofs) = currentOffset st
              write (currentBuffer st) ofs v
              return ((), st { currentOffset = currentOffset st + Offset 1 })

  build sizeChunksI origab = call origab (Size sizeChunksI)
    where
      call ab sizeChunks = do
          m        <- new sizeChunks
          ((), st) <- runState (runBuilder ab) (BuildingState [] m (Offset 0) sizeChunks)
          current <- unsafeFreezeShrink (currentBuffer st) (offsetAsSize $ currentOffset st)
          return $ mconcat $ Data.List.reverse (current:prevBuffers st)
