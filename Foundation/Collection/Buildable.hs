-- |
-- Module      : Foundation.Collection.Buildable
-- License     : BSD-style
-- Maintainer  : foundation
-- Stability   : experimental
-- Portability : portable
--
-- An interface for collections that can be built incrementally.
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Foundation.Collection.Buildable
    ( Buildable(..)
    , Builder(..)
    , BuildingState(..)
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

-- $setup
-- >>> import Control.Monad.ST
-- >>> import Foundation.Array.Unboxed
-- >>> import Foundation.Internal.Base
-- >>> import Foundation.Internal.Types

-- | Collections that can be built chunk by chunk.
--
-- Use the 'Monad' instance of 'Builder' to chain 'append' operations
-- and feed it into `build`:
--
-- >>> runST $ build 32 (append 'a' >> append 'b' >> append 'c') :: UArray Char
-- "abc"
class Buildable col where
  {-# MINIMAL append, build #-}

  -- | Mutable collection type used for incrementally writing chunks.
  type Mutable col :: * -> *

  -- | Unit of the smallest step possible in an `append` operation.
  --
  -- A UTF-8 character can have a size between 1 and 4 bytes, so this
  -- should be defined as 1 byte for collections of `Char`.
  type Step col

  append :: (PrimMonad prim) => Element col -> Builder col prim ()

  build :: (PrimMonad prim)
        => Int -- ^ Size of a chunk
        -> Builder col prim ()
        -> prim col

newtype Builder col st a = Builder
    { runBuilder :: State (BuildingState col (PrimState st)) st a }
    deriving (Functor, Applicative, Monad)

-- | The in-progress state of a building operation.
--
-- The previous buffers are in reverse order, and
-- this contains the current buffer and the state of
-- progress packing the elements inside.
data BuildingState col st = BuildingState
    { prevBuffers   :: [col]
    , currentBuffer :: Mutable col st
    , currentOffset :: !(Offset (Step col))
    , chunkSize     :: !(Size (Step col))
    }

instance PrimType ty => Buildable (UArray ty) where
  type Mutable (UArray ty) = MUArray ty
  type Step (UArray ty) = ty

  append v = Builder $ State $ \st ->
      if offsetAsSize (currentOffset st) == chunkSize st
          then do
              newChunk  <- new (chunkSize st)
              cur       <- unsafeFreeze (currentBuffer st)
              write newChunk 0 v
              return ((), st { prevBuffers   = cur : prevBuffers st
                             , currentOffset = Offset 1
                             , currentBuffer = newChunk
                             })
          else do
              let (Offset ofs) = currentOffset st
              write (currentBuffer st) ofs v
              return ((), st { currentOffset = Offset (ofs + 1) })

  build sizeChunksI ab = do
      m        <- new sizeChunks
      ((), st) <- runState (runBuilder ab) (BuildingState [] m (Offset 0) sizeChunks)
      current  <- unsafeFreezeShrink (currentBuffer st) (offsetAsSize $ currentOffset st)
      return $ mconcat $ Data.List.reverse (current : prevBuffers st)
    where
      sizeChunks = Size sizeChunksI
