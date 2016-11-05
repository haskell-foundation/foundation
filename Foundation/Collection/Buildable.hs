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

import           Foundation.Array.Unboxed
import           Foundation.Array.Unboxed.Mutable
import           Foundation.Collection.Element
import           Foundation.Internal.Base
import           Foundation.Internal.MonadTrans
import           Foundation.Internal.Types
import           Foundation.Numerical
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
    { runBuilder :: State (Offset (Step col), BuildingState col (PrimState st)) st a }
    deriving (Functor, Applicative, Monad)

-- | The in-progress state of a building operation.
--
-- The previous buffers are in reverse order, and
-- this contains the current buffer and the state of
-- progress packing the elements inside.
data BuildingState col st = BuildingState
    { prevChunks     :: [col]
    , prevChunksSize :: !(Size (Step col))
    , curChunk       :: Mutable col st
    , chunkSize      :: !(Size (Step col))
    }

instance PrimType ty => Buildable (UArray ty) where
  type Mutable (UArray ty) = MUArray ty
  type Step (UArray ty) = ty

  append v = Builder $ State $ \(i, st) ->
      if offsetAsSize i == chunkSize st
          then do
              cur      <- unsafeFreeze (curChunk st)
              newChunk <- new (chunkSize st)
              unsafeWrite newChunk 0 v
              return ((), (Offset 1, st { prevChunks     = cur : prevChunks st
                                        , prevChunksSize = chunkSize st + prevChunksSize st
                                        , curChunk       = newChunk
                                        }))
          else do
              let Offset i' = i
              unsafeWrite (curChunk st) i' v
              return ((), (i + Offset 1, st))
  {-# INLINE append #-}

  build sizeChunksI ab
    | sizeChunksI <= 0 = build 64 ab
    | otherwise        = do
        first         <- new sizeChunks
        ((), (i, st)) <- runState (runBuilder ab) (Offset 0, BuildingState [] (Size 0) first sizeChunks)
        cur           <- unsafeFreezeShrink (curChunk st) (offsetAsSize i)
        -- Build final array
        let totalSize = prevChunksSize st + offsetAsSize i
        new totalSize >>= fillFromEnd totalSize (cur : prevChunks st) >>= unsafeFreeze
    where
      sizeChunks = Size sizeChunksI

      fillFromEnd _   []     mua = return mua
      fillFromEnd !end (x:xs) mua = do
          let sz = lengthSize x
          unsafeCopyAtRO mua (sizeAsOffset (end - sz)) x (Offset 0) sz
          fillFromEnd (end - sz) xs mua
  {-# INLINE build #-}
