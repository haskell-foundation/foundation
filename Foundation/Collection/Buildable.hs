-- |
-- Module      : Foundation.Collection.Buildable
-- License     : BSD-style
-- Maintainer  : foundation
-- Stability   : experimental
-- Portability : portable
--
-- An interface for collections that can be built incrementally.
--
module Foundation.Collection.Buildable
    ( Buildable(..)
    , Builder(..)
    , BuildingState(..)
    ) where

import           Foundation.Array.Unboxed
import           Foundation.Array.Unboxed.Mutable
import           Foundation.Collection.Element
import           Foundation.Internal.Base
import           Foundation.Primitive.Monad
import           Foundation.Boot.Builder

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

    append :: (PrimMonad prim) => Element col -> Builder col (Mutable col) (Step col) prim ()

    build :: (PrimMonad prim)
          => Int -- ^ Size of a chunk
          -> Builder col (Mutable col) (Step col) prim ()
          -> prim col

instance PrimType ty => Buildable (UArray ty) where
  type Mutable (UArray ty) = MUArray ty
  type Step (UArray ty) = ty
  append = builderAppend
  {-# INLINE append #-}
  build = builderBuild
  {-# INLINE build #-}
