{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
module Core.Array.Unboxed.Builder
    where

import Core.Internal.Base
import Core.Internal.Types
import Core.Internal.MonadTrans
import Core.Primitive.Monad
import Core.Primitive.Types
import Core.Array.Unboxed.Mutable
import Core.Array.Unboxed
import Core.Number
import GHC.ST

import Control.Monad

-- | A Array being built chunks by chunks
--
-- The previous buffers are in reverse order, and
-- this contains the current buffer and the state of
-- progress packing ty inside.
data ArrayBuildingState ty st = ArrayBuildingState
    { prevBuffers   :: [UArray ty]
    , currentBuffer :: MUArray ty st
    , currentOffset :: !(Offset ty)
    }

newtype ArrayBuilder ty st a = ArrayBuilder { runArrayBuilder :: State (ArrayBuildingState ty (PrimState st)) st a }
    deriving (Functor,Applicative,Monad)

appendTy :: (PrimMonad st, PrimType ty, Monad st) => ty -> ArrayBuilder ty st ()
appendTy v = ArrayBuilder $ State $ \st -> do
    let (Offset ofs) = currentOffset st
    write (currentBuffer st) ofs v
    return ((), st { currentOffset = currentOffset st + Offset 1 })

build :: (PrimMonad prim, PrimType ty)
      => Int                     -- ^ size of chunks (elements)
      -> ArrayBuilder ty prim () -- ^ ..
      -> prim (UArray ty)
build (Size -> sizeChunks) origab = call origab (new sizeChunks)
  where
    call :: (PrimType ty, PrimMonad prim)
         => ArrayBuilder ty prim () -> prim (MUArray ty (PrimState prim)) -> prim (UArray ty)
    call ab allocNew = do
        m        <- allocNew
        ((), st) <- runState (runArrayBuilder ab) (ArrayBuildingState [] m (Offset 0))
        -- FIXME
        unsafeFreezeShrink (currentBuffer st) (offsetAsSize $ currentOffset st)
