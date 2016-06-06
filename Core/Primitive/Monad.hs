-- |
-- Module      : Core.Primitive.Monad
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- Allow to run operation in ST and IO, without having to
-- distinguinsh between the two. Most operations exposes
-- the bare nuts and bolts of how IO and ST actually
-- works, and relatively easy to shoot yourself in the foot
--
-- this is highly similar to the Control.Monad.Primitive
-- in the primitive package
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ExistentialQuantification #-}
module Core.Primitive.Monad
    ( PrimMonad(..)
    , unPrimMonad_
    , unsafePrimCast
    , unsafePrimToST
    , unsafePrimToIO
    , unsafePrimFromIO
    , primTouch
    ) where

import qualified Prelude
import           GHC.ST
import           GHC.IO
import           GHC.Prim
import           Core.Internal.Base (Exception, (.), ($))

-- | Primitive monad that can handle mutation.
--
-- For example: IO and ST.
class (Prelude.Functor m, Prelude.Monad m) => PrimMonad m where
    -- | type of state token associated with the PrimMonad m
    type PrimState m
    -- | Unwrap the State# token to pass to a function a primitive function that returns an unboxed state and a value.
    primitive :: (State# (PrimState m) -> (# State# (PrimState m), a #)) -> m a
    -- | Throw Exception in the primitive monad
    primThrow :: Exception e => e -> m a
    -- | Run a Prim monad from a dedicated state#
    unPrimMonad  :: m a -> State# (PrimState m) -> (# State# (PrimState m), a #)

-- | just like `unwrapPrimMonad` but throw away the result and return just the new State#
unPrimMonad_ :: PrimMonad m => m () -> State# (PrimState m) -> State# (PrimState m)
unPrimMonad_ p st =
    case unPrimMonad p st of
        (# st', () #) -> st'

instance PrimMonad IO where
    type PrimState IO = RealWorld
    primitive = IO
    primThrow = throwIO
    unPrimMonad (IO p) = p

instance PrimMonad (ST s) where
    type PrimState (ST s) = s
    primitive = ST
    primThrow = unsafeIOToST . throwIO
    unPrimMonad (ST p) = p

-- | Convert a prim monad to another prim monad.
--
-- The net effect is that it coerce the state repr to another,
-- so the runtime representation should be the same, otherwise
-- hilary ensues.
unsafePrimCast :: (PrimMonad m1, PrimMonad m2) => m1 a -> m2 a
unsafePrimCast m = primitive (unsafeCoerce# (unPrimMonad m))

-- | Convert any prim monad to an ST monad
unsafePrimToST :: PrimMonad prim => prim a -> ST s a
unsafePrimToST = unsafePrimCast

unsafePrimToIO :: PrimMonad prim => prim a -> IO a
unsafePrimToIO = unsafePrimCast

unsafePrimFromIO :: PrimMonad prim => IO a -> prim a
unsafePrimFromIO = unsafePrimCast


-- | Touch primitive lifted to any prim monad
primTouch :: PrimMonad m => a -> m ()
primTouch x = unsafePrimFromIO $ primitive $ \s -> case touch# x s of { s2 -> (# s2, () #) }
