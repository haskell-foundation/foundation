-- |
-- Module      : Core.Primitive.Monad
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Core.Primitive.Monad
    ( PrimMonad(..)
    , runPrimMonad_
    ) where

import qualified Prelude
import           GHC.ST
import           GHC.IO
import           GHC.Prim
import           Core.Internal.Base (Exception, (.))

-- | Primitive monad that can handle mutation.
--
-- For example: IO and ST.
class Prelude.Monad m => PrimMonad m where
    -- | type of state token associated with the PrimMonad m
    type PrimState m
    -- | Unwrap the State# token to pass to a function a primitive function that returns an unboxed state and a value.
    primitive :: (State# (PrimState m) -> (# State# (PrimState m), a #)) -> m a
    -- | Throw Exception in the primitive monad
    primThrow :: Exception e => e -> m a
    -- | Run a Prim monad from a dedicated state#
    runPrimMonad  :: m a -> State# (PrimState m) -> (# State# (PrimState m), a #)

-- | just like `internal` but throw away the result and return just the new State#
runPrimMonad_ :: PrimMonad m => m () -> State# (PrimState m) -> State# (PrimState m)
runPrimMonad_ p st =
    case runPrimMonad p st of
        (# st', () #) -> st'

instance PrimMonad IO where
    type PrimState IO = RealWorld
    primitive = IO
    primThrow = throwIO
    runPrimMonad (IO p) = p

instance PrimMonad (ST s) where
    type PrimState (ST s) = s
    primitive = ST
    primThrow = unsafeIOToST . throwIO
    runPrimMonad (ST p) = p
