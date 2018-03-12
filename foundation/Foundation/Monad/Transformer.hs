{-# LANGUAGE ConstraintKinds #-}
module Foundation.Monad.Transformer
    ( MonadTrans(..)
    ) where

import Basement.Compat.Base
import Basement.Compat.AMP

-- | Basic Transformer class
class MonadTrans trans where
    -- | Lift a computation from an inner monad to the current transformer monad
    lift :: AMPMonad m => m a -> trans m a
