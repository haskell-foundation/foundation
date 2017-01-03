module Foundation.Monad.Transformer
    ( MonadTrans(..)
    ) where

import Foundation.Internal.Base

-- | Basic Transformer class
class MonadTrans trans where
    -- | Lift a computation from an inner monad to the current transformer monad
    lift :: Monad m => m a -> trans m a
