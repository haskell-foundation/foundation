{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
module Foundation.Monad.MonadIO
    ( MonadIO(..)
    ) where

#if MIN_VERSION_base(4,9,0)
import Control.Monad.IO.Class
#else
import Basement.Compat.Base
import Basement.Compat.AMP

-- | Monads in which 'IO' computations may be embedded.
class AMPMonad m => MonadIO m where
    -- | Lift a computation from the 'IO' monad.
    liftIO :: IO a -> m a

instance MonadIO IO where
    liftIO io = io

#endif
