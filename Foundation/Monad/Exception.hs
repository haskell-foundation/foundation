
module Foundation.Monad.Exception
    ( MonadFailure(..)
    , MonadThrow(..)
    , MonadCatch(..)
    ) where

import           Foundation.Internal.Base
import qualified Control.Exception (throwIO, catch)

-- | Monad that can represent failure
--
-- Similar to MonadFail but with a parametrized Failure linked to the Monad
class Monad m => MonadFailure m where
    -- | The associated type with the MonadFailure, representing what
    -- failure can be encoded in this monad
    type Failure m

    -- | Raise a Failure through a monad.
    mFail :: Failure m -> m ()

-- | Monad that can throw exception
class Monad m => MonadThrow m where
    -- | Throw immediatity an exception.
    -- Only a 'MonadCatch' monad will be able to catch the exception using 'catch'
    throw :: Exception e => e -> m a

-- | Monad that can catch exception
class MonadThrow m => MonadCatch m where
    catch :: Exception e => m a -> (e -> m a) -> m a

instance MonadFailure Maybe where
    type Failure Maybe = ()
    mFail _ = Nothing
instance MonadFailure (Either a) where
    type Failure (Either a) = a
    mFail a = Left a

instance MonadThrow IO where
    throw = Control.Exception.throwIO
instance MonadCatch IO where
    catch = Control.Exception.catch
