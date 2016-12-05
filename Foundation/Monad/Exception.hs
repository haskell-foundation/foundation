
module Foundation.Monad.Exception
    ( MonadFailure(..)
    , MonadThrow(..)
    , MonadCatch(..)
    ) where

import           Foundation.Internal.Base
import qualified Control.Exception (throwIO, catch)

class Monad m => MonadFailure m where
    type Failure m
    failure :: Failure m -> m ()

class Monad m => MonadThrow m where
    throw :: Exception e => e -> m ()

-- | Monad that can catch
class MonadThrow m => MonadCatch m where
    catch :: Exception e => m a -> (e -> m a) -> m a

instance MonadFailure Maybe where
    type Failure Maybe = ()
    failure _ = Nothing
instance MonadFailure (Either a) where
    type Failure (Either a) = a
    failure a = Left a

instance MonadThrow IO where
    throw = Control.Exception.throwIO
instance MonadCatch IO where
    catch = Control.Exception.catch
