module Foundation.Exception
    ( finally
    , try
    , SomeException
    ) where

import Control.Exception (Exception)
import Foundation.Monad.Exception

finally :: MonadBracket m => m a -> m b -> m a
finally f g = generalBracket (pure ()) (\() a -> g >> pure a) (\() _ -> g) (const f)

try :: (MonadCatch, Exception e) => m a -> m (Either e a)
try a = catch (a >>= \ v -> return (Right v)) (\e -> return (Left e))
