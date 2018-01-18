{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Foundation.Monad.Except
    ( ExceptT(..)
    ) where

import Basement.Imports
import Basement.Compat.AMP
import Foundation.Monad.Base
import Foundation.Monad.Reader

newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

instance Functor m => Functor (ExceptT e m) where
    fmap f = ExceptT . fmap (fmap f) . runExceptT

instance AMPMonad m => Applicative (ExceptT e m) where
    pure a = ExceptT $ pure (Right a)
    ExceptT f <*> ExceptT v = ExceptT $ do
        mf <- f
        case mf of
            Left e -> pure (Left e)
            Right k -> do
                mv <- v
                case mv of
                    Left e -> pure (Left e)
                    Right x -> pure (Right (k x))

instance AMPMonad m => MonadFailure (ExceptT e m) where
    type Failure (ExceptT e m) = e
    mFail = ExceptT . pure . Left

instance AMPMonad m => Monad (ExceptT e m) where
    return a = ExceptT $ return (Right a)
    m >>= k = ExceptT $ do
        a <- runExceptT m
        case a of
            Left e -> return (Left e)
            Right x -> runExceptT (k x)
    fail = ExceptT . fail

instance MonadReader m => MonadReader (ExceptT e m) where
    type ReaderContext (ExceptT e m) = ReaderContext m
    ask = ExceptT (Right <$> ask)

instance MonadTrans (ExceptT e) where
    lift f = ExceptT (Right <$> f)

instance MonadIO m => MonadIO (ExceptT e m) where
    liftIO f = ExceptT (Right <$> liftIO f)
