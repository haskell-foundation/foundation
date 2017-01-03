module Foundation.Monad.Reader
    ( ReaderT
    ) where

import Foundation.Internal.Base (($), (.), const)
import Foundation.Monad.Base

-- | Reader Transformer
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
    fmap f m = ReaderT $ fmap f . runReaderT m
    {-# INLINE fmap #-}

instance Applicative m => Applicative (ReaderT r m) where
    pure a     = ReaderT $ const (pure a)
    {-# INLINE pure #-}
    fab <*> fa = ReaderT $ \r -> runReaderT fab r <*> runReaderT fa r
    {-# INLINE (<*>) #-}

instance Monad m => Monad (ReaderT r m) where
    return = pure
    {-# INLINE return #-}
    ma >>= mab = ReaderT $ \r -> runReaderT ma r >>= \a -> runReaderT (mab a) r
    {-# INLINE (>>=) #-}

instance MonadTrans (ReaderT r) where
    lift f = ReaderT $ \_ -> f
    {-# INLINE lift #-}

instance MonadIO m => MonadIO (ReaderT r m) where
    liftIO f = lift (liftIO f)
    {-# INLINE liftIO #-}

--instance MonadFailure m => MonadTrans (ReaderT r) where
--    type MonadFailure (ReaderT r) = MonadFailure

instance MonadThrow m => MonadThrow (ReaderT r m) where
    throw e = ReaderT $ \_ -> throw e 

instance MonadCatch m => MonadCatch (ReaderT r m) where
    catch (ReaderT m) c = ReaderT $ \r -> m r `catch` (\e -> runReaderT (c e) r)
