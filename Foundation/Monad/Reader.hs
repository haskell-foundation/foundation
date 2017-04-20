-- |
-- The Reader monad transformer.
--
-- This is useful to keep a non-modifiable value
-- in a context

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Foundation.Monad.Reader
    ( -- * MonadReader
      MonadReader(..)
    , asks

    , -- * ReaderT
      ReaderT
    , runReaderT
    ) where

import Foundation.Internal.Base (($), (.), const, id, (<$>))
import Foundation.Monad.Base

class Monad m => MonadReader r m where
    {-# MINIMAL (ask | reader), local #-}

    -- | Retrieves the monad environment.
    ask   :: m r
    ask = reader id

    -- | Executes a computation in a modified environment.
    local :: (r -> r) -- ^ The function to modify the environment.
          -> m a      -- ^ @Reader@ to run in the modified environment.
          -> m a

    -- | Retrieves a function of the current environment.
    reader :: (r -> a) -- ^ The selector function to apply to the environment.
           -> m a
    reader f = f <$> ask

-- | Retrieves a function of the current environment.
asks :: MonadReader r m
     => (r -> a) -- ^ The selector function to apply to the environment.
     -> m a
asks = reader

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
    return a = ReaderT $ const (return a)
    {-# INLINE return #-}
    ma >>= mab = ReaderT $ \r -> runReaderT ma r >>= \a -> runReaderT (mab a) r
    {-# INLINE (>>=) #-}

instance MonadTrans (ReaderT r) where
    lift f = ReaderT $ \_ -> f
    {-# INLINE lift #-}

instance MonadIO m => MonadIO (ReaderT r m) where
    liftIO f = lift (liftIO f)
    {-# INLINE liftIO #-}

instance MonadFailure m => MonadFailure (ReaderT r m) where
    type Failure (ReaderT r m) = Failure m
    mFail e = ReaderT $ \_ -> mFail e

instance MonadThrow m => MonadThrow (ReaderT r m) where
    throw e = ReaderT $ \_ -> throw e

instance MonadCatch m => MonadCatch (ReaderT r m) where
    catch (ReaderT m) c = ReaderT $ \r -> m r `catch` (\e -> runReaderT (c e) r)

instance Monad m => MonadReader r (ReaderT r m) where
    ask = ReaderT return
    local f m = ReaderT $ runReaderT m . f
