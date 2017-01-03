{-# LANGUAGE TupleSections #-}
module Foundation.Monad.State
    ( StateT
    ) where

import Foundation.Internal.Base (($))
import Foundation.Monad.Base

-- | State Transformer
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
    fmap f m = StateT $ \s1 -> (\(a, s2) -> (f a, s2)) `fmap` runStateT m s1
    {-# INLINE fmap #-}

instance Monad m => Applicative (StateT s m) where
    pure a     = StateT $ \s -> (,s) `fmap` pure a
    {-# INLINE pure #-}
    fab <*> fa = StateT $ \s1 -> do
        (ab,s2) <- runStateT fab s1
        (a, s3) <- runStateT fa s2
        return (ab a, s3)
    {-# INLINE (<*>) #-}

instance Monad m => Monad (StateT s m) where
    return = pure
    {-# INLINE return #-}
    ma >>= mab = StateT $ \s1 -> runStateT ma s1 >>= \(a, s2) -> runStateT (mab a) s2
    {-# INLINE (>>=) #-}

instance MonadTrans (StateT s) where
    lift f = StateT $ \s -> (,s) `fmap` f
    {-# INLINE lift #-}

instance MonadIO m => MonadIO (StateT s m) where
    liftIO f = lift (liftIO f)
    {-# INLINE liftIO #-}

instance MonadFailure m => MonadFailure (StateT s m) where
    type Failure (StateT s m) = Failure m
    mFail e = StateT $ \s -> ((,s) `fmap` mFail e)

instance MonadThrow m => MonadThrow (StateT s m) where
    throw e = StateT $ \_ -> throw e 

instance MonadCatch m => MonadCatch (StateT s m) where
    catch (StateT m) c = StateT $ \s1 -> m s1 `catch` (\e -> runStateT (c e) s1)
