{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Foundation.Monad.State
    ( -- * MonadState
      MonadState(..)
    , modify
    , modify'
    , gets

    , -- * StateT
      StateT
    , runStateT
    ) where

import Foundation.Internal.Base (($), (.), seq)
import Foundation.Monad.Base

class Monad m => MonadState s m | m -> s where
    {-# MINIMAL state | get, put #-}
    -- | Return the state from the internals of the monad.
    get :: m s
    get = state (\s -> (s, s))

    -- | Replace the state inside the monad.
    put :: s -> m ()
    put s = state (\_ -> ((), s))

    -- | Embed a simple state action into the monad.
    state :: (s -> (a, s)) -> m a
    state f = do
        s <- get
        let ~(a, s') = f s
        put s'
        return a

-- | Monadic state transformer.
--
--      Maps an old state to a new state inside a state monad.
--      The old state is thrown away.
--
-- >      Main> :t modify ((+1) :: Int -> Int)
-- >      modify (...) :: (MonadState Int a) => a ()
--
--    This says that @modify (+1)@ acts over any
--    Monad that is a member of the @MonadState@ class,
--    with an @Int@ state.
modify :: MonadState s m => (s -> s) -> m ()
modify f = state (\s -> ((), f s))

-- | A variant of 'modify' in which the computation is strict in the
-- new state.
modify' :: MonadState s m => (s -> s) -> m ()
modify' f = state (\s -> let s' = f s in s' `seq` ((), s'))

-- | Gets specific component of the state, using a projection function
-- supplied.
gets :: MonadState s m => (s -> a) -> m a
gets f = do
    s <- get
    return (f s)

-- | State Transformer
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
    fmap f m = StateT $ \s1 -> (\(a, s2) -> (f a, s2)) `fmap` runStateT m s1
    {-# INLINE fmap #-}

instance (Applicative m, Monad m) => Applicative (StateT s m) where
    pure a     = StateT $ \s -> (,s) `fmap` pure a
    {-# INLINE pure #-}
    fab <*> fa = StateT $ \s1 -> do
        (ab,s2) <- runStateT fab s1
        (a, s3) <- runStateT fa s2
        return (ab a, s3)
    {-# INLINE (<*>) #-}

instance (Functor m, Monad m) => Monad (StateT s m) where
    return a = StateT $ \s -> (,s) `fmap` return a
    {-# INLINE return #-}
    ma >>= mab = StateT $ \s1 -> runStateT ma s1 >>= \(a, s2) -> runStateT (mab a) s2
    {-# INLINE (>>=) #-}

instance MonadTrans (StateT s) where
    lift f = StateT $ \s -> f >>= return . (,s)
    {-# INLINE lift #-}

instance (Functor m, MonadIO m) => MonadIO (StateT s m) where
    liftIO f = lift (liftIO f)
    {-# INLINE liftIO #-}

instance (Functor m, MonadFailure m) => MonadFailure (StateT s m) where
    type Failure (StateT s m) = Failure m
    mFail e = StateT $ \s -> ((,s) `fmap` mFail e)

instance (Functor m, MonadThrow m) => MonadThrow (StateT s m) where
    throw e = StateT $ \_ -> throw e

instance (Functor m, MonadCatch m) => MonadCatch (StateT s m) where
    catch (StateT m) c = StateT $ \s1 -> m s1 `catch` (\e -> runStateT (c e) s1)

instance Monad m => MonadState s (StateT s m) where
  get = state $ \ s -> (s, s)
  state f = StateT (return . f)
  put s = state $ \ _ -> ((), s)
