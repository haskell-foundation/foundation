-- Module      : Foundation.Conduit.Internal
-- License     : BSD-style
-- Maintainer  : Foundation
-- Stability   : experimental
-- Portability : portable
--
-- Taken from the conduit package almost verbatim, and
-- Copyright (c) 2012 Michael Snoyman
--
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module Foundation.Conduit.Internal
    ( Pipe(..)
    , ConduitM(..)
    ) where

import Foundation.Internal.Base
import Control.Monad ((>=>))

-- | A pipe producing and consuming values
--
-- A basic intuition is that every @Pipe@ produces a stream of /output/ values
-- and eventually indicates that this stream is terminated by sending a
-- /result/. On the receiving end of a @Pipe@, these become the /input/ and /upstream/
-- parameters.
data Pipe leftOver input output upstream monad result =
      -- | Provide new output to be sent downstream. This constructor has three
      -- fields: the next @Pipe@ to be used, a finalization function, and the
      -- output value.
      HaveOutput (Pipe leftOver input output upstream monad result) (monad ()) output
      -- | Request more input from upstream. The first field takes a new input
      -- value and provides a new @Pipe@. The second takes an upstream result
      -- value, which indicates that upstream is producing no more results.
    | NeedInput (input -> Pipe leftOver input output upstream monad result)
                (upstream -> Pipe leftOver input output upstream monad result)
      -- | Processing with this @Pipe@ is complete, providing the final result.
    | Done result
      -- | Require running of a monadic action to get the next @Pipe@.
    | PipeM (monad (Pipe leftOver input output upstream monad result))
      -- | Return leftover input, which should be provided to future operations.
    | Leftover (Pipe leftOver input output upstream monad result) leftOver

instance Applicative m => Functor (Pipe l i o u m) where
    fmap = (<$>)
    {-# INLINE fmap #-}

instance Applicative m => Applicative (Pipe l i o u m) where
    pure = Done
    {-# INLINE pure #-}

    HaveOutput p c o <*> fa = HaveOutput (p <*> fa) c o
    NeedInput p c    <*> fa = NeedInput (\i -> p i <*> fa) (\o -> c o <*> fa)
    Done r           <*> fa = r <$> fa
    PipeM mp         <*> fa = PipeM ((<*> fa) <$> mp)
    Leftover p i     <*> fa = Leftover (p <*> fa) i
    {-# INLINE (<*>) #-}

instance Monad m => Monad (Pipe l i o u m) where
    return = pure
    {-# INLINE return #-}

    HaveOutput p c o >>= fp = HaveOutput (p >>= fp)            c          o
    NeedInput p c    >>= fp = NeedInput  (p >=> fp)            (c >=> fp)
    Done x           >>= fp = fp x
    PipeM mp         >>= fp = PipeM      ((>>= fp) <$> mp)
    Leftover p i     >>= fp = Leftover   (p >>= fp)            i

newtype ConduitM input output monad result = ConduitM
    { unConduitM :: forall a .
                 (result -> Pipe input input output () monad a)
                 -> Pipe input input output () monad a
    }

instance Functor (ConduitM i o m) where
    fmap f (ConduitM c) = ConduitM $ \resPipe -> c (resPipe . f)

instance Applicative (ConduitM i o m) where
    pure x = ConduitM ($ x)
    {-# INLINE pure #-}

    fab <*> fa = fab >>= \ab -> fa >>= \a -> pure (ab a)
    {-# INLINE (<*>) #-}

instance Monad (ConduitM i o m) where
    return = pure
    ConduitM f >>= g = ConduitM $ \h -> f $ \a -> unConduitM (g a) h
