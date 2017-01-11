{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Foundation.Monad
    ( MonadIO(..)
    , MonadFailure(..)
    , MonadThrow(..)
    , MonadCatch(..)
    , MonadTrans(..)
    , Identity(..)
    ) where

import Foundation.Monad.MonadIO
import Foundation.Monad.Exception
import Foundation.Monad.Transformer

#if MIN_VERSION_base(4,8,0)
import Data.Functor.Identity

#else

import Control.Monad.Fix
import Control.Monad.Zip
import Data.Coerce
import Foundation.Internal.Base
import GHC.Generics (Generic1)

-- | Identity functor and monad. (a non-strict monad)
--
-- @since 4.8.0.0
newtype Identity a = Identity { runIdentity :: a }
    deriving (Eq, Ord, Data, Generic, Generic1, Typeable)

instance Functor Identity where
    fmap     = coerce

instance Applicative Identity where
    pure     = Identity
    (<*>)    = coerce

instance Monad Identity where
    return   = Identity
    m >>= k  = k (runIdentity m)

instance MonadFix Identity where
    mfix f   = Identity (fix (runIdentity . f))

instance MonadZip Identity where
    mzipWith = coerce
    munzip   = coerce

#endif
