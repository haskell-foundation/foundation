-- |
-- Module      : Basement.Compat.Bifunctor
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- Formally, the class 'Bifunctor' represents a bifunctor
-- from @Hask@ -> @Hask@.
--
-- Intuitively it is a bifunctor where both the first and second
-- arguments are covariant.
--
-- You can define a 'Bifunctor' by either defining 'bimap' or by
-- defining both 'first' and 'second'.
--
{-# LANGUAGE CPP #-}
module Basement.Compat.Bifunctor
  ( Bifunctor(..)
  ) where

#if MIN_VERSION_base(4,8,0)

import Data.Bifunctor (Bifunctor(..))

#else

import           Control.Applicative ( Const(..) )
import           GHC.Generics ( K1(..) )
import qualified Prelude as P

class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}

    -- | Map over both arguments at the same time.
    --
    -- @'bimap' f g ≡ 'first' f '.' 'second' g@
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f P.. second g

    -- | Map covariantly over the first argument.
    --
    -- @'first' f ≡ 'bimap' f 'id'@
    first :: (a -> b) -> p a c -> p b c
    first f = bimap f P.id

    -- | Map covariantly over the second argument.
    --
    -- @'second' ≡ 'bimap' 'id'@
    second :: (b -> c) -> p a b -> p a c
    second = bimap P.id


instance Bifunctor (,) where
    bimap f g ~(a, b) = (f a, g b)

instance Bifunctor ((,,) x1) where
    bimap f g ~(x1, a, b) = (x1, f a, g b)

instance Bifunctor ((,,,) x1 x2) where
    bimap f g ~(x1, x2, a, b) = (x1, x2, f a, g b)

instance Bifunctor ((,,,,) x1 x2 x3) where
    bimap f g ~(x1, x2, x3, a, b) = (x1, x2, x3, f a, g b)

instance Bifunctor ((,,,,,) x1 x2 x3 x4) where
    bimap f g ~(x1, x2, x3, x4, a, b) = (x1, x2, x3, x4, f a, g b)

instance Bifunctor ((,,,,,,) x1 x2 x3 x4 x5) where
    bimap f g ~(x1, x2, x3, x4, x5, a, b) = (x1, x2, x3, x4, x5, f a, g b)


instance Bifunctor P.Either where
    bimap f _ (P.Left a) = P.Left (f a)
    bimap _ g (P.Right b) = P.Right (g b)

instance Bifunctor Const where
    bimap f _ (Const a) = Const (f a)

instance Bifunctor (K1 i) where
    bimap f _ (K1 c) = K1 (f c)

#endif
