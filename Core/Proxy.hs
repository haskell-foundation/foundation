-- |
-- Module      : Core.Proxy
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
{-# LANGUAGE CPP #-}
module Core.Proxy
    ( Proxy(..)
    , asProxyTypeOf
    ) where

#if MIN_VERSION_base(4,7,0)
import Data.Proxy
#else
import qualified Prelude

data Proxy a = Proxy

instance Prelude.Show (Proxy a) where
    showsPrec _ _ = Prelude.showString "Proxy"

-- | 'asProxyTypeOf' is a type-restricted version of 'const'.
-- It is usually used as an infix operator, and its typing forces its first
-- argument (which is usually overloaded) to have the same type as the tag
-- of the second.
asProxyTypeOf :: a -> Proxy a -> a
asProxyTypeOf = Prelude.const
{-# INLINE asProxyTypeOf #-}

#endif
