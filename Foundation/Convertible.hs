-- |
-- Module      : Foundation.Convertible
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Foundation.Convertible
    ( Convertible(..)
    ) where

import Foundation.Internal.Base
import Data.Proxy

-- | Class of things that can be converted from a to b
class Convertible a b where
    type Convert a b
    convert :: Proxy b -> a -> Convert a b

instance Convertible a a where
    type Convert a a = a
    convert _ = id
