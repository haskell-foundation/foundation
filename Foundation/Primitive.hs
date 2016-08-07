-- |
-- Module      : Foundation.Primitive
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- Different collections (list, vector, string, ..) unified under 1 API.
-- an API to rules them all, and in the darkness bind them.
--
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
module Foundation.Primitive
    ( PrimType(..)
    , PrimMonad(..)
    ) where

import Foundation.Primitive.Types
import Foundation.Primitive.Monad
