-- |
-- Module      : Core.String.Encoding.Encoding
-- License     : BSD-style
-- Maintainer  : Foundation
-- Stability   : experimental
-- Portability : portable
--

module Core.String.Encoding.Encoding
    ( Encoding(..)
    ) where

import Core.Internal.Base
import Core.Internal.Types
import Core.Primitive.Monad
import Core.Array.Unboxed.Builder

class Encoding encoding where
    type Unit encoding
    type Error encoding
    -- | consume an `Unit encoding` and return the Unicode point and the position
    -- of the next possible `Unit encoding`
    --
    next :: encoding
              -- ^ only used for type deduction
         -> (Offset (Unit encoding) -> Unit encoding)
              -- ^ method to access a given `Unit encoding`
              -- (see `unsafeIndexer`)
         -> Offset (Unit encoding)
              -- ^ offset of the `Unit encoding` where starts the encoding of
              -- a given unicode
         -> Either (Error encoding) (Char, Offset (Unit encoding))
              -- ^ either successfully validated the `Unit encoding` and
              -- returned the next offset or fail with an `Error encoding`

    -- Write a unicode point encoded into one or multiple `Unit encoding`
    --
    -- > build 64 $ sequence_ (write UTF8) "this is a simple list of char..."
    --
    write :: (PrimMonad st, Monad st)
          => encoding
              -- ^ only used for type deduction
          -> Char
              -- ^ the unicode character to encode
          -> ArrayBuilder (Unit encoding) st ()
