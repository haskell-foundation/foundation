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
    -- | the unit element use for the encoding.
    -- i.e. Word8 for ASCII7 or UTF8, Word16 for UTF16...
    --
    type Unit encoding

    -- | define the type of error handling you want to use for the
    -- next function.
    --
    -- as example, one can use:
    --
    -- > type Error ASCII7 a = Maybe a
    --
    -- or can use:
    --
    -- > type Error UTF8 a = Either UTF8_Invalid a
    --
    type Error encoding a

    -- | consume an `Unit encoding` and return the Unicode point and the position
    -- of the next possible `Unit encoding`
    --
    encodingNext :: encoding
                      -- ^ only used for type deduction
                -> (Offset (Unit encoding) -> Unit encoding)
                      -- ^ method to access a given `Unit encoding`
                      -- (see `unsafeIndexer`)
                -> Offset (Unit encoding)
                      -- ^ offset of the `Unit encoding` where starts the
                      -- encoding of a given unicode
                -> Error encoding (Char, Offset (Unit encoding))
                      -- ^ either successfully validated the `Unit encoding`
                      -- and returned the next offset or fail with an
                      -- `Error encoding`

    -- Write a unicode point encoded into one or multiple `Unit encoding`
    --
    -- > build 64 $ sequence_ (write UTF8) "this is a simple list of char..."
    --
    encodingWrite :: (PrimMonad st, Monad st)
                  => encoding
                      -- ^ only used for type deduction
                  -> Char
                      -- ^ the unicode character to encode
                  -> ArrayBuilder (Unit encoding) st ()
