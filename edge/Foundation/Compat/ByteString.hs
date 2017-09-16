-- |
-- Module      : Foundation.Compat.ByteString
-- License     : BSD-style
-- Maintainer  : Foundation
-- Stability   : experimental
-- Portability : portable
--
-- Module to convert bytestring's ByteString type
{-# LANGUAGE ViewPatterns #-}
module Foundation.Compat.ByteString
    ( fromByteString
    , toByteString
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Internal (toForeignPtr, unsafeCreate, memcpy)
import Foundation
import Foundation.Array
import Foundation.Array.Internal (withPtr, fromForeignPtr)

-- | Convert a ByteString to a UArray Word8,
-- without re-allocating or copying anything
fromByteString :: ByteString -> UArray Word8
fromByteString = fromForeignPtr . toForeignPtr

-- | Convert a UArray Word8 to ByteString
--
-- all the bytes are copied to a brand new memory chunk
toByteString :: UArray Word8 -> ByteString
toByteString v = unsafeCreate len $ \dst -> withPtr v $ \src -> memcpy dst src len
  where
    !(CountOf len) = length v
