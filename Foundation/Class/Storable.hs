-- |
-- Module      : Foundation.Class.Storable
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
-- Stability   : experimental
-- Portability : portable
--
-- <https://github.com/haskell-foundation/issues/111>
--
--

{-# LANGUAGE FlexibleInstances #-}

module Foundation.Class.Storable
    ( Storable(..)
    , StorableFixed(..)

    , Ptr, plusPtr, castPtr
    , peekOff, pokeOff
    ) where

import GHC.Types (Double, Float)

import Foreign.Ptr (castPtr)
import qualified Foreign.Ptr
import qualified Foreign.Storable (peek, poke, sizeOf, alignment)

import Foundation.Internal.Base
import Foundation.Internal.Types
import Foundation.Internal.Proxy
import Foundation.Primitive.Types
import Foundation.Primitive.Endianness
import Foundation.Numerical

toProxy :: proxy ty -> Proxy ty
toProxy _ = Proxy

-- | Storable type of self determined size.
--
class Storable a where
    peek :: Ptr a -> IO a
    poke :: Ptr a -> a -> IO ()

-- | Extending the Storable type class to the types that can be sequenced
-- in a structure.
--
class Storable a => StorableFixed a where
    size :: proxy a -> Size Word8
    alignment :: proxy a -> Size Word8

plusPtr :: StorableFixed a => Ptr a -> Size a -> Ptr a
plusPtr ptr (Size num) = ptr `Foreign.Ptr.plusPtr` (num * (size ptr `align` alignment ptr))
  where
    align (Size sz) (Size a) = sz + (sz `mod` a)

-- | like `peek` but at a given offset.
peekOff :: StorableFixed a => Ptr a -> Offset a -> IO a
peekOff ptr off = peek (ptr `plusPtr` offsetAsSize off)

-- | like `poke` but at a given offset.
pokeOff :: StorableFixed a => Ptr a -> Offset a -> a -> IO ()
pokeOff ptr off = poke (ptr `plusPtr` offsetAsSize off)

instance Storable Char where
    peek (Ptr addr) = primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0)
instance Storable Double where
    peek (Ptr addr) = primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0)
instance Storable Float where
    peek (Ptr addr) = primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0)
instance Storable Int8 where
    peek (Ptr addr) = primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0)
instance Storable Int16 where
    peek (Ptr addr) = primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0)
instance Storable Int32 where
    peek (Ptr addr) = primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0)
instance Storable Int64 where
    peek (Ptr addr) = primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0)
instance Storable Word8 where
    peek (Ptr addr) = primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0)
instance Storable Word16 where
    peek (Ptr addr) = primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0)
instance Storable (BE Word16) where
    peek (Ptr addr) = BE <$> primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0) . unBE
instance Storable (LE Word16) where
    peek (Ptr addr) = LE <$> primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0) . unLE
instance Storable Word32 where
    peek (Ptr addr) = primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0)
instance Storable (BE Word32) where
    peek (Ptr addr) = BE <$> primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0) . unBE
instance Storable (LE Word32) where
    peek (Ptr addr) = LE <$> primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0) . unLE
instance Storable Word64 where
    peek (Ptr addr) = primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0)
instance Storable (BE Word64) where
    peek (Ptr addr) = BE <$> primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0) . unBE
instance Storable (LE Word64) where
    peek (Ptr addr) = LE <$> primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0) . unLE
instance Storable (Ptr a) where
    peek = Foreign.Storable.peek
    poke = Foreign.Storable.poke

instance StorableFixed Char where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed Double where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed Float where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed Int8 where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed Int16 where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed Int32 where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed Int64 where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed Word8 where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed Word16 where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed (BE Word16) where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed (LE Word16) where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed Word32 where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed (BE Word32) where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed (LE Word32) where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed Word64 where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed (BE Word64) where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed (LE Word64) where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed (Ptr a) where
    size      = Size . Foreign.Storable.sizeOf    . toUndefined
    alignment = Size . Foreign.Storable.alignment . toUndefined

toUndefined :: proxy a -> a
toUndefined _ = undefined
