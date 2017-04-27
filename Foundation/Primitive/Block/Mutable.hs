-- |
-- Module      : Foundation.Primitive.Block.Mutable
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
--
-- A block of memory that contains elements of a type,
-- very similar to an unboxed array but with the key difference:
--
-- * It doesn't have slicing capability (no cheap take or drop)
-- * It consume less memory: 1 Offset, 1 Size, 1 Pinning status trimmed
-- * It's unpackable in any constructor
-- * It uses unpinned memory by default
--
-- It should be rarely needed in high level API, but
-- in lowlevel API or some data structure containing lots
-- of unboxed array that will benefit from optimisation.
--
-- Because it's unpinned, the blocks are compactable / movable,
-- at the expense of making them less friendly to C layer / address.
--
-- Note that sadly the bytearray primitive type automatically create
-- a pinned bytearray if the size is bigger than a certain threshold
--
-- GHC Documentation associated:
--
-- includes/rts/storage/Block.h
--   * LARGE_OBJECT_THRESHOLD ((uint32_t)(BLOCK_SIZE * 8 / 10))
--   * BLOCK_SIZE   (1<<BLOCK_SHIFT)
--
-- includes/rts/Constant.h
--   * BLOCK_SHIFT  12
--
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}
module Foundation.Primitive.Block.Mutable
    ( Block(..)
    , MutableBlock(..)
    , new
    , isPinned
    , iterSet
    , unsafeNew
    , unsafeWrite
    , unsafeRead
    , unsafeFreeze
    , unsafeThaw
    , unsafeCopyElements
    , unsafeCopyElementsRO
    , unsafeCopyBytes
    , unsafeCopyBytesRO
    ) where

import           GHC.Prim
import           GHC.Types
import           GHC.Ptr
import           Foundation.Internal.Base
import           Foundation.Internal.Proxy
import           Foundation.Primitive.Types.OffsetSize
import           Foundation.Primitive.Monad
import           Foundation.Numerical
import           Foundation.Primitive.Types

-- | A block of memory containing unpacked bytes representing values of type 'ty'
data Block ty = Block ByteArray#

-- | A Mutable block of memory containing unpacked bytes representing values of type 'ty'
data MutableBlock ty st = MutableBlock (MutableByteArray# st)

-- | Return the length of a Mutable Block
--
-- note: we don't allow resizing yet, so this can remain a pure function
mutableLengthSize :: forall ty st . PrimType ty => MutableBlock ty st -> Size ty
mutableLengthSize (MutableBlock mba) =
    let !(Size (I# szBits)) = primSizeInBytes (Proxy :: Proxy ty)
        !elems              = quotInt# (sizeofMutableByteArray# mba) szBits
     in Size (I# elems)
{-# INLINE[1] mutableLengthSize #-}

-- | Return if a Mutable Block is pinned or not
isPinned :: MutableBlock ty st -> Bool
isPinned (MutableBlock mba) =
    -- TODO use the exact value where the array become pinned (LARGE_OBJECT_THRESHOLD)
    -- in 8.2, there's a primitive to know if an array in pinned
    I# (sizeofMutableByteArray# mba) > 3000

-- | Freeze a mutable block into a block.
--
-- If the mutable block is still use after freeze,
-- then the modification will be reflected in an unexpected
-- way in the Block.
unsafeFreeze :: PrimMonad prim => MutableBlock ty (PrimState prim) -> prim (Block ty)
unsafeFreeze (MutableBlock mba) = primitive $ \s1 ->
    case unsafeFreezeByteArray# mba s1 of
        (# s2, ba #) -> (# s2, Block ba #)
{-# INLINE unsafeFreeze #-}

-- | Thaw an immutable block.
--
-- If the immutable block is modified, then the original immutable block will
-- be modified too, but lead to unexpected results when querying
unsafeThaw :: (PrimType ty, PrimMonad prim) => Block ty -> prim (MutableBlock ty (PrimState prim))
unsafeThaw (Block ba) = primitive $ \st -> (# st, MutableBlock (unsafeCoerce# ba) #)

-- | Create a new mutable block of a specific size in bytes.
--
-- Note that no checks are made to see if the size in bytes is compatible with the size
-- of the underlaying element 'ty' in the block.
--
-- use 'new' if unsure
unsafeNew :: PrimMonad prim => Size Word8 -> prim (MutableBlock ty (PrimState prim))
unsafeNew (Size (I# bytes)) =
    primitive $ \s1 -> case newByteArray# bytes s1 of { (# s2, mba #) -> (# s2, MutableBlock mba #) }


-- | Create a new mutable block of a specific N size of 'ty' elements
new :: forall prim ty . (PrimMonad prim, PrimType ty) => Size ty -> prim (MutableBlock ty (PrimState prim))
new n = unsafeNew (sizeOfE (primSizeInBytes (Proxy :: Proxy ty)) n)

-- | Set all mutable block element to a value
iterSet :: (PrimType ty, PrimMonad prim)
        => (Offset ty -> ty)
        -> MutableBlock ty (PrimState prim)
        -> prim ()
iterSet f ma = loop 0
  where
    !sz = mutableLengthSize ma
    loop i
        | i .==# sz = pure ()
        | otherwise = unsafeWrite ma i (f i) >> loop (i+1)
    {-# INLINE loop #-}

-- | read from a cell in a mutable block without bounds checking.
--
-- Reading from invalid memory can return unpredictable and invalid values.
-- use 'read' if unsure.
unsafeRead :: (PrimMonad prim, PrimType ty) => MutableBlock ty (PrimState prim) -> Offset ty -> prim ty
unsafeRead (MutableBlock mba) i = primMbaRead mba i
{-# INLINE unsafeRead #-}

-- | write to a cell in a mutable block without bounds checking.
--
-- Writing with invalid bounds will corrupt memory and your program will
-- become unreliable. use 'write' if unsure.
unsafeWrite :: (PrimMonad prim, PrimType ty) => MutableBlock ty (PrimState prim) -> Offset ty -> ty -> prim ()
unsafeWrite (MutableBlock mba) i v = primMbaWrite mba i v
{-# INLINE unsafeWrite #-}

-- | Copy a number of elements from an array to another array with offsets
unsafeCopyElements :: forall prim ty . (PrimMonad prim, PrimType ty)
                   => MutableBlock ty (PrimState prim) -- ^ destination mutable block
                   -> Offset ty                        -- ^ offset at destination
                   -> MutableBlock ty (PrimState prim) -- ^ source mutable block
                   -> Offset ty                        -- ^ offset at source
                   -> Size ty                          -- ^ number of elements to copy
                   -> prim ()
unsafeCopyElements dstMb destOffset srcMb srcOffset n = -- (MutableBlock dstMba) ed (MutableBlock srcBa) es n =
    unsafeCopyBytes dstMb (offsetOfE sz destOffset)
                    srcMb (offsetOfE sz srcOffset)
                    (sizeOfE sz n)
  where
    !sz = primSizeInBytes (Proxy :: Proxy ty)

unsafeCopyElementsRO :: forall prim ty . (PrimMonad prim, PrimType ty)
                     => MutableBlock ty (PrimState prim) -- ^ destination mutable block
                     -> Offset ty                        -- ^ offset at destination
                     -> Block ty                         -- ^ source block
                     -> Offset ty                        -- ^ offset at source
                     -> Size ty                          -- ^ number of elements to copy
                     -> prim ()
unsafeCopyElementsRO dstMb destOffset srcMb srcOffset n =
    unsafeCopyBytesRO dstMb (offsetOfE sz destOffset)
                      srcMb (offsetOfE sz srcOffset)
                      (sizeOfE sz n)
  where
    !sz = primSizeInBytes (Proxy :: Proxy ty)

-- | Copy a number of bytes from a MutableBlock to another MutableBlock with specific byte offsets
unsafeCopyBytes :: forall prim ty . PrimMonad prim
                => MutableBlock ty (PrimState prim) -- ^ destination mutable block
                -> Offset Word8                     -- ^ offset at destination
                -> MutableBlock ty (PrimState prim) -- ^ source mutable block
                -> Offset Word8                     -- ^ offset at source
                -> Size Word8                       -- ^ number of elements to copy
                -> prim ()
unsafeCopyBytes (MutableBlock dstMba) (Offset (I# d)) (MutableBlock srcBa) (Offset (I# s)) (Size (I# n)) =
    primitive $ \st -> (# copyMutableByteArray# srcBa s dstMba d n st, () #)
{-# INLINE unsafeCopyBytes #-}

-- | Copy a number of bytes from a Block to a MutableBlock with specific byte offsets
unsafeCopyBytesRO :: forall prim ty . PrimMonad prim
                  => MutableBlock ty (PrimState prim) -- ^ destination mutable block
                  -> Offset Word8                     -- ^ offset at destination
                  -> Block ty                         -- ^ source block
                  -> Offset Word8                     -- ^ offset at source
                  -> Size Word8                       -- ^ number of elements to copy
                  -> prim ()
unsafeCopyBytesRO (MutableBlock dstMba) (Offset (I# d)) (Block srcBa) (Offset (I# s)) (Size (I# n)) =
    primitive $ \st -> (# copyByteArray# srcBa s dstMba d n st, () #)
{-# INLINE unsafeCopyBytesRO #-}
