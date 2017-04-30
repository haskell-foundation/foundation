-- |
-- Module      : Foundation.Primitive.Block
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
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}
module Foundation.Primitive.Block
    ( Block(..)
    , MutableBlock(..)
    -- * Properties
    , length
    , lengthSize
    -- * Lowlevel functions
    , unsafeThaw
    , unsafeFreeze
    , unsafeIndex
    , thaw
    , copy
    -- * safer api
    , create
    , singleton
    , replicate
    , index
    , map
    , foldl
    , foldl'
    , foldr
    , cons
    , snoc
    , uncons
    , unsnoc
    , sub
    , splitAt
    , break
    , span
    , elem
    , all
    , any
    -- * Foreign interfaces
    , unsafeCopyToPtr
    ) where

import           GHC.Prim
import           GHC.Types
import           GHC.ST
import           Foundation.Internal.Base hiding (fromList, toList)
import           Foundation.Internal.Proxy
import           Foundation.Internal.Primitive
import           Foundation.Primitive.Types.OffsetSize
import           Foundation.Primitive.Monad
import           Foundation.Primitive.Exception
import           Foundation.Primitive.IntegralConv
import           Foundation.Primitive.Types
import qualified Foundation.Primitive.Block.Mutable as M
import           Foundation.Primitive.Block.Mutable (Block(..), MutableBlock(..), new, unsafeThaw, unsafeFreeze)
import           Foundation.Primitive.Block.Base
import           Foundation.Numerical

-- | return the number of elements of the array.
length :: PrimType ty => Block ty -> Int
length a = let (Size len) = lengthSize a in len
{-# INLINE[1] length #-}

-- | Copy all the block content to the memory starting at the destination address
unsafeCopyToPtr :: forall ty prim . PrimMonad prim
                => Block ty -- ^ the source block to copy
                -> Ptr ty   -- ^ The destination address where the copy is going to start
                -> prim ()
unsafeCopyToPtr (Block blk) (Ptr p) = primitive $ \s1 ->
    (# compatCopyByteArrayToAddr# blk 0# p (sizeofByteArray# blk) s1, () #)

-- | Create a new array of size @n by settings each cells through the
-- function @f.
create :: forall ty . PrimType ty
       => Size ty           -- ^ the size of the block (in element of ty)
       -> (Offset ty -> ty) -- ^ the function that set the value at the index
       -> Block ty          -- ^ the array created
create n initializer
    | n == 0    = mempty
    | otherwise = runST $ do
        mb <- new n
        M.iterSet initializer mb
        unsafeFreeze mb

singleton :: PrimType ty => ty -> Block ty
singleton ty = create 1 (const ty)

replicate :: PrimType ty => Word -> ty -> Block ty
replicate sz ty = create (Size (integralCast sz)) (const ty)

-- | Thaw a Block into a MutableBlock
--
-- the Block is not modified, instead a new Mutable Block is created
-- and its content is copied to the mutable block
thaw :: (PrimMonad prim, PrimType ty) => Block ty -> prim (MutableBlock ty (PrimState prim))
thaw array = do
    ma <- M.unsafeNew (lengthBytes array)
    M.unsafeCopyBytesRO ma 0 array 0 (lengthBytes array)
    return ma
{-# INLINE thaw #-}

-- | Copy every cells of an existing Block to a new Block
copy :: PrimType ty => Block ty -> Block ty
copy array = runST (thaw array >>= unsafeFreeze)

-- | Return the element at a specific index from an array.
--
-- If the index @n is out of bounds, an error is raised.
index :: PrimType ty => Block ty -> Offset ty -> ty
index array n
    | isOutOfBound n len = outOfBound OOB_Index n len
    | otherwise          = unsafeIndex array n
  where
    !len = lengthSize array
{-# INLINE index #-}

-- | Map all element 'a' from a block to a new block of 'b'
map :: (PrimType a, PrimType b) => (a -> b) -> Block a -> Block b
map f a = create lenB (\i -> f $ unsafeIndex a (offsetCast Proxy i))
  where !lenB = sizeCast (Proxy :: Proxy (a -> b)) (lengthSize a)

foldl :: PrimType ty => (a -> ty -> a) -> a -> Block ty -> a
foldl f initialAcc vec = loop 0 initialAcc
  where
    !len = lengthSize vec
    loop i acc
        | i .==# len = acc
        | otherwise  = loop (i+1) (f acc (unsafeIndex vec i))

foldr :: PrimType ty => (ty -> a -> a) -> a -> Block ty -> a
foldr f initialAcc vec = loop 0
  where
    !len = lengthSize vec
    loop i
        | i .==# len = initialAcc
        | otherwise  = unsafeIndex vec i `f` loop (i+1)

foldl' :: PrimType ty => (a -> ty -> a) -> a -> Block ty -> a
foldl' f initialAcc vec = loop 0 initialAcc
  where
    !len = lengthSize vec
    loop i !acc
        | i .==# len = acc
        | otherwise  = loop (i+1) (f acc (unsafeIndex vec i))

cons :: PrimType ty => ty -> Block ty -> Block ty
cons e vec
    | len == 0  = singleton e
    | otherwise = runST $ do
        muv <- new (len + 1)
        M.unsafeCopyElementsRO muv 1 vec 0 len
        M.unsafeWrite muv 0 e
        unsafeFreeze muv
  where
    !len = lengthSize vec

snoc :: PrimType ty => Block ty -> ty -> Block ty
snoc vec e
    | len == Size 0 = singleton e
    | otherwise     = runST $ do
        muv <- new (len + 1)
        M.unsafeCopyElementsRO muv 0 vec 0 len
        M.unsafeWrite muv (0 `offsetPlusE` lengthSize vec) e
        unsafeFreeze muv
  where
     !len = lengthSize vec

sub :: PrimType ty => Block ty -> Offset ty -> Offset ty -> Block ty
sub vec start end
    | start >= end = mempty
    | otherwise    = runST $ do
        dst <- new len
        M.unsafeCopyElementsRO dst 0 vec start newLen
        unsafeFreeze dst
  where
    newLen = end' - start
    end' = min end (start `offsetPlusE` (end - start))
    !len = lengthSize vec

uncons :: PrimType ty => Block ty -> Maybe (ty, Block ty)
uncons vec
    | nbElems == 0 = Nothing
    | otherwise    = Just (unsafeIndex vec 0, sub vec 1 (0 `offsetPlusE` nbElems))
  where
    !nbElems = lengthSize vec

unsnoc :: PrimType ty => Block ty -> Maybe (Block ty, ty)
unsnoc vec
    | nbElems == 0 = Nothing
    | otherwise    = Just (sub vec 0 lastElem, unsafeIndex vec lastElem)
  where
    !lastElem = 0 `offsetPlusE` (nbElems - 1)
    !nbElems = lengthSize vec

splitAt :: PrimType ty => Size ty -> Block ty -> (Block ty, Block ty)
splitAt nbElems v
    | nbElems <= 0 = (mempty, v)
    | n == vlen    = (v, mempty)
    | otherwise    = runST $ do
        left  <- new nbElems
        right <- new (vlen - nbElems)
        M.unsafeCopyElementsRO left  0 v 0                      nbElems
        M.unsafeCopyElementsRO right 0 v (sizeAsOffset nbElems) (vlen - nbElems)

        (,) <$> unsafeFreeze left <*> unsafeFreeze right
  where
    n    = min nbElems vlen
    vlen = lengthSize v

break :: PrimType ty => (ty -> Bool) -> Block ty -> (Block ty, Block ty)
break predicate blk = findBreak 0
  where
    !len = lengthSize blk
    findBreak !i
        | i .==# len                    = (blk, mempty)
        | predicate (unsafeIndex blk i) = splitAt (offsetAsSize i) blk
        | otherwise                     = findBreak (i + 1)
    {-# INLINE findBreak #-}

span :: PrimType ty => (ty -> Bool) -> Block ty -> (Block ty, Block ty)
span p = break (not . p)

elem :: PrimType ty => ty -> Block ty -> Bool
elem v blk = loop 0
  where
    !len = lengthSize blk
    loop i
        | i .==# len             = False
        | unsafeIndex blk i == v = True
        | otherwise              = loop (i+1)

all :: PrimType ty => (ty -> Bool) -> Block ty -> Bool
all p blk = loop 0
  where
    !len = lengthSize blk
    loop i
        | i .==# len            = True
        | p (unsafeIndex blk i) = loop (i+1)
        | otherwise             = False

any :: PrimType ty => (ty -> Bool) -> Block ty -> Bool
any p blk = loop 0
  where
    !len = lengthSize blk
    loop i
        | i .==# len            = False
        | p (unsafeIndex blk i) = True
        | otherwise             = loop (i+1)
