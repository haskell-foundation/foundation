-- |
-- Module      : Foundation.Primitive.Block
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
--
-- A block of memory that contains elements of a type,
-- very similar to an unboxed array but with the key difference:
--
-- * It doesn't have slicing capability (no cheap take or drop)
-- * It consume less memory: 1 Offset, 1 CountOf, 1 Pinning status trimmed
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
    -- * Lowlevel functions
    , unsafeThaw
    , unsafeFreeze
    , unsafeIndex
    , thaw
    , freeze
    , copy
    -- * safer api
    , create
    , singleton
    , replicate
    , index
    , map
    , foldl'
    , foldr
    , cons
    , snoc
    , uncons
    , unsnoc
    , sub
    , splitAt
    , revSplitAt
    , splitOn
    , break
    , span
    , elem
    , all
    , any
    , find
    , filter
    , reverse
    , sortBy
    , intersperse
    -- * Foreign interfaces
    , unsafeCopyToPtr
    ) where

import           GHC.Prim
import           GHC.Types
import           GHC.ST
import qualified Data.List
import           Foundation.Internal.Base
import           Foundation.Internal.Proxy
import           Foundation.Internal.Primitive
import           Foundation.Primitive.Types.OffsetSize
import           Foundation.Primitive.Monad
import           Foundation.Primitive.Exception
import           Foundation.Primitive.Types
import qualified Foundation.Primitive.Block.Mutable as M
import           Foundation.Primitive.Block.Mutable (Block(..), MutableBlock(..), new, unsafeThaw, unsafeFreeze)
import           Foundation.Primitive.Block.Base
import           Foundation.Numerical

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
       => CountOf ty           -- ^ the size of the block (in element of ty)
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

replicate :: PrimType ty => CountOf ty -> ty -> Block ty
replicate sz ty = create sz (const ty)

-- | Thaw a Block into a MutableBlock
--
-- the Block is not modified, instead a new Mutable Block is created
-- and its content is copied to the mutable block
thaw :: (PrimMonad prim, PrimType ty) => Block ty -> prim (MutableBlock ty (PrimState prim))
thaw array = do
    ma <- M.unsafeNew unpinned (lengthBytes array)
    M.unsafeCopyBytesRO ma 0 array 0 (lengthBytes array)
    return ma
{-# INLINE thaw #-}

freeze :: (PrimType ty, PrimMonad prim) => MutableBlock ty (PrimState prim) -> prim (Block ty)
freeze ma = do
    ma' <- unsafeNew unpinned len
    M.unsafeCopyBytes ma' 0 ma 0 len
    --M.copyAt ma' (Offset 0) ma (Offset 0) len
    unsafeFreeze ma'
  where
    len = M.mutableLengthBytes ma

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
    !len = length array
{-# INLINE index #-}

-- | Map all element 'a' from a block to a new block of 'b'
map :: (PrimType a, PrimType b) => (a -> b) -> Block a -> Block b
map f a = create lenB (\i -> f $ unsafeIndex a (offsetCast Proxy i))
  where !lenB = sizeCast (Proxy :: Proxy (a -> b)) (length a)

foldr :: PrimType ty => (ty -> a -> a) -> a -> Block ty -> a
foldr f initialAcc vec = loop 0
  where
    !len = length vec
    loop i
        | i .==# len = initialAcc
        | otherwise  = unsafeIndex vec i `f` loop (i+1)

foldl' :: PrimType ty => (a -> ty -> a) -> a -> Block ty -> a
foldl' f initialAcc vec = loop 0 initialAcc
  where
    !len = length vec
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
    !len = length vec

snoc :: PrimType ty => Block ty -> ty -> Block ty
snoc vec e
    | len == CountOf 0 = singleton e
    | otherwise     = runST $ do
        muv <- new (len + 1)
        M.unsafeCopyElementsRO muv 0 vec 0 len
        M.unsafeWrite muv (0 `offsetPlusE` length vec) e
        unsafeFreeze muv
  where
     !len = length vec

sub :: PrimType ty => Block ty -> Offset ty -> Offset ty -> Block ty
sub blk start end
    | start >= end' = mempty
    | otherwise     = runST $ do
        dst <- new newLen
        M.unsafeCopyElementsRO dst 0 blk start newLen
        unsafeFreeze dst
  where
    newLen = end' - start
    end' = min (sizeAsOffset len) end
    !len = length blk

uncons :: PrimType ty => Block ty -> Maybe (ty, Block ty)
uncons vec
    | nbElems == 0 = Nothing
    | otherwise    = Just (unsafeIndex vec 0, sub vec 1 (0 `offsetPlusE` nbElems))
  where
    !nbElems = length vec

unsnoc :: PrimType ty => Block ty -> Maybe (Block ty, ty)
unsnoc vec
    | nbElems == 0 = Nothing
    | otherwise    = Just (sub vec 0 lastElem, unsafeIndex vec lastElem)
  where
    !lastElem = 0 `offsetPlusE` (nbElems - 1)
    !nbElems = length vec

splitAt :: PrimType ty => CountOf ty -> Block ty -> (Block ty, Block ty)
splitAt nbElems blk
    | nbElems <= 0 = (mempty, blk)
    | n == vlen    = (blk, mempty)
    | otherwise    = runST $ do
        left  <- new nbElems
        right <- new (vlen - nbElems)
        M.unsafeCopyElementsRO left  0 blk 0                      nbElems
        M.unsafeCopyElementsRO right 0 blk (sizeAsOffset nbElems) (vlen - nbElems)

        (,) <$> unsafeFreeze left <*> unsafeFreeze right
  where
    n    = min nbElems vlen
    vlen = length blk

revSplitAt :: PrimType ty => CountOf ty -> Block ty -> (Block ty, Block ty)
revSplitAt n blk
    | n <= 0    = (mempty, blk)
    | otherwise = let (x,y) = splitAt (length blk - n) blk in (y,x)

break :: PrimType ty => (ty -> Bool) -> Block ty -> (Block ty, Block ty)
break predicate blk = findBreak 0
  where
    !len = length blk
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
    !len = length blk
    loop i
        | i .==# len             = False
        | unsafeIndex blk i == v = True
        | otherwise              = loop (i+1)

all :: PrimType ty => (ty -> Bool) -> Block ty -> Bool
all p blk = loop 0
  where
    !len = length blk
    loop i
        | i .==# len            = True
        | p (unsafeIndex blk i) = loop (i+1)
        | otherwise             = False

any :: PrimType ty => (ty -> Bool) -> Block ty -> Bool
any p blk = loop 0
  where
    !len = length blk
    loop i
        | i .==# len            = False
        | p (unsafeIndex blk i) = True
        | otherwise             = loop (i+1)

splitOn :: PrimType ty => (ty -> Bool) -> Block ty -> [Block ty]
splitOn predicate blk
    | len == 0  = [mempty]
    | otherwise = go 0 0
  where
    !len = length blk
    go !prevIdx !idx
        | idx .==# len = [sub blk prevIdx idx]
        | otherwise    =
            let e = unsafeIndex blk idx
                idx' = idx + 1
             in if predicate e
                    then sub blk prevIdx idx : go idx' idx'
                    else go prevIdx idx'

find :: PrimType ty => (ty -> Bool) -> Block ty -> Maybe ty
find predicate vec = loop 0
  where
    !len = length vec
    loop i
        | i .==# len = Nothing
        | otherwise  =
            let e = unsafeIndex vec i
             in if predicate e then Just e else loop (i+1)

filter :: PrimType ty => (ty -> Bool) -> Block ty -> Block ty
filter predicate vec = fromList $ Data.List.filter predicate $ toList vec

reverse :: forall ty . PrimType ty => Block ty -> Block ty
reverse blk
    | len == 0  = mempty
    | otherwise = runST $ do
        mb <- new len
        go mb
        unsafeFreeze mb
  where
    !len = length blk
    !endOfs = 0 `offsetPlusE` len

    go :: MutableBlock ty s -> ST s ()
    go mb = loop endOfs 0
      where
        loop o i
            | i .==# len = pure ()
            | otherwise  = unsafeWrite mb o' (unsafeIndex blk i) >> loop o' (i+1)
          where o' = pred o

sortBy :: forall ty . PrimType ty => (ty -> ty -> Ordering) -> Block ty -> Block ty
sortBy xford vec
    | len == 0  = mempty
    | otherwise = runST (thaw vec >>= doSort xford)
  where
    len = length vec
    doSort :: (PrimType ty, PrimMonad prim) => (ty -> ty -> Ordering) -> MutableBlock ty (PrimState prim) -> prim (Block ty)
    doSort ford ma = qsort 0 (sizeLastOffset len) >> unsafeFreeze ma
      where
        qsort lo hi
            | lo >= hi  = return ()
            | otherwise = do
                p <- partition lo hi
                qsort lo (pred p)
                qsort (p+1) hi
        partition lo hi = do
            pivot <- unsafeRead ma hi
            let loop i j
                    | j == hi   = pure i
                    | otherwise = do
                        aj <- unsafeRead ma j
                        i' <- if ford aj pivot == GT
                                then pure i
                                else do
                                    ai <- unsafeRead ma i
                                    unsafeWrite ma j ai
                                    unsafeWrite ma i aj
                                    pure $ i + 1
                        loop i' (j+1)

            i <- loop lo lo
            ai  <- unsafeRead ma i
            ahi <- unsafeRead ma hi
            unsafeWrite ma hi ai
            unsafeWrite ma i ahi
            pure i

intersperse :: forall ty . PrimType ty => ty -> Block ty -> Block ty
intersperse sep blk
    | len <= 1  = blk
    | otherwise = runST $ do
        mb <- new newSize
        go mb
        unsafeFreeze mb
  where
    !len = length blk
    newSize = len + len - 1

    go :: MutableBlock ty s -> ST s ()
    go mb = loop 0 0
      where
        loop !o !i
            | i .==# (len - 1) = unsafeWrite mb o (unsafeIndex blk i)
            | otherwise        = do
                unsafeWrite mb o     (unsafeIndex blk i)
                unsafeWrite mb (o+1) sep
                loop (o+2) (i+1)
