-- |
-- Module      : Core.Vector.Unboxed
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- A simple array abstraction that allow to use typed
-- array of bytes where the array is pinned in memory
-- to allow easy use with Foreign interfaces, ByteString
-- and always aligned to 64 bytes.
--
-- Import this module qualified
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Core.Vector.Unboxed
    ( UVector
    , MUVector
    , PrimType(..)
    -- * methods
    , mutableLength
    , copy
    -- * internal methods
    , copyAddr
    -- * Creation
    , new
    , create
    -- * accessors
    , update
    , unsafeUpdate
    -- * Functions
    , map
    , mapIndex
    ) where

import           GHC.Prim
import           GHC.Types
import           GHC.ST
import           GHC.Ptr
import qualified Prelude
import           Core.Internal.Base
import           Core.Internal.Primitive
import           Core.Internal.Proxy
import           Core.Primitive.Monad
import           Core.Primitive.Types
import           Core.Primitive.Utils
import qualified Core.Collection as C
import           Core.Vector.Common
import           Core.Number
import qualified Data.List

-- | An array of type built on top of GHC primitive.
--
-- The elements need to have fixed sized and the representation is a
-- packed contiguous array in memory that can easily be passed
-- to foreign interface
data UVector ty = A ByteArray#

-- | A Mutable array of types built on top of GHC primitive.
--
-- Element in this array can be modified in place.
data MUVector ty st = MA (MutableByteArray# st)

instance (PrimType ty, Show ty) => Show (UVector ty) where
    show v = show (toList v)
instance (PrimType ty, Eq ty) => Eq (UVector ty) where
    (==) = equal
instance (PrimType ty, Ord ty) => Ord (UVector ty) where
    compare = vCompare

instance PrimType ty => Monoid (UVector ty) where
    mempty  = empty
    mappend = append
    mconcat = concat

type instance C.Element (UVector ty) = ty

instance PrimType ty => IsList (UVector ty) where
    type Item (UVector ty) = ty
    fromList = vFromList
    toList = vToList

instance PrimType ty => C.InnerFunctor (UVector ty) where
    imap = map

instance PrimType ty => C.SemiOrderedCollection (UVector ty) where
    snoc = snoc
    cons = cons
    find = find
    sortBy = sortBy
    length = length
    singleton = fromList . (:[])

instance PrimType ty => C.OrderedCollection (UVector ty) where
    null = null
    take = take
    drop = drop
    splitAt = splitAt
    splitOn = splitOn
    break = break
    span = span
    filter = filter
    reverse = reverse

instance PrimType ty => C.IndexedCollection (UVector ty) where
    (!) l n
        | n < 0 || n >= length l = Nothing
        | otherwise              = Just $ index l n
    findIndex predicate c = loop 0
      where
        !len = length c
        loop i
            | i == len                    = Nothing
            | predicate (unsafeIndex c i) = Just i
            | otherwise                   = Nothing

instance PrimType ty => C.MutableCollection (MUVector ty) where
    type Collection (MUVector ty) = UVector ty
    type MutableKey (MUVector ty) = Int
    type MutableValue (MUVector ty) = ty

    thaw = thaw
    freeze = freeze
    unsafeThaw = unsafeThaw
    unsafeFreeze = unsafeFreeze

    mutUnsafeWrite = unsafeWrite
    mutUnsafeRead = unsafeRead
    mutWrite = write
    mutRead = read

{-
fmapUVec :: (PrimType a, PrimType b) => (a -> b) -> UVector a -> UVector b
fmapUVec mapper a = runST (new nbElems >>= copyMap (unsafeIndex a) mapper)
  where
    !nbElems = length a
    copyMap :: (PrimType a, PrimType b, PrimMonad prim)
            => (Int -> a) -> (a -> b) -> MUVector b (PrimState prim) -> prim (UVector b)
    copyMap get f ma = iter 0
      where
        iter i
            | i == nbElems = unsafeFreeze ma
            | otherwise    = unsafeWrite ma i (f $ get i) >> iter (i+1)
-}

-- rename to sizeInBitsOfCell
sizeInBitsOfContent :: PrimType ty => UVector ty -> Int
sizeInBitsOfContent = getSize Proxy
  where getSize :: PrimType ty => Proxy ty -> UVector ty -> Int
        getSize ty _ = sizeInBits ty
{-# INLINE sizeInBitsOfContent #-}

-- | Copy every cells of an existing array to a new array
copy :: PrimType ty => UVector ty -> UVector ty
copy array = runST (thaw array >>= unsafeFreeze)

-- | Thaw an array to a mutable array.
--
-- the array is not modified, instead a new mutable array is created
-- and every values is copied, before returning the mutable array.
thaw :: (PrimMonad prim, PrimType ty) => UVector ty -> prim (MUVector ty (PrimState prim))
thaw array@(A ba) = do
    ma@(MA mba) <- new (length array)
    primCopyFreezedBytes mba ba
    return ma
{-# INLINE thaw #-}

-- | Return the element at a specific index from an array.
--
-- If the index @n is out of bounds, an error is raised.
index :: PrimType ty => UVector ty -> Int -> ty
index array n
    | n < 0 || n >= len = throw (OutOfBound OOB_Index n len)
    | otherwise         = unsafeIndex array n
  where len = length array
{-# INLINE index #-}

-- | Return the element at a specific index from an array without bounds checking.
--
-- Reading from invalid memory can return unpredictable and invalid values.
-- use 'index' if unsure.
unsafeIndex :: PrimType ty => UVector ty -> Int -> ty
unsafeIndex (A ba) = primBaIndex ba
{-# INLINE unsafeIndex #-}

-- | read a cell in a mutable array.
--
-- If the index is out of bounds, an error is raised.
read :: (PrimMonad prim, PrimType ty) => MUVector ty (PrimState prim) -> Int -> prim ty
read array n
    | n < 0 || n >= len = primThrow (OutOfBound OOB_Read n len)
    | otherwise         = unsafeRead array n
  where len = mutableLength array
{-# INLINE read #-}

-- | read from a cell in a mutable array without bounds checking.
--
-- Reading from invalid memory can return unpredictable and invalid values.
-- use 'read' if unsure.
unsafeRead :: (PrimMonad prim, PrimType ty) => MUVector ty (PrimState prim) -> Int -> prim ty
unsafeRead (MA mba) i = primMbaRead mba i
{-# INLINE unsafeRead #-}

-- | Write to a cell in a mutable array.
--
-- If the index is out of bounds, an error is raised.
write :: (PrimMonad prim, PrimType ty) => MUVector ty (PrimState prim) -> Int -> ty -> prim ()
write array n val
    | n < 0 || n >= len = primThrow (OutOfBound OOB_Write n len)
    | otherwise         = unsafeWrite array n val
  where len = mutableLength array
{-# INLINE write #-}

-- | write to a cell in a mutable array without bounds checking.
--
-- Writing with invalid bounds will corrupt memory and your program will
-- become unreliable. use 'write' if unsure.
unsafeWrite :: (PrimMonad prim, PrimType ty) => MUVector ty (PrimState prim) -> Int -> ty -> prim ()
unsafeWrite (MA mba) i = primMbaWrite mba i
{-# INLINE unsafeWrite #-}

-- | Create a new mutable array of size @n.
--
-- all the cells are uninitialized and could contains invalid values.
--
-- All mutable arrays are allocated on a 64 bits aligned addresses
new :: (PrimMonad prim, PrimType ty) => Int -> prim (MUVector ty (PrimState prim))
new (I# n) = newFake Proxy
  where newFake :: (PrimMonad prim, PrimType ty) => Proxy ty -> prim (MUVector ty (PrimState prim))
        newFake ty = primitive $ \s1 ->
            case newAlignedPinnedByteArray# bytes 8# s1 of
                (# s2, mba #) -> (# s2, MA mba #)
          where !(I# szBits) = sizeInBits ty
                !bytes = quotInt# (roundUp (n *# szBits) 8#) 8#
                -- !(I# bytes)   = trace ("new: " ++ show (I# szBits) ++ "bits/elem " ++ show (I# n) ++ " elems " ++ show (I# bytes_) ++ " bytes") (I# bytes_)

                roundUp :: Int# -> Int# -> Int#
                roundUp i d = negateInt# (compatAndI# (negateInt# i)  (negateInt# d))
                {-# INLINE roundUp #-}
        {-# INLINE newFake #-}
{-# INLINE new #-}

-- | Copy a number of elements from an array to another array with offsets
copyAt :: (PrimMonad prim, PrimType ty)
       => MUVector ty (PrimState prim) -- ^ destination array
       -> Int                -- ^ offset at destination
       -> MUVector ty (PrimState prim) -- ^ source array
       -> Int                -- ^ offset at source
       -> Int                -- ^ number of elements to copy
       -> prim ()
copyAt dst od src os n = loop od os
  where endIndex = os + n
        loop d i
            | i == endIndex = return ()
            | otherwise     = unsafeRead src i >>= unsafeWrite dst d >> loop (d+1) (i+1)

copyAtRO :: (PrimMonad prim, PrimType ty)
         => MUVector ty (PrimState prim) -- ^ destination array
         -> Int                -- ^ offset at destination
         -> UVector ty         -- ^ source array
         -> Int                -- ^ offset at source
         -> Int                -- ^ number of elements to copy
         -> prim ()
copyAtRO dst od src os n = loop od os
  where endIndex = os + n
        loop d i
            | i == endIndex = return ()
            | otherwise     = unsafeWrite dst d (unsafeIndex src i) >> loop (d+1) (i+1)

copyAddr :: (PrimMonad prim, PrimType ty)
         => MUVector ty (PrimState prim) -- ^ destination array
         -> Int                -- ^ offset at destination
         -> Ptr Word8          -- ^ source ptr
         -> Int                -- ^ offset at source
         -> Int                -- ^ number of elements to copy
         -> prim ()
copyAddr (MA dst) (I# od) (Ptr src) (I# os) (I# sz) = primitive $ \s ->
    (# copyAddrToByteArray# (plusAddr# src os) dst od sz s, () #)

-- | return the number of elements of the array.
length :: UVector ty -> Int
length (A a) = I# (sizeofByteArray# a)
{-# INLINE length #-}

-- | return the numbers of elements in a mutable array
mutableLength :: MUVector ty st -> Int
mutableLength (MA ma) = I# (sizeofMutableByteArray# ma)
{-# INLINE mutableLength #-}

-- | Freeze a mutable array into an array.
--
-- the MUVector must not be changed after freezing.
unsafeFreeze :: PrimMonad prim => MUVector ty (PrimState prim) -> prim (UVector ty)
unsafeFreeze (MA mba) = primitive $ \s1 ->
    case unsafeFreezeByteArray# mba s1 of
        (# s2, ba #) -> (# s2, A ba #)
{-# INLINE unsafeFreeze #-}

freeze :: (PrimType ty, PrimMonad prim) => MUVector ty (PrimState prim) -> prim (UVector ty)
freeze ma = do
    ma' <- new len
    copyAt ma' 0 ma 0 len
    unsafeFreeze ma'
  where len = mutableLength ma
-- | Thaw an immutable array.
--
-- The UVector must not be used after thawing.
unsafeThaw :: PrimMonad prim => UVector ty -> prim (MUVector ty (PrimState prim))
unsafeThaw (A ba) = primitive $ \st -> (# st, MA (unsafeCoerce# ba) #)
{-# INLINE unsafeThaw #-}

-- | Create a new array of size @n by settings each cells through the
-- function @f.
create :: PrimType ty
       => Int         -- ^ the size of the array
       -> (Int -> ty) -- ^ the function that set the value at the index
       -> UVector ty  -- ^ the array created
create n initializer = runST (new n >>= iter initializer)
  where
    iter :: (PrimType ty, PrimMonad prim) => (Int -> ty) -> MUVector ty (PrimState prim) -> prim (UVector ty)
    iter f ma = loop 0
      where
        loop i
            | i == n    = unsafeFreeze ma
            | otherwise = unsafeWrite ma i (f i) >> loop (i+1)
        {-# INLINE loop #-}
    {-# INLINE iter #-}

-----------------------------------------------------------------------
-- higher level collection implementation
-----------------------------------------------------------------------

empty :: PrimType ty => UVector ty
empty = runST (new 0 >>= unsafeFreeze)

-- | make an array from a list of elements.
vFromList :: PrimType ty => [ty] -> UVector ty
vFromList l = runST $ do
    ma <- new len
    iter 0 l $ \i x -> unsafeWrite ma i x
    unsafeFreeze ma
  where len = C.length l
        iter _ [] _ = return ()
        iter i (x:xs) z = z i x >> iter (i+1) xs z

-- | transform an array to a list.
vToList :: PrimType ty => UVector ty -> [ty]
vToList a = loop 0
  where len = length a
        loop i | i == len  = []
               | otherwise = unsafeIndex a i : loop (i+1)

-- | Check if two vectors are identical
equal :: (PrimType ty, Eq ty) => UVector ty -> UVector ty -> Bool
equal a b
    | la /= lb  = False
    | otherwise = loop 0
  where
    !la = length a
    !lb = length b
    loop n | n == la    = True
           | otherwise = (unsafeIndex a n == unsafeIndex b n) && loop (n+1)

-- | Compare 2 vectors
vCompare :: (Ord ty, PrimType ty) => UVector ty -> UVector ty -> Ordering
vCompare a b = loop 0
  where
    !la = length a
    !lb = length b
    loop n
        | n == la   = if la == lb then EQ else LT
        | n == lb   = GT
        | otherwise =
            case unsafeIndex a n `compare` unsafeIndex b n of
                EQ -> loop (n+1)
                r  -> r

-- | Append 2 arrays together by creating a new bigger array
append :: PrimType ty => UVector ty -> UVector ty -> UVector ty
append a b
    | la == 0 && lb == 0 = empty
    | la == 0            = b
    | lb == 0            = a
    | otherwise = runST $ do
        r  <- new (la+lb)
        ma <- unsafeThaw a
        mb <- unsafeThaw b
        copyAt r 0 ma 0 la
        copyAt r la mb 0 lb
        unsafeFreeze r
  where
    !la = length a
    !lb = length b

concat :: PrimType ty => [UVector ty] -> UVector ty
concat l = runST $ do
    r <- new (Prelude.sum $ fmap length l)
    loop r 0 l
    unsafeFreeze r
  where loop _ _ []     = return ()
        loop r i (x:xs) = do
            mx <- unsafeThaw x
            copyAt r i mx 0 lx
            loop r (i+lx) xs
          where lx = length x

-- | update an array by creating a new array with the updates.
--
-- the operation copy the previous array, modify it in place, then freeze it.
update :: PrimType ty
       => UVector ty
       -> [(Int, ty)]
       -> UVector ty
update array modifiers = runST (thaw array >>= doUpdate modifiers)
  where doUpdate l ma = loop l
          where loop []         = unsafeFreeze ma
                loop ((i,v):xs) = write ma i v >> loop xs
                {-# INLINE loop #-}
        {-# INLINE doUpdate #-}

unsafeUpdate :: PrimType ty
             => UVector ty
             -> [(Int, ty)]
             -> UVector ty
unsafeUpdate array modifiers = runST (thaw array >>= doUpdate modifiers)
  where doUpdate l ma = loop l
          where loop []         = unsafeFreeze ma
                loop ((i,v):xs) = unsafeWrite ma i v >> loop xs
                {-# INLINE loop #-}
        {-# INLINE doUpdate #-}

{-
withConstPtr :: UVector ty
             -> (Ptr Word8 -> IO a)
             -> IO a
withConstPtr (A _ a) f =
    f $ Ptr (byteArrayContents# a)

withMutablePtr :: MUVector ty RealWorld
               -> (Ptr Word8 -> IO a)
               -> IO a
withMutablePtr ma f =
    stToIO (unsafeFreeze ma) >>= flip withConstPtr f
-}

null :: UVector ty -> Bool
null = (==) 0 . length

take :: PrimType ty => Int -> UVector ty -> UVector ty
take nbElems v
    | nbElems <= 0 = empty
    | otherwise    = runST $ do
        muv <- new n
        copyAtRO muv 0 v 0 n
        unsafeFreeze muv
  where
    n = min nbElems (length v)

drop :: PrimType ty => Int -> UVector ty -> UVector ty
drop nbElems v
    | nbElems <= 0 = v
    | otherwise    = runST $ do
        muv <- new n
        copyAtRO muv 0 v offset n
        unsafeFreeze muv
  where
    offset = min nbElems (length v)
    n = length v - offset

splitAt :: PrimType ty => Int -> UVector ty -> (UVector ty, UVector ty)
splitAt n v = (take n v, drop n v)

splitOn :: PrimType ty => (ty -> Bool) -> UVector ty -> [UVector ty]
splitOn predicate vec
    | len == 0  = []
    | otherwise = loop 0 0
  where
    !len = length vec
    loop prevIdx idx
        | idx == len = [runST $ sub vec prevIdx idx]
        | otherwise  =
            let e = unsafeIndex vec idx
                idx' = idx + 1
             in if predicate e
                    then runST (sub vec prevIdx idx) : loop idx' idx'
                    else loop prevIdx idx'

sub :: (PrimType ty, PrimMonad prim) => UVector ty -> Int -> Int -> prim (UVector ty)
sub vec@(A ba) startIdx expectedEndIdx
    | startIdx == endIdx     = return empty
    | startIdx >= length vec = return empty
    | leftBits /= 0          = error "cannot sub on vector with non-byte aligned data yet"
    | otherwise              = do
        muv@(MA mba) <- new (endIdx - startIdx)
        primitive $ \st ->
            let sz  = end -# start
                st2 = copyByteArray# ba start mba 0# sz st
             in (# st2, () #)
        unsafeFreeze muv
  where
    endIdx = min expectedEndIdx (length vec)
    !(I# end) = endIdx * bytes
    !(I# start) = startIdx * bytes
    bits = sizeInBitsOfContent vec
    (bytes, leftBits) = bits `Prelude.divMod` 8

break :: PrimType ty => (ty -> Bool) -> UVector ty -> (UVector ty, UVector ty)
break predicate v = findBreak 0
  where
    findBreak i
        | i == length v = (v, empty)
        | otherwise     =
            if predicate (unsafeIndex v i)
                then splitAt i v
                else findBreak (i+1)

span :: PrimType ty => (ty -> Bool) -> UVector ty -> (UVector ty, UVector ty)
span p = break (not . p)

map :: (PrimType a, PrimType b) => (a -> b) -> UVector a -> UVector b
map f a = create (length a) (\i -> f $ unsafeIndex a i)

mapIndex :: (PrimType a, PrimType b) => (Int -> a -> b) -> UVector a -> UVector b
mapIndex f a = create (length a) (\i -> f i $ unsafeIndex a i)

cons :: PrimType ty => ty -> UVector ty -> UVector ty
cons e vec@(A ba)
    | leftBits /= 0 = error "cannot cons on bit vectors"
    | otherwise     = runST $ do
        muv@(MA mba) <- new (len + 1)
        -- bench with "copyAtRO muv 1 vec 0 len"
        primCopyFreezedBytesOffset mba bytes ba (len# *# bytes)
        unsafeWrite muv 0 e
        unsafeFreeze muv
  where
    bits = sizeInBitsOfContent vec
    !(I# bytes, leftBits) = bits `Prelude.divMod` 8
    !len@(I# len#) = length vec

snoc :: PrimType ty => UVector ty -> ty -> UVector ty
snoc vec@(A ba) e = runST $ do
    muv@(MA mba) <- new (len + 1)
    primCopyFreezedBytes mba ba
    unsafeWrite muv len e
    unsafeFreeze muv
  where
    !len = length vec

find :: PrimType ty => (ty -> Bool) -> UVector ty -> Maybe ty
find predicate vec = loop 0
  where
    !len = length vec
    loop i
        | i == len  = Nothing
        | otherwise =
            let e = unsafeIndex vec i
             in if predicate e then Just e else loop (i+1)

sortBy :: PrimType ty => (ty -> ty -> Ordering) -> UVector ty -> UVector ty
sortBy xford vec = runST (thaw vec >>= doSort xford)
  where
    len = length vec
    doSort :: (PrimType ty, PrimMonad prim) => (ty -> ty -> Ordering) -> MUVector ty (PrimState prim) -> prim (UVector ty)
    doSort ford ma = qsort 0 (len - 1) >> unsafeFreeze ma
      where
        qsort lo hi
            | lo >= hi  = return ()
            | otherwise = do
                p <- partition lo hi
                qsort lo (p-1)
                qsort (p+1) hi
        partition lo hi = do
            pivot <- unsafeRead ma hi
            let loop i j
                    | j == hi   = return i
                    | otherwise = do
                        aj <- unsafeRead ma j
                        i' <- if ford aj pivot == GT
                                then return i
                                else do
                                    ai <- unsafeRead ma i
                                    unsafeWrite ma j ai
                                    unsafeWrite ma i aj
                                    return $ i + 1
                        loop i' (j+1)

            i <- loop lo lo
            ai  <- unsafeRead ma i
            ahi <- unsafeRead ma hi
            unsafeWrite ma hi ai
            unsafeWrite ma i ahi
            return i

filter :: PrimType ty => (ty -> Bool) -> UVector ty -> UVector ty
filter predicate vec = vFromList $ Data.List.filter predicate $ vToList vec

reverse :: PrimType ty => UVector ty -> UVector ty
reverse a = create len toEnd
  where
    len = length a
    toEnd i = unsafeIndex a (len - i - 1)
