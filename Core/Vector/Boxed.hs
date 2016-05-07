-- |
-- Module      : Core.Vector.Boxed
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- Simple boxed array abstraction
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
module Core.Vector.Boxed
    ( Vector
    ) where

import           GHC.Prim
import           GHC.Types
import           GHC.ST
import           Core.Internal.Base
import           Core.Primitive.Monad
import           Core.Vector.Common
import qualified Core.Collection as C
import qualified Prelude
import           Prelude ((-), (+))

-- | Vector of a
data Vector a = Vector (Array# a)

data MVector a st = MVector (MutableArray# st a)

instance Functor Vector where
    fmap = map

instance Monoid (Vector a) where
    mempty  = empty
    mappend = append
    mconcat = concat

instance Show a => Show (Vector a) where
    show v = show (toList v)

instance Eq a => Eq (Vector a) where
    (==) = equal
instance Ord a => Ord (Vector a) where
    compare = vCompare

type instance C.Element (Vector ty) = ty

instance IsList (Vector ty) where
    type Item (Vector ty) = ty
    fromList = vFromList
    toList = vToList

instance C.InnerFunctor (Vector ty)

instance C.SemiOrderedCollection (Vector ty) where
    snoc = snoc
    cons = cons
    find = find
    sortBy = sortBy
    length = length
    singleton = fromList . (:[])

instance C.OrderedCollection (Vector ty) where
    null = null
    take = take
    drop = drop
    splitAt = splitAt
    splitOn = splitOn
    break = break
    span = span
    reverse = reverse
    filter = filter

instance C.MutableCollection (MVector ty) where
    type Collection (MVector ty) = Vector ty
    type MutableKey (MVector ty) = Int
    type MutableValue (MVector ty) = ty

    thaw = thaw
    freeze = freeze
    unsafeThaw = unsafeThaw
    unsafeFreeze = unsafeFreeze

    mutUnsafeWrite = unsafeWrite
    mutUnsafeRead = unsafeRead
    mutWrite = write
    mutRead = read

instance C.IndexedCollection (Vector ty) where
    (!) l n
        | n < 0 || n >= length l = Nothing
        | otherwise              = Just $ index l n
    findIndex predicate c = loop 0
      where
        !len = length c
        loop i
            | i == len  = Nothing
            | otherwise =
                if predicate (unsafeIndex c i) then Just i else Nothing

-- | return the numbers of elements in a mutable array
mutableLength :: MVector ty st -> Int
mutableLength (MVector ma) = I# (sizeofMutableArray# ma)
{-# INLINE mutableLength #-}

-- | Return the element at a specific index from an array.
--
-- If the index @n is out of bounds, an error is raised.
index :: Vector ty -> Int -> ty
index array n
    | n < 0 || n >= len = throw (OutOfBound OOB_Index n len)
    | otherwise         = unsafeIndex array n
  where len = length array
{-# INLINE index #-}

-- | Return the element at a specific index from an array without bounds checking.
--
-- Reading from invalid memory can return unpredictable and invalid values.
-- use 'index' if unsure.
unsafeIndex :: Vector ty -> Int -> ty
unsafeIndex (Vector a) (I# n) = let (# v #) = indexArray# a n in v
{-# INLINE unsafeIndex #-}

-- | read a cell in a mutable array.
--
-- If the index is out of bounds, an error is raised.
read :: PrimMonad prim => MVector ty (PrimState prim) -> Int -> prim ty
read array n
    | n < 0 || n >= len = primThrow (OutOfBound OOB_Read n len)
    | otherwise         = unsafeRead array n
  where len = mutableLength array
{-# INLINE read #-}

-- | read from a cell in a mutable array without bounds checking.
--
-- Reading from invalid memory can return unpredictable and invalid values.
-- use 'read' if unsure.
unsafeRead :: PrimMonad prim => MVector ty (PrimState prim) -> Int -> prim ty
unsafeRead (MVector ma) (I# i) = primitive $ \s1 -> readArray# ma i s1
--readArray# :: MutableArray# s a -> Int# -> State# s -> (#State# s, a#)
{-# INLINE unsafeRead #-}

-- | Write to a cell in a mutable array.
--
-- If the index is out of bounds, an error is raised.
write :: PrimMonad prim => MVector ty (PrimState prim) -> Int -> ty -> prim ()
write array n val
    | n < 0 || n >= len = primThrow (OutOfBound OOB_Write n len)
    | otherwise         = unsafeWrite array n val
  where len = mutableLength array
{-# INLINE write #-}

-- | write to a cell in a mutable array without bounds checking.
--
-- Writing with invalid bounds will corrupt memory and your program will
-- become unreliable. use 'write' if unsure.
unsafeWrite :: PrimMonad prim => MVector ty (PrimState prim) -> Int -> ty -> prim ()
unsafeWrite (MVector ma) (I# i) v = primitive $ \s1 -> let !s2 = writeArray# ma i v s1 in (# s2, () #)
{-# INLINE unsafeWrite #-}

-- | Freeze a mutable array into an array.
--
-- the MVector must not be changed after freezing.
unsafeFreeze :: PrimMonad prim => MVector ty (PrimState prim) -> prim (Vector ty)
unsafeFreeze (MVector ma) = primitive $ \s1 ->
    case unsafeFreezeArray# ma s1 of
        (# s2, a #) -> (# s2, Vector a #)
{-# INLINE unsafeFreeze #-}

-- | Thaw an immutable array.
--
-- The Vector must not be used after thawing.
unsafeThaw :: PrimMonad prim => Vector ty -> prim (MVector ty (PrimState prim))
unsafeThaw (Vector a) = primitive $ \st -> (# st, MVector (unsafeCoerce# a) #)
{-# INLINE unsafeThaw #-}

-- | Thaw an array to a mutable array.
--
-- the array is not modified, instead a new mutable array is created
-- and every values is copied, before returning the mutable array.
thaw :: PrimMonad prim => Vector ty -> prim (MVector ty (PrimState prim))
thaw array = do
    m <- new (length array)
    copyAtRO m 0 array 0 (length array)
    return m
{-# INLINE thaw #-}

freeze :: PrimMonad prim => MVector ty (PrimState prim) -> prim (Vector ty)
freeze = undefined

-- | Copy a number of elements from an array to another array with offsets
copyAt :: PrimMonad prim
       => MVector ty (PrimState prim) -- ^ destination array
       -> Int                -- ^ offset at destination
       -> MVector ty (PrimState prim) -- ^ source array
       -> Int                -- ^ offset at source
       -> Int                -- ^ number of elements to copy
       -> prim ()
copyAt dst od src os n = loop od os
  where endIndex = os + n
        loop d i
            | i == endIndex = return ()
            | otherwise     = unsafeRead src i >>= unsafeWrite dst d >> loop (d+1) (i+1)

copyAtRO :: PrimMonad prim
         => MVector ty (PrimState prim) -- ^ destination array
         -> Int                -- ^ offset at destination
         -> Vector ty         -- ^ source array
         -> Int                -- ^ offset at source
         -> Int                -- ^ number of elements to copy
         -> prim ()
copyAtRO dst od src os n = loop od os
  where endIndex = os + n
        loop d i
            | i == endIndex = return ()
            | otherwise     = unsafeWrite dst d (unsafeIndex src i) >> loop (d+1) (i+1)

-- | Create a new mutable array of size @n.
--
-- all the cells are uninitialized and could contains invalid values.
--
-- All mutable arrays are allocated on a 64 bits aligned addresses
-- and always contains a number of bytes multiples of 64 bits.
new :: PrimMonad prim => Int -> prim (MVector ty (PrimState prim))
new (I# n) = primitive $ \s1 ->
                case newArray# n (error "vector: internal error uninitialized vector") s1 of
                    (# s2, ma #) -> (# s2, MVector ma #)

-- | Create a new array of size @n by settings each cells through the
-- function @f.
create :: Int         -- ^ the size of the array
       -> (Int -> ty) -- ^ the function that set the value at the index
       -> Vector ty   -- ^ the array created
create n initializer = runST (new n >>= iter initializer)
  where
    iter :: PrimMonad prim => (Int -> ty) -> MVector ty (PrimState prim) -> prim (Vector ty)
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
equal :: Eq a => Vector a -> Vector a -> Bool
equal a b = (len == length b) && eachEqual 0
  where
    len = length a
    eachEqual !i
        | i == len                           = True
        | unsafeIndex a i /= unsafeIndex b i = False
        | otherwise                          = eachEqual (i+1)

vCompare :: Ord a => Vector a -> Vector a -> Ordering
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

empty :: Vector a
empty = runST $ onNewArray 0 (\_ s -> s)

length :: Vector a -> Int
length (Vector a) = I# (sizeofArray# a)

vFromList :: [a] -> Vector a
vFromList l = runST (new len >>= loop 0 l)
  where
    len = Prelude.length l
    loop _ []     ma = unsafeFreeze ma
    loop i (x:xs) ma = unsafeWrite ma i x >> loop (i+1) xs ma

vToList :: Vector a -> [a]
vToList v = fmap (unsafeIndex v) [0..(Prelude.subtract 1 $ length v)]

-- | Append 2 arrays together by creating a new bigger array
append :: Vector ty -> Vector ty -> Vector ty
append a b = runST $ do
    r  <- new (la+lb)
    ma <- unsafeThaw a
    mb <- unsafeThaw b
    copyAt r 0 ma 0 la
    copyAt r la mb 0 lb
    unsafeFreeze r
  where la = length a
        lb = length b

concat :: [Vector ty] -> Vector ty
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

{-
modify :: PrimMonad m
       => Vector a
       -> (MVector (PrimState m) a -> m ())
       -> m (Vector a)
modify (Vector a) f = primitive $ \st -> do
    case thawArray# a 0# (sizeofArray# a) st of
        (# st2, mv #) ->
            case internal_ (f $ MVector mv) st2 of
                st3 ->
                    case unsafeFreezeArray# mv st3 of
                        (# st4, a' #) -> (# st4, Vector a' #)
-}

-----------------------------------------------------------------------
-- helpers

onNewArray :: PrimMonad m
           => Int
           -> (MutableArray# (PrimState m) a -> State# (PrimState m) -> State# (PrimState m))
           -> m (Vector a)
onNewArray (I# len) f = primitive $ \st -> do
    case newArray# len (error "onArray") st of
        (# st2, mv #) ->
            case f mv st2 of
                st3 ->
                    case unsafeFreezeArray# mv st3 of
                        (# st4, a #) -> (# st4, Vector a #)

-----------------------------------------------------------------------


null :: Vector ty -> Bool
null = (==) 0 . length

take ::  Int -> Vector ty -> Vector ty
take nbElems v
    | nbElems <= 0 = empty
    | otherwise    = runST $ do
        muv <- new n
        copyAtRO muv 0 v 0 n
        unsafeFreeze muv
  where
    n = min nbElems (length v)

drop ::  Int -> Vector ty -> Vector ty
drop nbElems v
    | nbElems <= 0 = v
    | otherwise    = runST $ do
        muv <- new n
        copyAtRO muv 0 v offset n
        unsafeFreeze muv
  where
    offset = min nbElems (length v)
    n = length v - offset

splitAt ::  Int -> Vector ty -> (Vector ty, Vector ty)
splitAt n v = (take n v, drop n v)

splitOn ::  (ty -> Bool) -> Vector ty -> [Vector ty]
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

sub :: PrimMonad prim => Vector ty -> Int -> Int -> prim (Vector ty)
sub vec startIdx expectedEndIdx
    | startIdx == endIdx     = return empty
    | startIdx >= length vec = return empty
    | otherwise              = new sz >>= loop startIdx 0
  where
    loop os od mv
        | os == endIdx = unsafeFreeze mv
        | otherwise    = unsafeWrite mv od (unsafeIndex vec os) >> loop (os+1) (od+1) mv
    !sz  = endIdx - startIdx
    !endIdx = min expectedEndIdx (length vec)

break ::  (ty -> Bool) -> Vector ty -> (Vector ty, Vector ty)
break predicate v = findBreak 0
  where
    findBreak i
        | i == length v = (v, empty)
        | otherwise     =
            if predicate (unsafeIndex v i)
                then splitAt i v
                else findBreak (i+1)

span ::  (ty -> Bool) -> Vector ty -> (Vector ty, Vector ty)
span p = break (not . p)

map :: (a -> b) -> Vector a -> Vector b
map f a = create (length a) (\i -> f $ unsafeIndex a i)

mapIndex :: (Int -> a -> b) -> Vector a -> Vector b
mapIndex f a = create (length a) (\i -> f i $ unsafeIndex a i)

cons ::  ty -> Vector ty -> Vector ty
cons e vec = runST $ do
    mv <- new (len + 1)
    unsafeWrite mv 0 e
    copyAtRO mv 1 vec 0 len
    unsafeFreeze mv
  where
    !len = length vec

snoc ::  Vector ty -> ty -> Vector ty
snoc vec e = runST $ do
    mv <- new (len + 1)
    copyAtRO mv 0 vec 0 len
    unsafeWrite mv len e
    unsafeFreeze mv
  where
    !len = length vec

find ::  (ty -> Bool) -> Vector ty -> Maybe ty
find predicate vec = loop 0
  where
    !len = length vec
    loop i
        | i == len  = Nothing
        | otherwise =
            let e = unsafeIndex vec i
             in if predicate e then Just e else loop (i+1)

sortBy ::  (ty -> ty -> Ordering) -> Vector ty -> Vector ty
sortBy xford vec = runST (thaw vec >>= doSort xford)
  where
    len = length vec
    doSort :: PrimMonad prim => (ty -> ty -> Ordering) -> MVector ty (PrimState prim) -> prim (Vector ty)
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

filter :: (ty -> Bool) -> Vector ty -> Vector ty
filter predicate vec = runST (new len >>= copyFilterFreeze predicate (unsafeIndex vec))
  where
    !len = length vec
    copyFilterFreeze :: PrimMonad prim => (ty -> Bool) -> (Int -> ty) -> MVector ty (PrimState prim) -> prim (Vector ty)
    copyFilterFreeze predi getVec mvec = loop 0 0 >>= freezeUntilIndex mvec
      where
        loop d s
            | s == len    = return d
            | predi v     = unsafeWrite mvec d v >> loop (d+1) (s+1)
            | otherwise   = loop d (s+1)
          where
            v = getVec s

freezeUntilIndex :: PrimMonad prim => MVector ty (PrimState prim) -> Int -> prim (Vector ty)
freezeUntilIndex mvec d = do
    m <- new d
    copyAt m 0 mvec 0 d
    unsafeFreeze m

reverse :: Vector ty -> Vector ty
reverse a = create len toEnd
  where
    len = length a
    toEnd i = unsafeIndex a (len - i - 1)
