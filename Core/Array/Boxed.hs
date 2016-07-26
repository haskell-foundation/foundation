-- |
-- Module      : Core.Array.Boxed
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
module Core.Array.Boxed
    ( Array
    , MArray
    ) where

import           GHC.Prim
import           GHC.Types
import           GHC.ST
import           Core.Internal.Base
import           Core.Primitive.Monad
import           Core.Array.Common
import qualified Core.Collection as C
import qualified Prelude
import           Prelude ((-), (+), (*))

-- | Array of a
data Array a = Array (Array# a)
    deriving (Typeable)

-- | Mutable Array of a
data MArray a st = MArray (MutableArray# st a)
    deriving (Typeable)

instance Functor Array where
    fmap = map

instance Monoid (Array a) where
    mempty  = empty
    mappend = append
    mconcat = concat

instance Show a => Show (Array a) where
    show v = show (toList v)

instance Eq a => Eq (Array a) where
    (==) = equal
instance Ord a => Ord (Array a) where
    compare = vCompare

type instance C.Element (Array ty) = ty

instance IsList (Array ty) where
    type Item (Array ty) = ty
    fromList = vFromList
    toList = vToList

instance C.InnerFunctor (Array ty)

instance C.Sequential (Array ty) where
    null = null
    take = take
    drop = drop
    splitAt = splitAt
    revTake = revTake
    revDrop = revDrop
    revSplitAt = revSplitAt
    splitOn = splitOn
    break = break
    intersperse = intersperse
    span = span
    reverse = reverse
    filter = filter
    unsnoc = unsnoc
    uncons = uncons
    snoc = snoc
    cons = cons
    find = find
    sortBy = sortBy
    length = length
    singleton = fromList . (:[])

instance C.MutableCollection (MArray ty) where
    type Collection (MArray ty) = Array ty
    type MutableKey (MArray ty) = Int
    type MutableValue (MArray ty) = ty

    thaw = thaw
    freeze = freeze
    unsafeThaw = unsafeThaw
    unsafeFreeze = unsafeFreeze

    mutUnsafeWrite = unsafeWrite
    mutUnsafeRead = unsafeRead
    mutWrite = write
    mutRead = read

instance C.IndexedCollection (Array ty) where
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

instance C.Zippable (Array ty) where
    -- TODO Use an array builder once available
    zipWith f a b = runST $ do
        mv <- new len
        go mv 0 f (toList a) (toList b)
        unsafeFreeze mv
      where
        !len = min (C.length a) (C.length b)
        go _  _  _ []       _        = return ()
        go _  _  _ _        []       = return ()
        go mv i f' (a':as') (b':bs') = do
            write mv i (f' a' b')
            go mv (i + 1) f' as' bs'

-- | return the numbers of elements in a mutable array
mutableLength :: MArray ty st -> Int
mutableLength (MArray ma) = I# (sizeofMutableArray# ma)
{-# INLINE mutableLength #-}

-- | Return the element at a specific index from an array.
--
-- If the index @n is out of bounds, an error is raised.
index :: Array ty -> Int -> ty
index array n
    | n < 0 || n >= len = throw (OutOfBound OOB_Index n len)
    | otherwise         = unsafeIndex array n
  where len = length array
{-# INLINE index #-}

-- | Return the element at a specific index from an array without bounds checking.
--
-- Reading from invalid memory can return unpredictable and invalid values.
-- use 'index' if unsure.
unsafeIndex :: Array ty -> Int -> ty
unsafeIndex (Array a) (I# n) = let (# v #) = indexArray# a n in v
{-# INLINE unsafeIndex #-}

-- | read a cell in a mutable array.
--
-- If the index is out of bounds, an error is raised.
read :: PrimMonad prim => MArray ty (PrimState prim) -> Int -> prim ty
read array n
    | n < 0 || n >= len = primThrow (OutOfBound OOB_Read n len)
    | otherwise         = unsafeRead array n
  where len = mutableLength array
{-# INLINE read #-}

-- | read from a cell in a mutable array without bounds checking.
--
-- Reading from invalid memory can return unpredictable and invalid values.
-- use 'read' if unsure.
unsafeRead :: PrimMonad prim => MArray ty (PrimState prim) -> Int -> prim ty
unsafeRead (MArray ma) (I# i) = primitive $ \s1 -> readArray# ma i s1
--readArray# :: MutableArray# s a -> Int# -> State# s -> (#State# s, a#)
{-# INLINE unsafeRead #-}

-- | Write to a cell in a mutable array.
--
-- If the index is out of bounds, an error is raised.
write :: PrimMonad prim => MArray ty (PrimState prim) -> Int -> ty -> prim ()
write array n val
    | n < 0 || n >= len = primThrow (OutOfBound OOB_Write n len)
    | otherwise         = unsafeWrite array n val
  where len = mutableLength array
{-# INLINE write #-}

-- | write to a cell in a mutable array without bounds checking.
--
-- Writing with invalid bounds will corrupt memory and your program will
-- become unreliable. use 'write' if unsure.
unsafeWrite :: PrimMonad prim => MArray ty (PrimState prim) -> Int -> ty -> prim ()
unsafeWrite (MArray ma) (I# i) v = primitive $ \s1 -> let !s2 = writeArray# ma i v s1 in (# s2, () #)
{-# INLINE unsafeWrite #-}

-- | Freeze a mutable array into an array.
--
-- the MArray must not be changed after freezing.
unsafeFreeze :: PrimMonad prim => MArray ty (PrimState prim) -> prim (Array ty)
unsafeFreeze (MArray ma) = primitive $ \s1 ->
    case unsafeFreezeArray# ma s1 of
        (# s2, a #) -> (# s2, Array a #)
{-# INLINE unsafeFreeze #-}

-- | Thaw an immutable array.
--
-- The Array must not be used after thawing.
unsafeThaw :: PrimMonad prim => Array ty -> prim (MArray ty (PrimState prim))
unsafeThaw (Array a) = primitive $ \st -> (# st, MArray (unsafeCoerce# a) #)
{-# INLINE unsafeThaw #-}

-- | Thaw an array to a mutable array.
--
-- the array is not modified, instead a new mutable array is created
-- and every values is copied, before returning the mutable array.
thaw :: PrimMonad prim => Array ty -> prim (MArray ty (PrimState prim))
thaw array = do
    m <- new (length array)
    unsafeCopyAtRO m 0 array 0 (length array)
    return m
{-# INLINE thaw #-}

freeze :: PrimMonad prim => MArray ty (PrimState prim) -> prim (Array ty)
freeze = undefined

-- | Copy a number of elements from an array to another array with offsets
copyAt :: PrimMonad prim
       => MArray ty (PrimState prim) -- ^ destination array
       -> Int                -- ^ offset at destination
       -> MArray ty (PrimState prim) -- ^ source array
       -> Int                -- ^ offset at source
       -> Int                -- ^ number of elements to copy
       -> prim ()
copyAt dst od src os n = loop od os
  where endIndex = os + n
        loop d i
            | i == endIndex = return ()
            | otherwise     = unsafeRead src i >>= unsafeWrite dst d >> loop (d+1) (i+1)

-- | Copy @n@ sequential elements from the specified offset in a source array
--   to the specified position in a destination array.
--
--   This function does not check bounds. Accessing invalid memory can return
--   unpredictable and invalid values.
unsafeCopyAtRO :: PrimMonad prim
               => MArray ty (PrimState prim) -- ^ destination array
               -> Int                        -- ^ offset at destination
               -> Array ty                   -- ^ source array
               -> Int                        -- ^ offset at source
               -> Int                        -- ^ number of elements to copy
               -> prim ()
unsafeCopyAtRO dst od src os n = loop od os
  where endIndex = os + n
        loop d i
            | i == endIndex = return ()
            | otherwise     = unsafeWrite dst d (unsafeIndex src i) >> loop (d+1) (i+1)

-- | Allocate a new array with a fill function that has access to the elements of
--   the source array.
unsafeCopyFrom :: Array ty -- ^ Source array
               -> Int -- ^ Length of the destination array
               -> (Array ty -> Int -> MArray ty s -> ST s ())
               -- ^ Function called for each element in the source array
               -> ST s (Array ty) -- ^ Returns the filled new array
unsafeCopyFrom v' newLen f = new newLen >>= fill 0 f >>= unsafeFreeze
  where len = length v'
        fill i f' r'
            | i == len  = return r'
            | otherwise = do f' v' i r'
                             fill (i + 1) f' r'

-- | Create a new mutable array of size @n.
--
-- all the cells are uninitialized and could contains invalid values.
--
-- All mutable arrays are allocated on a 64 bits aligned addresses
-- and always contains a number of bytes multiples of 64 bits.
new :: PrimMonad prim => Int -> prim (MArray ty (PrimState prim))
new (I# n) = primitive $ \s1 ->
                case newArray# n (error "vector: internal error uninitialized vector") s1 of
                    (# s2, ma #) -> (# s2, MArray ma #)

-- | Create a new array of size @n by settings each cells through the
-- function @f.
create :: Int         -- ^ the size of the array
       -> (Int -> ty) -- ^ the function that set the value at the index
       -> Array ty   -- ^ the array created
create n initializer = runST (new n >>= iter initializer)
  where
    iter :: PrimMonad prim => (Int -> ty) -> MArray ty (PrimState prim) -> prim (Array ty)
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
equal :: Eq a => Array a -> Array a -> Bool
equal a b = (len == length b) && eachEqual 0
  where
    len = length a
    eachEqual !i
        | i == len                           = True
        | unsafeIndex a i /= unsafeIndex b i = False
        | otherwise                          = eachEqual (i+1)

vCompare :: Ord a => Array a -> Array a -> Ordering
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

empty :: Array a
empty = runST $ onNewArray 0 (\_ s -> s)

length :: Array a -> Int
length (Array a) = I# (sizeofArray# a)

vFromList :: [a] -> Array a
vFromList l = runST (new len >>= loop 0 l)
  where
    len = Prelude.length l
    loop _ []     ma = unsafeFreeze ma
    loop i (x:xs) ma = unsafeWrite ma i x >> loop (i+1) xs ma

vToList :: Array a -> [a]
vToList v = fmap (unsafeIndex v) [0..(Prelude.subtract 1 $ length v)]

-- | Append 2 arrays together by creating a new bigger array
append :: Array ty -> Array ty -> Array ty
append a b = runST $ do
    r  <- new (la+lb)
    ma <- unsafeThaw a
    mb <- unsafeThaw b
    copyAt r 0 ma 0 la
    copyAt r la mb 0 lb
    unsafeFreeze r
  where la = length a
        lb = length b

concat :: [Array ty] -> Array ty
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
       => Array a
       -> (MArray (PrimState m) a -> m ())
       -> m (Array a)
modify (Array a) f = primitive $ \st -> do
    case thawArray# a 0# (sizeofArray# a) st of
        (# st2, mv #) ->
            case internal_ (f $ MArray mv) st2 of
                st3 ->
                    case unsafeFreezeArray# mv st3 of
                        (# st4, a' #) -> (# st4, Array a' #)
-}

-----------------------------------------------------------------------
-- helpers

onNewArray :: PrimMonad m
           => Int
           -> (MutableArray# (PrimState m) a -> State# (PrimState m) -> State# (PrimState m))
           -> m (Array a)
onNewArray (I# len) f = primitive $ \st -> do
    case newArray# len (error "onArray") st of
        (# st2, mv #) ->
            case f mv st2 of
                st3 ->
                    case unsafeFreezeArray# mv st3 of
                        (# st4, a #) -> (# st4, Array a #)

-----------------------------------------------------------------------


null :: Array ty -> Bool
null = (==) 0 . length

take ::  Int -> Array ty -> Array ty
take nbElems v
    | nbElems <= 0 = empty
    | otherwise    = runST $ do
        muv <- new n
        unsafeCopyAtRO muv 0 v 0 n
        unsafeFreeze muv
  where
    n = min nbElems (length v)

drop ::  Int -> Array ty -> Array ty
drop nbElems v
    | nbElems <= 0 = v
    | otherwise    = runST $ do
        muv <- new n
        unsafeCopyAtRO muv 0 v offset n
        unsafeFreeze muv
  where
    offset = min nbElems (length v)
    n = length v - offset

splitAt ::  Int -> Array ty -> (Array ty, Array ty)
splitAt n v = (take n v, drop n v)

revTake :: Int -> Array ty -> Array ty
revTake nbElems v = drop (length v - nbElems) v

revDrop :: Int -> Array ty -> Array ty
revDrop nbElems v = take (length v - nbElems) v

revSplitAt :: Int -> Array ty -> (Array ty, Array ty)
revSplitAt n v = (drop idx v, take idx v)
  where idx = length v - n

splitOn ::  (ty -> Bool) -> Array ty -> [Array ty]
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

sub :: PrimMonad prim => Array ty -> Int -> Int -> prim (Array ty)
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

break ::  (ty -> Bool) -> Array ty -> (Array ty, Array ty)
break predicate v = findBreak 0
  where
    findBreak i
        | i == length v = (v, empty)
        | otherwise     =
            if predicate (unsafeIndex v i)
                then splitAt i v
                else findBreak (i+1)

intersperse :: ty -> Array ty -> Array ty
intersperse sep v
    | len <= 1  = v
    | otherwise = runST $ unsafeCopyFrom v (len * 2 - 1) (go sep)
  where len = length v
        go :: ty -> Array ty -> Int -> MArray ty s -> ST s ()
        go sep' oldV oldI newV
            | oldI == len - 1 = unsafeWrite newV newI e
            | otherwise       = do
                unsafeWrite newV newI e
                unsafeWrite newV (newI + 1) sep'
          where
            e = unsafeIndex oldV oldI
            newI = oldI * 2

span ::  (ty -> Bool) -> Array ty -> (Array ty, Array ty)
span p = break (not . p)

map :: (a -> b) -> Array a -> Array b
map f a = create (length a) (\i -> f $ unsafeIndex a i)

{-
mapIndex :: (Int -> a -> b) -> Array a -> Array b
mapIndex f a = create (length a) (\i -> f i $ unsafeIndex a i)
-}

cons ::  ty -> Array ty -> Array ty
cons e vec
    | len == 0  = C.singleton e
    | otherwise = runST $ do
        mv <- new (len + 1)
        unsafeWrite mv 0 e
        unsafeCopyAtRO mv 1 vec 0 len
        unsafeFreeze mv
  where
    !len = length vec

snoc ::  Array ty -> ty -> Array ty
snoc vec e
    | len == 0  = C.singleton e
    | otherwise = runST $ do
        mv <- new (len + 1)
        unsafeCopyAtRO mv 0 vec 0 len
        unsafeWrite mv len e
        unsafeFreeze mv
  where
    !len = length vec

uncons :: Array ty -> Maybe (ty, Array ty)
uncons vec
    | len == 0  = Nothing
    | otherwise = Just (unsafeIndex vec 0, drop 1 vec)
  where
    !len = length vec

unsnoc :: Array ty -> Maybe (Array ty, ty)
unsnoc vec
    | len == 0  = Nothing
    | otherwise = Just (take (len - 1) vec, unsafeIndex vec (len-1))
  where
    !len = length vec

find ::  (ty -> Bool) -> Array ty -> Maybe ty
find predicate vec = loop 0
  where
    !len = length vec
    loop i
        | i == len  = Nothing
        | otherwise =
            let e = unsafeIndex vec i
             in if predicate e then Just e else loop (i+1)

sortBy ::  (ty -> ty -> Ordering) -> Array ty -> Array ty
sortBy xford vec = runST (thaw vec >>= doSort xford)
  where
    len = length vec
    doSort :: PrimMonad prim => (ty -> ty -> Ordering) -> MArray ty (PrimState prim) -> prim (Array ty)
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

filter :: (ty -> Bool) -> Array ty -> Array ty
filter predicate vec = runST (new len >>= copyFilterFreeze predicate (unsafeIndex vec))
  where
    !len = length vec
    copyFilterFreeze :: PrimMonad prim => (ty -> Bool) -> (Int -> ty) -> MArray ty (PrimState prim) -> prim (Array ty)
    copyFilterFreeze predi getVec mvec = loop 0 0 >>= freezeUntilIndex mvec
      where
        loop d s
            | s == len    = return d
            | predi v     = unsafeWrite mvec d v >> loop (d+1) (s+1)
            | otherwise   = loop d (s+1)
          where
            v = getVec s

freezeUntilIndex :: PrimMonad prim => MArray ty (PrimState prim) -> Int -> prim (Array ty)
freezeUntilIndex mvec d = do
    m <- new d
    copyAt m 0 mvec 0 d
    unsafeFreeze m

reverse :: Array ty -> Array ty
reverse a = create len toEnd
  where
    len = length a
    toEnd i = unsafeIndex a (len - i - 1)
