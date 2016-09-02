-- |
-- Module      : Foundation.Array.Boxed
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
module Foundation.Array.Boxed
    ( Array
    , MArray
    , copy
    ) where

import           GHC.Prim
import           GHC.Types
import           GHC.ST
import           Foundation.Number
import           Foundation.Internal.Base
import           Foundation.Internal.Types
import           Foundation.Primitive.Types
import           Foundation.Primitive.Monad
import           Foundation.Array.Common
import qualified Foundation.Collection as C
import qualified Prelude

-- | Array of a
data Array a = Array {-# UNPACK #-} !(Offset a)
                     {-# UNPACK #-} !(Size a)
                                    (Array# a)
    deriving (Typeable)

-- | Mutable Array of a
data MArray a st = MArray {-# UNPACK #-} !(Offset a)
                          {-# UNPACK #-} !(Size a)
                                         (MutableArray# st a)
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
        !len = Size $ min (C.length a) (C.length b)
        go _  _  _ []       _        = return ()
        go _  _  _ _        []       = return ()
        go mv i f' (a':as') (b':bs') = do
            write mv i (f' a' b')
            go mv (i + 1) f' as' bs'

instance C.BoxedZippable (Array ty)

-- | return the numbers of elements in a mutable array
mutableLength :: MArray ty st -> Int
mutableLength (MArray _ (Size len) _) = len
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
unsafeIndex (Array (Offset (I# ofs)) _ a) (I# i) = let (# v #) = indexArray# a (ofs +# i) in v
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
unsafeRead (MArray (Offset (I# ofs)) _ ma) (I# i) = primitive $ \s1 -> readArray# ma (ofs +# i) s1
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
unsafeWrite (MArray (Offset (I# ofs)) _ ma) (I# i) v =
    primitive $ \s1 -> let !s2 = writeArray# ma (ofs +# i) v s1 in (# s2, () #)
{-# INLINE unsafeWrite #-}

-- | Freeze a mutable array into an array.
--
-- the MArray must not be changed after freezing.
unsafeFreeze :: PrimMonad prim => MArray ty (PrimState prim) -> prim (Array ty)
unsafeFreeze (MArray ofs sz ma) = primitive $ \s1 ->
    case unsafeFreezeArray# ma s1 of
        (# s2, a #) -> (# s2, Array ofs sz a #)
{-# INLINE unsafeFreeze #-}

-- | Thaw an immutable array.
--
-- The Array must not be used after thawing.
unsafeThaw :: PrimMonad prim => Array ty -> prim (MArray ty (PrimState prim))
unsafeThaw (Array ofs sz a) = primitive $ \st -> (# st, MArray ofs sz (unsafeCoerce# a) #)
{-# INLINE unsafeThaw #-}

-- | Thaw an array to a mutable array.
--
-- the array is not modified, instead a new mutable array is created
-- and every values is copied, before returning the mutable array.
thaw :: PrimMonad prim => Array ty -> prim (MArray ty (PrimState prim))
thaw array = do
    m <- new (lengthSize array)
    unsafeCopyAtRO m (Offset 0) array (Offset 0) (lengthSize array)
    return m
{-# INLINE thaw #-}

freeze :: PrimMonad prim => MArray ty (PrimState prim) -> prim (Array ty)
freeze marray = do
    m <- new sz
    copyAt m (Offset 0) marray (Offset 0) sz
    unsafeFreeze m
  where
    sz = Size $ mutableLength marray

-- | Copy the element to a new element array
copy :: Array ty -> Array ty
copy a = runST (unsafeThaw a >>= freeze)

-- | Copy a number of elements from an array to another array with offsets
copyAt :: PrimMonad prim
       => MArray ty (PrimState prim) -- ^ destination array
       -> Offset ty                  -- ^ offset at destination
       -> MArray ty (PrimState prim) -- ^ source array
       -> Offset ty                  -- ^ offset at source
       -> Size ty                    -- ^ number of elements to copy
       -> prim ()
copyAt dst od src os n = loop od os
  where !endIndex = os `offsetPlusE` n
        loop (Offset d) s@(Offset i)
            | s == endIndex = return ()
            | otherwise     = unsafeRead src i >>= unsafeWrite dst d >> loop (Offset $ d+1) (Offset $ i+1)

-- | Copy @n@ sequential elements from the specified offset in a source array
--   to the specified position in a destination array.
--
--   This function does not check bounds. Accessing invalid memory can return
--   unpredictable and invalid values.
unsafeCopyAtRO :: PrimMonad prim
               => MArray ty (PrimState prim) -- ^ destination array
               -> Offset ty                  -- ^ offset at destination
               -> Array ty                   -- ^ source array
               -> Offset ty                  -- ^ offset at source
               -> Size ty                    -- ^ number of elements to copy
               -> prim ()
unsafeCopyAtRO dst od src os n = loop od os
  where !endIndex = os `offsetPlusE` n
        loop (Offset d) s@(Offset i)
            | s == endIndex = return ()
            | otherwise     = unsafeWrite dst d (unsafeIndex src i) >> loop (Offset $ d+1) (Offset $ i+1)

-- | Allocate a new array with a fill function that has access to the elements of
--   the source array.
unsafeCopyFrom :: Array ty -- ^ Source array
               -> Size ty  -- ^ Length of the destination array
               -> (Array ty -> Offset ty -> MArray ty s -> ST s ())
               -- ^ Function called for each element in the source array
               -> ST s (Array ty) -- ^ Returns the filled new array
unsafeCopyFrom v' newLen f = new newLen >>= fill (Offset 0) f >>= unsafeFreeze
  where len = lengthSize v'
        endIdx = Offset 0 `offsetPlusE` len
        fill i f' r'
            | i == endIdx = return r'
            | otherwise   = do f' v' i r'
                               fill (i + Offset 1) f' r'

-- | Create a new mutable array of size @n.
--
-- all the cells are uninitialized and could contains invalid values.
--
-- All mutable arrays are allocated on a 64 bits aligned addresses
-- and always contains a number of bytes multiples of 64 bits.
new :: PrimMonad prim => Size ty -> prim (MArray ty (PrimState prim))
new sz@(Size (I# n)) = primitive $ \s1 ->
    case newArray# n (error "vector: internal error uninitialized vector") s1 of
        (# s2, ma #) -> (# s2, MArray (Offset 0) sz ma #)

-- | Create a new array of size @n by settings each cells through the
-- function @f.
create :: Int         -- ^ the size of the array
       -> (Int -> ty) -- ^ the function that set the value at the index
       -> Array ty   -- ^ the array created
create n initializer = runST (new (Size n) >>= iter initializer)
  where
    iter :: PrimMonad prim => (Int -> ty) -> MArray ty (PrimState prim) -> prim (Array ty)
    iter f ma = loop (Offset 0)
      where
        !end = Offset 0 `offsetPlusE` Size n
        loop s@(Offset i)
            | s == end  = unsafeFreeze ma
            | otherwise = unsafeWrite ma i (f i) >> loop (Offset $ i+1)
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
length (Array _ (Size len) _) = len

lengthSize :: Array a -> Size a
lengthSize (Array _ sz _) = sz

vFromList :: [a] -> Array a
vFromList l = runST (new len >>= loop 0 l)
  where
    len = Size $ C.length l
    loop _ []     ma = unsafeFreeze ma
    loop i (x:xs) ma = unsafeWrite ma i x >> loop (i+1) xs ma

vToList :: Array a -> [a]
vToList v = fmap (unsafeIndex v) [0..(length v - 1)]

-- | Append 2 arrays together by creating a new bigger array
append :: Array ty -> Array ty -> Array ty
append a b = runST $ do
    r  <- new (la+lb)
    ma <- unsafeThaw a
    mb <- unsafeThaw b
    copyAt r (Offset 0) ma (Offset 0) la
    copyAt r (sizeAsOffset la) mb (Offset 0) lb
    unsafeFreeze r
  where la = lengthSize a
        lb = lengthSize b

concat :: [Array ty] -> Array ty
concat l = runST $ do
    r <- new (Size $ Prelude.sum $ fmap length l)
    loop r (Offset 0) l
    unsafeFreeze r
  where loop _ _ []     = return ()
        loop r i (x:xs) = do
            mx <- unsafeThaw x
            copyAt r i mx (Offset 0) lx
            loop r (i `offsetPlusE` lx) xs
          where lx = lengthSize x

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
onNewArray len@(I# len#) f = primitive $ \st -> do
    case newArray# len# (error "onArray") st of { (# st2, mv #) ->
    case f mv st2                            of { st3           ->
    case unsafeFreezeArray# mv st3           of { (# st4, a #)  ->
        (# st4, Array (Offset 0) (Size len) a #) }}}

-----------------------------------------------------------------------


null :: Array ty -> Bool
null = (==) 0 . length

take ::  Int -> Array ty -> Array ty
take nbElems a@(Array start len arr)
    | nbElems <= 0 = empty
    | n == len     = a
    | otherwise    = Array start n arr
  where
    n = min (Size nbElems) len

drop ::  Int -> Array ty -> Array ty
drop nbElems a@(Array start len arr)
    | nbElems <= 0 = a
    | n == len     = empty
    | otherwise    = Array (start `offsetPlusE` n) (len - n) arr
  where
    n = min (Size nbElems) len

splitAt ::  Int -> Array ty -> (Array ty, Array ty)
splitAt nbElems a@(Array start len arr)
    | nbElems <= 0 = (empty, a)
    | n == len     = (a, empty)
    | otherwise    =
        (Array start n arr, Array (start `offsetPlusE` n) (len - n) arr)
  where
    n = min (Size nbElems) len

revTake :: Int -> Array ty -> Array ty
revTake nbElems v = drop (length v - nbElems) v

revDrop :: Int -> Array ty -> Array ty
revDrop nbElems v = take (length v - nbElems) v

revSplitAt :: Int -> Array ty -> (Array ty, Array ty)
revSplitAt n v = (drop idx v, take idx v)
  where idx = length v - n

splitOn ::  (ty -> Bool) -> Array ty -> [Array ty]
splitOn predicate vec
    | len == Size 0 = []
    | otherwise     = loop (Offset 0) (Offset 0)
  where
    !len = lengthSize vec
    !endIdx = Offset 0 `offsetPlusE` len
    loop prevIdx idx@(Offset i)
        | idx == endIdx = [sub vec prevIdx idx]
        | otherwise     =
            let e = unsafeIndex vec i
                idx' = idx + Offset 1
             in if predicate e
                    then sub vec prevIdx idx : loop idx' idx'
                    else loop prevIdx idx'

sub :: Array ty -> Offset ty -> Offset ty -> Array ty
sub (Array start len a) startIdx expectedEndIdx
    | startIdx == endIdx           = empty
    | otherwise                    = Array (start + startIdx) newLen a
  where
    newLen = endIdx - startIdx
    endIdx = min expectedEndIdx (sizeAsOffset len)

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
    | len <= Size 1 = v
    | otherwise     = runST $ unsafeCopyFrom v ((len + len) - Size 1) (go (Offset 0 `offsetPlusE` (len - Size 1)) sep)
  where len = lengthSize v
        -- terminate 1 before the end

        go :: Offset ty -> ty -> Array ty -> Offset ty -> MArray ty s -> ST s ()
        go endI sep' oldV oldI@(Offset oi) newV
            | oldI == endI = unsafeWrite newV dst e
            | otherwise    = do
                unsafeWrite newV dst e
                unsafeWrite newV (dst + 1) sep'
          where
            e = unsafeIndex oldV oi
            (Offset dst) = oldI + oldI

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
    | len == Size 0 = C.singleton e
    | otherwise     = runST $ do
        mv <- new (len + Size 1)
        unsafeWrite mv 0 e
        unsafeCopyAtRO mv (Offset 1) vec (Offset 0) len
        unsafeFreeze mv
  where
    !len = lengthSize vec

snoc ::  Array ty -> ty -> Array ty
snoc vec e
    | len == Size 0 = C.singleton e
    | otherwise     = runST $ do
        mv <- new (len + Size 1)
        unsafeCopyAtRO mv (Offset 0) vec (Offset 0) len
        unsafeWrite mv lastI e
        unsafeFreeze mv
  where
    !len@(Size lastI) = lengthSize vec

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
    !len = lengthSize vec
    !end = Offset 0 `offsetPlusE` len
    copyFilterFreeze :: PrimMonad prim => (ty -> Bool) -> (Int -> ty) -> MArray ty (PrimState prim) -> prim (Array ty)
    copyFilterFreeze predi getVec mvec = loop (Offset 0) (Offset 0) >>= freezeUntilIndex mvec
      where
        loop d@(Offset di) s@(Offset si)
            | s == end    = return d
            | predi v     = unsafeWrite mvec di v >> loop (d+Offset 1) (s+Offset 1)
            | otherwise   = loop d (s+Offset 1)
          where
            v = getVec si

freezeUntilIndex :: PrimMonad prim => MArray ty (PrimState prim) -> Offset ty -> prim (Array ty)
freezeUntilIndex mvec d = do
    m <- new (offsetAsSize d)
    copyAt m (Offset 0) mvec (Offset 0) (offsetAsSize d)
    unsafeFreeze m

reverse :: Array ty -> Array ty
reverse a = create len toEnd
  where
    len = length a
    toEnd i = unsafeIndex a (len - i - 1)
