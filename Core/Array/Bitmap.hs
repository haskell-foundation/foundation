module Core.Array.Bitmap
    ( Bits
    , MutableBits
    ) where

import           Core.Array.Unboxed
import           Core.Internal.Base
import qualified Core.Collection as C

data Bits = Bits Int (UVector Word32)

data MutableBits st = MutableBits Int (MUVector Word32 st)

instance Show Bits where
    show v = show (toList v)
instance Eq Bits where
    (==) = equal
instance Ord Bits where
    compare = vCompare
instance Monoid Bits where
    mempty  = empty
    mappend = append
    mconcat = concat

type instance C.Element Bits = Bool

instance IsList Bits where
    type Item Bits = ty
    fromList = vFromList
    toList = vToList

instance C.InnerFunctor Bits where
    imap = map

instance C.Foldable Bits where
    foldl = foldl
    foldr = foldr
    foldl' = foldl'
    foldr' = foldr'

instance C.SemiOrderedCollection Bits where
    snoc = snoc
    cons = cons
    find = find
    sortBy = sortBy
    length = length
    singleton = fromList . (:[])

instance C.OrderedCollection Bits where
    null = null
    take = take
    drop = drop
    splitAt = splitAt
    splitOn = splitOn
    break = break
    span = span
    filter = filter
    reverse = reverse

instance C.IndexedCollection Bits where
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

instance C.MutableCollection MutableBits where
    type Collection MutableBits = Bits
    type MutableKey MutableBits = Int
    type MutableValue MutableBits = Bool

    thaw = thaw
    freeze = freeze
    unsafeThaw = unsafeThaw
    unsafeFreeze = unsafeFreeze

    mutUnsafeWrite = unsafeWrite
    mutUnsafeRead = unsafeRead
    mutWrite = write
    mutRead = read

{-
-- return the index and mask to a bit in a bitmap
bitmapAddr :: Int# -> (# Int# , Word# #)
bitmapAddr !i = (# idx, mask #)
  where (# !idx, !bit #) = compatQuotRemInt# i 4#
        !mask = case bit of
                    0#  -> 0x1##
                    1#  -> 0x2##
                    2#  -> 0x4##
                    3#  -> 0x8##
                    4#  -> 0x10##
                    5#  -> 0x20##
                    6#  -> 0x40##
                    7#  -> 0x80##
                    8#  -> 0x100##
                    9#  -> 0x200##
                    10# -> 0x400##
                    11# -> 0x800##
                    12# -> 0x1000##
                    13# -> 0x2000##
                    14# -> 0x4000##
                    15# -> 0x8000##
                    16# -> 0x10000##
                    17# -> 0x20000##
                    18# -> 0x40000##
                    19# -> 0x80000##
                    20# -> 0x100000##
                    21# -> 0x200000##
                    22# -> 0x400000##
                    23# -> 0x800000##
                    24# -> 0x1000000##
                    25# -> 0x2000000##
                    26# -> 0x4000000##
                    27# -> 0x8000000##
                    28# -> 0x10000000##
                    29# -> 0x20000000##
                    30# -> 0x40000000##
                    _   -> 0x80000000##

instance PrimType Bool where
    sizeInBytes _ = 1
    {-# INLINE sizeInBytes #-}
    primBaIndex ba (I# n) =
         bool# (0# /=# word2Int# (and# v mask))
      where (# idx, mask #) = bitmapAddr n
            !v = indexWord32Array# ba idx
    {-# INLINE primBaIndex #-}
    primMbaRead mba (I# n) = primitive $ \s1 ->
        case readWord32Array# mba idx s1 of
            (# s2, v #) -> (# s2, bool# (word2Int# (and# v mask) ==# 0#) #)
      where (# !idx, !mask #) = bitmapAddr n
    {-# INLINE primMbaRead #-}
    primMbaWrite mba (I# n) setValue = primitive $ \s1 ->
        case readWord32Array# mba idx s1 of
            (# s2, v #) -> (# writeWord32Array# mba idx (newVal v) s2, () #)
      where (# !idx, !mask #) = bitmapAddr n
            newVal v
                | setValue  = or# v mask
                | otherwise = and# v (not# mask)
    {-# INLINE primMbaWrite #-}
    primAddrIndex addr (I# n) =
         bool# (0# /=# word2Int# (and# v mask))
      where (# idx, mask #) = bitmapAddr n
            !v = indexWord32OffAddr# addr idx
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (I# n) = primitive $ \s1 ->
        case readWord32OffAddr# addr idx s1 of
            (# s2, v #) -> (# s2, bool# (word2Int# (and# v mask) ==# 0#) #)
      where (# !idx, !mask #) = bitmapAddr n
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (I# n) setValue = primitive $ \s1 ->
        case readWord32OffAddr# addr idx s1 of
            (# s2, v #) -> (# writeWord32OffAddr# addr idx (newVal v) s2, () #)
      where (# !idx, !mask #) = bitmapAddr n
            newVal v
                | setValue  = or# v mask
                | otherwise = and# v (not# mask)
    {-# INLINE primAddrWrite #-}
-}

-----------------------------------------------------------------------
-- higher level collection implementation
-----------------------------------------------------------------------

empty :: Bits
empty = runST (new 0 >>= unsafeFreeze)

-- | make an array from a list of elements.
vFromList :: [Bool] -> Bits
vFromList l = runST $ do
    ma <- new len
    iter 0 l $ \i x -> unsafeWrite ma i x
    unsafeFreeze ma
  where len = C.length l
        iter _ [] _ = return ()
        iter i (x:xs) z = z i x >> iter (i+1) xs z

-- | transform an array to a list.
vToList :: Bits -> [Bool]
vToList a = loop 0
  where len = length a
        loop i | i == len  = []
               | otherwise = unsafeIndex a i : loop (i+1)

-- | Check if two vectors are identical
equal :: Bits -> Bits -> Bool
equal a b
    | la /= lb  = False
    | otherwise = loop 0
  where
    !la = length a
    !lb = length b
    loop n | n == la    = True
           | otherwise = (unsafeIndex a n == unsafeIndex b n) && loop (n+1)

-- | Compare 2 vectors
vCompare :: Bits -> Bits -> Ordering
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
append :: Bits -> Bits -> Bits
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

concat :: [Bits] -> Bits
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
update :: Bits
       -> [(Int, Bool)]
       -> Bits
update array modifiers = runST (thaw array >>= doUpdate modifiers)
  where doUpdate l ma = loop l
          where loop []         = unsafeFreeze ma
                loop ((i,v):xs) = write ma i v >> loop xs
                {-# INLINE loop #-}
        {-# INLINE doUpdate #-}

unsafeUpdate :: PrimType ty
             => Bits
             -> [(Int, Bool)]
             -> Bits
unsafeUpdate array modifiers = runST (thaw array >>= doUpdate modifiers)
  where doUpdate l ma = loop l
          where loop []         = unsafeFreeze ma
                loop ((i,v):xs) = unsafeWrite ma i v >> loop xs
                {-# INLINE loop #-}
        {-# INLINE doUpdate #-}

null :: Bits -> Bool
null (Bits nbBits _) = b == 0

take :: Int -> Bits -> Bits
take nbElems v
    | nbElems <= 0 = empty
    | otherwise    = runST $ do
        muv <- new n
        copyAtRO muv 0 v 0 n
        unsafeFreeze muv
  where
    n = min nbElems (length v)

drop :: Int -> Bits -> Bits
drop nbElems v
    | nbElems <= 0 = v
    | otherwise    = runST $ do
        muv <- new n
        copyAtRO muv 0 v offset n
        unsafeFreeze muv
  where
    offset = min nbElems (length v)
    n = length v - offset

splitAt :: Int -> Bits -> (Bits, Bits)
splitAt n v = (take n v, drop n v)

splitOn :: (ty -> Bool) -> Bits -> [Bits]
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

sub :: PrimMonad prim => Bits -> Int -> Int -> prim Bits
sub vec@(A ba) startIdx expectedEndIdx
    | startIdx == endIdx     = return empty
    | startIdx >= length vec = return empty
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
    bytes = sizeInBytesOfContent vec

break :: (Bool -> Bool) -> Bits -> (Bits, Bits)
break predicate v = findBreak 0
  where
    findBreak i
        | i == length v = (v, empty)
        | otherwise     =
            if predicate (unsafeIndex v i)
                then splitAt i v
                else findBreak (i+1)

span :: (Bool -> Bool) -> Bits -> (Bits, Bits)
span p = break (not . p)

map :: (Bool -> Bool) -> Bits -> Bits
map f a = create (length a) (\i -> f $ unsafeIndex a i)

mapIndex :: (Int -> Bool -> Bool) -> Bool -> Bool
mapIndex f a = create (length a) (\i -> f i $ unsafeIndex a i)

cons :: Bool -> Bits -> Bits
cons e vec@(A ba) = runST $ do
    muv@(MA mba) <- new (len + 1)
    -- bench with "copyAtRO muv 1 vec 0 len"
    primCopyFreezedBytesOffset mba bytes ba (len# *# bytes)
    unsafeWrite muv 0 e
    unsafeFreeze muv
  where
    !(I# bytes) = sizeInBytesOfContent vec
    !len@(I# len#) = length vec

snoc :: Bits -> Bool -> Bits
snoc vec@(A ba) e = runST $ do
    muv@(MA mba) <- new (len + 1)
    primCopyFreezedBytes mba ba
    unsafeWrite muv len e
    unsafeFreeze muv
  where
    !len = length vec

find :: (Bool -> Bool) -> Bits -> Maybe Bool
find predicate vec = loop 0
  where
    !len = length vec
    loop i
        | i == len  = Nothing
        | otherwise =
            let e = unsafeIndex vec i
             in if predicate e then Just e else loop (i+1)

sortBy :: (Bool -> Bool -> Ordering) -> Bits -> Bits
sortBy xford vec = runST (thaw vec >>= doSort xford)
  where
    len = length vec
    doSort :: (PrimType ty, PrimMonad prim) => (ty -> ty -> Ordering) -> MBits (PrimState prim) -> prim (Bits)
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

filter :: (Bool -> Bool) -> Bits -> Bits
filter predicate vec = vFromList $ Data.List.filter predicate $ vToList vec

reverse :: Bits -> Bits
reverse a = create len toEnd
  where
    len = length a
    toEnd i = unsafeIndex a (len - i - 1)

foldl :: (a -> Bool -> a) -> a -> Bits -> a
foldl f initialAcc vec = loop 0 initialAcc
  where
    len = length vec
    loop i acc
        | i == len  = acc
        | otherwise = loop (i+1) (f acc (unsafeIndex vec i))

foldr :: (Bool -> a -> a) -> a -> Bits -> a
foldr f initialAcc vec = loop 0
  where
    len = length vec
    loop i
        | i == len  = initialAcc
        | otherwise = unsafeIndex vec i `f` loop (i+1)

foldl' :: (a -> Bool -> a) -> a -> Bits -> a
foldl' f initialAcc vec = loop 0 initialAcc
  where
    len = length vec
    loop i !acc
        | i == len  = acc
        | otherwise = loop (i+1) (f acc (unsafeIndex vec i))
