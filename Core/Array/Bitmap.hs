{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
module Core.Array.Bitmap
    ( Bits
    , MutableBits
    ) where

import           Core.Array.Unboxed (UArray)
import qualified Core.Array.Unboxed as A
import           Core.Array.Unboxed.Mutable (MUArray)
--import qualified Core.Array.Unboxed.Mutable as MA
import           Core.Array.Common
import           Core.Internal.Base
import           Core.Primitive.Monad
import qualified Core.Collection as C
import           Core.Number
import           Data.Bits hiding (Bits)
--import           GHC.ST
--import qualified Prelude
import qualified Data.List

data Bits = Bits Int (UArray Word32)

data MutableBits st = MutableBits Int (MUArray Word32 st)

bitsPerTy :: Int
bitsPerTy = 32

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
    type Item Bits = Bool
    fromList = vFromList
    toList = vToList

instance C.InnerFunctor Bits where
    imap = map

instance C.Foldable Bits where
    foldl = foldl
    foldr = foldr
    foldl' = foldl'
    foldr' = foldr'

instance C.Sequential Bits where
    null = null
    take = take
    drop = drop
    splitAt = splitAt
    splitOn = splitOn
    break = break
    span = span
    filter = filter
    reverse = reverse
    snoc = snoc
    cons = cons
    find = find
    sortBy = sortBy
    length = length
    singleton = fromList . (:[])

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

bitmapIndex :: Int -> (Int, Int)
bitmapIndex !i = i `divMod` bitsPerTy
{-# INLINE bitmapIndex #-}

-- return the index in word32 quantity and mask to a bit in a bitmap
bitmapAddr :: Int -> (# Int , Word #)
bitmapAddr !i = (# idx, mask #)
  where (!idx, !bit) = bitmapIndex i
        !mask = case bit of
                    0  -> 0x1
                    1  -> 0x2
                    2  -> 0x4
                    3  -> 0x8
                    4  -> 0x10
                    5  -> 0x20
                    6  -> 0x40
                    7  -> 0x80
                    8  -> 0x100
                    9  -> 0x200
                    10 -> 0x400
                    11 -> 0x800
                    12 -> 0x1000
                    13 -> 0x2000
                    14 -> 0x4000
                    15 -> 0x8000
                    16 -> 0x10000
                    17 -> 0x20000
                    18 -> 0x40000
                    19 -> 0x80000
                    20 -> 0x100000
                    21 -> 0x200000
                    22 -> 0x400000
                    23 -> 0x800000
                    24 -> 0x1000000
                    25 -> 0x2000000
                    26 -> 0x4000000
                    27 -> 0x8000000
                    28 -> 0x10000000
                    29 -> 0x20000000
                    30 -> 0x40000000
                    _  -> 0x80000000

thaw :: PrimMonad prim => Bits -> prim (MutableBits (PrimState prim))
thaw = undefined

freeze :: PrimMonad prim => MutableBits (PrimState prim) -> prim Bits
freeze = undefined

unsafeThaw :: PrimMonad prim => Bits -> prim (MutableBits (PrimState prim))
unsafeThaw = undefined

unsafeFreeze :: PrimMonad prim => MutableBits (PrimState prim) -> prim Bits
unsafeFreeze = undefined

unsafeWrite :: MutableBits (PrimState prim) -> Int -> Bool -> prim ()
unsafeWrite = undefined

unsafeRead :: MutableBits (PrimState prim) -> Int -> prim Bool
unsafeRead = undefined

write :: MutableBits (PrimState prim) -> Int -> Bool -> prim ()
write = undefined

read :: MutableBits (PrimState prim) -> Int -> prim Bool
read = undefined

-- | Return the element at a specific index from a Bits.
--
-- If the index @n is out of bounds, an error is raised.
index :: Bits -> Int -> Bool
index array n
    | n < 0 || n >= len = throw (OutOfBound OOB_Index n len)
    | otherwise         = unsafeIndex array n
  where len = length array
{-# INLINE index #-}

-- | Return the element at a specific index from an array without bounds checking.
--
-- Reading from invalid memory can return unpredictable and invalid values.
-- use 'index' if unsure.
unsafeIndex :: Bits -> Int -> Bool
unsafeIndex (Bits _ ba) n =
    let (idx, bitIdx) = bitmapIndex n
     in testBit (A.unsafeIndex ba idx) bitIdx

{-# INLINE unsafeIndex #-}

-----------------------------------------------------------------------
-- higher level collection implementation
-----------------------------------------------------------------------
length :: Bits -> Int
length (Bits len _) = len

empty :: Bits
empty = Bits 0 A.empty

-- | make an array from a list of elements.
vFromList :: [Bool] -> Bits
vFromList l = undefined

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
--
-- TODO completely non optimized
append :: Bits -> Bits -> Bits
append a b = fromList $ toList a `mappend` toList b

-- TODO completely non optimized
concat :: [Bits] -> Bits
concat l = fromList $ mconcat $ fmap toList l

-- | update an array by creating a new array with the updates.
--
-- the operation copy the previous array, modify it in place, then freeze it.
update :: Bits
       -> [(Int, Bool)]
       -> Bits
update array modifiers = undefined

unsafeUpdate :: Bits
             -> [(Int, Bool)]
             -> Bits
unsafeUpdate array modifiers = undefined

null :: Bits -> Bool
null (Bits nbBits _) = nbBits == 0

take :: Int -> Bits -> Bits
take nbElems bits@(Bits nbBits ba)
    | nbElems <= 0     = empty
    | nbElems >= nbBits = bits
    | otherwise        = Bits nbElems ba -- TODO : although it work right now, take on the underlaying ba too

drop :: Int -> Bits -> Bits
drop nbElems bits@(Bits nbBits ba)
    | nbElems <= 0      = bits
    | nbElems >= nbBits = empty
    | otherwise         = undefined -- TODO: decide if we have drop easy by having a bit offset in the data structure
                                    -- or if we need to shift stuff around making all the indexing slighlty more complicated

splitAt :: Int -> Bits -> (Bits, Bits)
splitAt n v = (take n v, drop n v)

splitOn :: (ty -> Bool) -> Bits -> [Bits]
splitOn _ _ = undefined

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
map _ _ = undefined

mapIndex :: (Int -> Bool -> Bool) -> Bool -> Bool
mapIndex _ _ = undefined

cons :: Bool -> Bits -> Bits
cons _ _ = undefined

snoc :: Bits -> Bool -> Bits
snoc _ _ = undefined

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
sortBy _ _ = undefined

filter :: (Bool -> Bool) -> Bits -> Bits
filter predicate vec = vFromList $ Data.List.filter predicate $ vToList vec

reverse :: Bits -> Bits
reverse _ = undefined

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

foldr' :: (Bool -> a -> a) -> a -> Bits -> a
foldr' = foldr

foldl' :: (a -> Bool -> a) -> a -> Bits -> a
foldl' f initialAcc vec = loop 0 initialAcc
  where
    len = length vec
    loop i !acc
        | i == len  = acc
        | otherwise = loop (i+1) (f acc (unsafeIndex vec i))
