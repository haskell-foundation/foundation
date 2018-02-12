-- |
-- Module      : Foundation.Array.Bitmap
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- A simple abstraction to a set of Bits (Bitmap)
--
-- Largely a placeholder for a more performant implementation,
-- most operation goes through the List representation (e.g. [Bool])
-- to conduct even the most trivial operation, leading to a lots of
-- unnecessary churn.
--
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Foundation.Array.Bitmap
    ( Bitmap
    , MutableBitmap
    , empty
    , append
    , concat
    , unsafeIndex
    , index
    , read
    , unsafeRead
    , write
    , unsafeWrite
    , snoc
    , cons
    ) where

import           Basement.UArray (UArray)
import qualified Basement.UArray as A
import           Basement.UArray.Mutable (MUArray)
import           Basement.Compat.Bifunctor (first, second, bimap)
import           Basement.Compat.Semigroup
import           Basement.Exception
import           Basement.Compat.Base
import           Basement.Types.OffsetSize
import           Basement.Monad
import           Basement.Bits (Bit)

import qualified Foundation.Collection as C
import           Foundation.Numerical
import           Data.Bits
import           Foundation.Bits
import           GHC.ST
import qualified Data.List

data Bitmap = Bitmap (CountOf Bit) (UArray Word32)
    deriving (Typeable)

data MutableBitmap st = MutableBitmap (CountOf Bit) (MUArray Word32 st)

bitsPerTy :: Int
bitsPerTy = 32

shiftPerTy :: Int
shiftPerTy = 5

maskPerTy :: Int
maskPerTy = 0x1f

instance Show Bitmap where
    show v = show (toList v)
instance Eq Bitmap where
    (==) = equal
instance Ord Bitmap where
    compare = vCompare
instance Semigroup Bitmap where
    (<>) = append
instance Monoid Bitmap where
    mempty  = empty
    mappend = append
    mconcat = concat

type instance C.Element Bitmap = Bit

instance IsList Bitmap where
    type Item Bitmap = Bit
    fromList = vFromList
    toList = vToList

instance C.InnerFunctor Bitmap where
    imap = map

instance C.Foldable Bitmap where
    foldr = foldr
    foldl' = foldl'
    foldr' = foldr'

instance C.Collection Bitmap where
    null = null
    length = length
    elem e = Data.List.elem e . toList
    maximum = any id . C.getNonEmpty
    minimum = all id . C.getNonEmpty
    all = all
    any = any

instance C.Sequential Bitmap where
    take = take
    drop = drop
    splitAt = splitAt
    revTake n = unoptimised (C.revTake n)
    revDrop n = unoptimised (C.revDrop n)
    splitOn = splitOn
    break = break
    breakEnd = breakEnd
    span = span
    filter = filter
    reverse = reverse
    snoc = snoc
    cons = cons
    unsnoc = unsnoc
    uncons = uncons
    intersperse = intersperse
    find = find
    sortBy = sortBy
    singleton = fromList . (:[])
    replicate n = fromList . C.replicate n

instance C.IndexedCollection Bitmap where
    (!) l n
        | isOutOfBound n (length l) = Nothing
        | otherwise                     = Just $ index l n
    findIndex predicate c = loop 0
      where
        !len = length c
        loop i
            | i .==# len                  = Nothing
            | predicate (unsafeIndex c i) = Just i
            | otherwise                   = Nothing

instance C.MutableCollection MutableBitmap where
    type MutableFreezed MutableBitmap = Bitmap
    type MutableKey MutableBitmap = Offset Bit
    type MutableValue MutableBitmap = Bit

    thaw = thaw
    freeze = freeze
    unsafeThaw = unsafeThaw
    unsafeFreeze = unsafeFreeze

    mutNew = new
    mutUnsafeWrite = unsafeWrite
    mutUnsafeRead = unsafeRead
    mutWrite = write
    mutRead = read



bitmapIndex :: Offset Bit -> (Offset Word32, Int)
bitmapIndex (Offset !i) = (Offset (i .>>. shiftPerTy), i .&. maskPerTy)
{-# INLINE bitmapIndex #-}

-- return the index in word32 quantity and mask to a bit in a bitmap
{-
bitmapAddr :: Int -> (# Int , Word #)
bitmapAddr !i = (# idx, mask #)
  where (!idx, !bitIdx) = bitmapIndex i
        !mask = case bitIdx of
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
-}

thaw :: PrimMonad prim => Bitmap -> prim (MutableBitmap (PrimState prim))
thaw (Bitmap len ba) = MutableBitmap len `fmap` C.thaw ba

freeze :: PrimMonad prim => MutableBitmap (PrimState prim) -> prim Bitmap
freeze (MutableBitmap len mba) = Bitmap len `fmap` C.freeze mba

unsafeThaw :: PrimMonad prim => Bitmap -> prim (MutableBitmap (PrimState prim))
unsafeThaw (Bitmap len ba) = MutableBitmap len `fmap` C.unsafeThaw ba

unsafeFreeze :: PrimMonad prim => MutableBitmap (PrimState prim) -> prim Bitmap
unsafeFreeze (MutableBitmap len mba) = Bitmap len `fmap` C.unsafeFreeze mba

unsafeWrite :: PrimMonad prim => MutableBitmap (PrimState prim) -> Offset Bit -> Bit -> prim ()
unsafeWrite (MutableBitmap _ ma) i v = do
    let (idx, bitIdx) = bitmapIndex i
    w <- A.unsafeRead ma idx
    let w' = if v then setBit w bitIdx else clearBit w bitIdx
    A.unsafeWrite ma idx w'
{-# INLINE unsafeWrite #-}

unsafeRead :: PrimMonad prim => MutableBitmap (PrimState prim) -> Offset Bit -> prim Bit
unsafeRead (MutableBitmap _ ma) i = do
    let (idx, bitIdx) = bitmapIndex i
    flip testBit bitIdx `fmap` A.unsafeRead ma idx
{-# INLINE unsafeRead #-}

write :: PrimMonad prim => MutableBitmap (PrimState prim) -> Offset Bit -> Bit -> prim ()
write mb n val
    | isOutOfBound n len = primOutOfBound OOB_Write n len
    | otherwise          = unsafeWrite mb n val
  where
    len = mutableLength mb
{-# INLINE write #-}

read :: PrimMonad prim => MutableBitmap (PrimState prim) -> Offset Bit -> prim Bit
read mb n
    | isOutOfBound n len = primOutOfBound OOB_Read n len
    | otherwise        = unsafeRead mb n
  where len = mutableLength mb
{-# INLINE read #-}

-- | Return the element at a specific index from a Bitmap.
--
-- If the index @n is out of bounds, an error is raised.
index :: Bitmap -> Offset Bit -> Bit
index bits n
    | isOutOfBound n len = outOfBound OOB_Index n len
    | otherwise          = unsafeIndex bits n
  where len = length bits
{-# INLINE index #-}

-- | Return the element at a specific index from an array without bounds checking.
--
-- Reading from invalid memory can return unpredictable and invalid values.
-- use 'index' if unsure.
unsafeIndex :: Bitmap -> Offset Bit -> Bit
unsafeIndex (Bitmap _ ba) n =
    let (idx, bitIdx) = bitmapIndex n
     in testBit (A.unsafeIndex ba idx) bitIdx

{-# INLINE unsafeIndex #-}

-----------------------------------------------------------------------
-- higher level collection implementation
-----------------------------------------------------------------------
length :: Bitmap -> CountOf Bit
length (Bitmap sz _) = sz

mutableLength :: MutableBitmap st -> CountOf Bit
mutableLength (MutableBitmap sz _) = sz

empty :: Bitmap
empty = Bitmap 0 mempty

new :: PrimMonad prim => CountOf Bit -> prim (MutableBitmap (PrimState prim))
new sz@(CountOf len) =
    MutableBitmap sz <$> A.new nbElements
  where
    nbElements :: CountOf Word32
    nbElements = CountOf ((len `alignRoundUp` bitsPerTy) .>>. shiftPerTy)

-- | make an array from a list of elements.
vFromList :: [Bit] -> Bitmap
vFromList allBools = runST $ do
    mbitmap <- new len
    loop mbitmap 0 allBools
  where
    loop mb _ []     = unsafeFreeze mb
    loop mb i (x:xs) = unsafeWrite mb i x >> loop mb (i+1) xs

{-
    runST $ do
    mba <- A.new nbElements
    ba  <- loop mba (0 :: Int) allBools
    pure (Bitmap len ba)
  where
    loop mba _ [] = A.unsafeFreeze mba
    loop mba i l  = do
        let (l1, l2) = C.splitAt bitsPerTy l
            w = toPacked l1
        A.unsafeWrite mba i w
        loop mba (i+1) l2

    toPacked :: [Bool] -> Word32
    toPacked l =
        C.foldl' (.|.) 0 $ Prelude.zipWith (\b w -> if b then (1 `shiftL` w) else 0) l (C.reverse [0..31])
-}
    len        = C.length allBools

-- | transform an array to a list.
vToList :: Bitmap -> [Bit]
vToList a = loop 0
  where len = length a
        loop i | i .==# len  = []
               | otherwise = unsafeIndex a i : loop (i+1)

-- | Check if two vectors are identical
equal :: Bitmap -> Bitmap -> Bool
equal a b
    | la /= lb  = False
    | otherwise = loop 0
  where
    !la = length a
    !lb = length b
    loop n | n .==# la = True
           | otherwise = (unsafeIndex a n == unsafeIndex b n) && loop (n+1)

-- | Compare 2 vectors
vCompare :: Bitmap -> Bitmap -> Ordering
vCompare a b = loop 0
  where
    !la = length a
    !lb = length b
    loop n
        | n .==# la = if la == lb then EQ else LT
        | n .==# lb = GT
        | otherwise =
            case unsafeIndex a n `compare` unsafeIndex b n of
                EQ -> loop (n+1)
                r  -> r

-- | Append 2 arrays together by creating a new bigger array
--
-- TODO completely non optimized
append :: Bitmap -> Bitmap -> Bitmap
append a b = fromList $ toList a `mappend` toList b

-- TODO completely non optimized
concat :: [Bitmap] -> Bitmap
concat l = fromList $ mconcat $ fmap toList l

null :: Bitmap -> Bool
null (Bitmap nbBits _) = nbBits == 0

take :: CountOf Bit -> Bitmap -> Bitmap
take nbElems bits@(Bitmap nbBits ba)
    | nbElems <= 0      = empty
    | nbElems >= nbBits = bits
    | otherwise         = Bitmap nbElems ba -- TODO : although it work right now, take on the underlaying ba too

drop :: CountOf Bit -> Bitmap -> Bitmap
drop nbElems bits@(Bitmap nbBits _)
    | nbElems <= 0      = bits
    | nbElems >= nbBits = empty
    | otherwise         = unoptimised (C.drop nbElems) bits
        -- TODO: decide if we have drop easy by having a bit offset in the data structure
        -- or if we need to shift stuff around making all the indexing slighlty more complicated

splitAt :: CountOf Bit -> Bitmap -> (Bitmap, Bitmap)
splitAt n v = (take n v, drop n v)

-- unoptimised
splitOn :: (Bit -> Bool) -> Bitmap -> [Bitmap]
splitOn f bits = fmap fromList $ C.splitOn f $ toList bits

-- unoptimised
break :: (Bit -> Bool) -> Bitmap -> (Bitmap, Bitmap)
break predicate v = findBreak 0
  where
    len = length v
    findBreak i
        | i .==# len = (v, empty)
        | otherwise  =
            if predicate (unsafeIndex v i)
                then splitAt (offsetAsSize i) v
                else findBreak (i+1)

breakEnd :: (Bit -> Bool) -> Bitmap -> (Bitmap, Bitmap)
breakEnd predicate = bimap fromList fromList . C.breakEnd predicate . toList

span :: (Bit -> Bool) -> Bitmap -> (Bitmap, Bitmap)
span p = break (not . p)

map :: (Bit -> Bit) -> Bitmap -> Bitmap
map f bits = unoptimised (fmap f) bits

--mapIndex :: (Int -> Bool -> Bool) -> Bitmap -> Bitmap
--mapIndex f Bitmap =

cons :: Bit -> Bitmap -> Bitmap
cons v l = unoptimised (C.cons v) l

snoc :: Bitmap -> Bit -> Bitmap
snoc l v = unoptimised (flip C.snoc v) l

-- unoptimised
uncons :: Bitmap -> Maybe (Bit, Bitmap)
uncons b = fmap (second fromList) $ C.uncons $ toList b

-- unoptimised
unsnoc :: Bitmap -> Maybe (Bitmap, Bit)
unsnoc b = fmap (first fromList) $ C.unsnoc $ toList b

intersperse :: Bit -> Bitmap -> Bitmap
intersperse b = unoptimised (C.intersperse b)

find :: (Bit -> Bool) -> Bitmap -> Maybe Bool
find predicate vec = loop 0
  where
    !len = length vec
    loop i
        | i .==# len = Nothing
        | otherwise  =
            let e = unsafeIndex vec i
             in if predicate e then Just e else loop (i+1)

sortBy :: (Bit -> Bit -> Ordering) -> Bitmap -> Bitmap
sortBy by bits = unoptimised (C.sortBy by) bits

filter :: (Bit -> Bool) -> Bitmap -> Bitmap
filter predicate vec = unoptimised (Data.List.filter predicate) vec

reverse :: Bitmap -> Bitmap
reverse bits = unoptimised C.reverse bits

foldr :: (Bit -> a -> a) -> a -> Bitmap -> a
foldr f initialAcc vec = loop 0
  where
    len = length vec
    loop i
        | i .==# len = initialAcc
        | otherwise  = unsafeIndex vec i `f` loop (i+1)

foldr' :: (Bit -> a -> a) -> a -> Bitmap -> a
foldr' = foldr

foldl' :: (a -> Bit -> a) -> a -> Bitmap -> a
foldl' f initialAcc vec = loop 0 initialAcc
  where
    len = length vec
    loop i !acc
        | i .==# len = acc
        | otherwise  = loop (i+1) (f acc (unsafeIndex vec i))

all :: (Bit -> Bool) -> Bitmap -> Bool
all p bm = loop 0
  where
    len = length bm
    loop !i
      | i .==# len = True
      | not $ p (unsafeIndex bm i) = False
      | otherwise = loop (i + 1)

any :: (Bit -> Bool) -> Bitmap -> Bool
any p bm = loop 0
  where
    len = length bm
    loop !i
      | i .==# len = False
      | p (unsafeIndex bm i) = True
      | otherwise = loop (i + 1)

unoptimised :: ([Bit] -> [Bit]) -> Bitmap -> Bitmap
unoptimised f = vFromList . f . vToList
