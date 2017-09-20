{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}
module Basement.Block.Base
    ( Block(..)
    , MutableBlock(..)
    -- * Basic accessor
    , unsafeNew
    , unsafeThaw
    , unsafeFreeze
    , unsafeCopyElements
    , unsafeCopyElementsRO
    , unsafeCopyBytes
    , unsafeCopyBytesRO
    , unsafeRead
    , unsafeWrite
    , unsafeIndex
    -- * Properties
    , length
    , lengthBytes
    -- * Other methods
    , mutableEmpty
    , new
    , newPinned
    , touch
    , mutableTouch
    ) where

import           GHC.Prim
import           GHC.Types
import           GHC.ST
import           GHC.IO
import qualified Data.List
import           Basement.Compat.Base
import           Data.Proxy
import           Basement.Compat.Primitive
import           Basement.Compat.Semigroup
import           Basement.Bindings.Memory (sysHsMemcmpBaBa)
import           Basement.Types.OffsetSize
import           Basement.Monad
import           Basement.NormalForm
import           Basement.Numerical.Additive
import           Basement.PrimType

-- | A block of memory containing unpacked bytes representing values of type 'ty'
data Block ty = Block ByteArray#
    deriving (Typeable)

instance Data ty => Data (Block ty) where
    dataTypeOf _ = blockType
    toConstr _   = error "toConstr"
    gunfold _ _  = error "gunfold"

blockType :: DataType
blockType = mkNoRepType "Foundation.Block"

instance NormalForm (Block ty) where
    toNormalForm (Block !_) = ()
instance (PrimType ty, Show ty) => Show (Block ty) where
    show v = show (toList v)
instance (PrimType ty, Eq ty) => Eq (Block ty) where
    {-# SPECIALIZE instance Eq (Block Word8) #-}
    (==) = equal
instance (PrimType ty, Ord ty) => Ord (Block ty) where
    compare = internalCompare

instance PrimType ty => Semigroup (Block ty) where
    (<>) = append
instance PrimType ty => Monoid (Block ty) where
    mempty  = empty
    mappend = append
    mconcat = concat

instance PrimType ty => IsList (Block ty) where
    type Item (Block ty) = ty
    fromList = internalFromList
    toList = internalToList

length :: forall ty . PrimType ty => Block ty -> CountOf ty
length (Block ba) =
    case primShiftToBytes (Proxy :: Proxy ty) of
        0           -> CountOf (I# (sizeofByteArray# ba))
        (I# szBits) -> CountOf (I# (uncheckedIShiftRL# (sizeofByteArray# ba) szBits))
{-# INLINE[1] length #-}
{-# SPECIALIZE [2] length :: Block Word8 -> CountOf Word8 #-}

lengthBytes :: Block ty -> CountOf Word8
lengthBytes (Block ba) = CountOf (I# (sizeofByteArray# ba))
{-# INLINE[1] lengthBytes #-}

-- | Create an empty block of memory
empty :: Block ty
empty = Block ba where !(Block ba) = empty_

empty_ :: Block ()
empty_ = runST $ primitive $ \s1 ->
    case newByteArray# 0# s1           of { (# s2, mba #) ->
    case unsafeFreezeByteArray# mba s2 of { (# s3, ba  #) ->
        (# s3, Block ba #) }}

mutableEmpty :: PrimMonad prim => prim (MutableBlock ty (PrimState prim))
mutableEmpty = primitive $ \s1 ->
    case newByteArray# 0# s1 of { (# s2, mba #) ->
        (# s2, MutableBlock mba #) }

-- | Return the element at a specific index from an array without bounds checking.
--
-- Reading from invalid memory can return unpredictable and invalid values.
-- use 'index' if unsure.
unsafeIndex :: forall ty . PrimType ty => Block ty -> Offset ty -> ty
unsafeIndex (Block ba) n = primBaIndex ba n
{-# SPECIALIZE unsafeIndex :: Block Word8 -> Offset Word8 -> Word8 #-}
{-# INLINE unsafeIndex #-}

-- | make a block from a list of elements.
internalFromList :: PrimType ty => [ty] -> Block ty
internalFromList l = runST $ do
    ma <- new (CountOf len)
    iter azero l $ \i x -> unsafeWrite ma i x
    unsafeFreeze ma
  where len = Data.List.length l
        iter _  []     _ = return ()
        iter !i (x:xs) z = z i x >> iter (i+1) xs z

-- | transform a block to a list.
internalToList :: forall ty . PrimType ty => Block ty -> [ty]
internalToList blk@(Block ba)
    | len == azero = []
    | otherwise    = loop azero
  where
    !len = length blk
    loop !i | i .==# len = []
            | otherwise  = primBaIndex ba i : loop (i+1)

-- | Check if two blocks are identical
equal :: (PrimType ty, Eq ty) => Block ty -> Block ty -> Bool
equal a b
    | la /= lb  = False
    | otherwise = loop azero
  where
    !la = lengthBytes a
    !lb = lengthBytes b
    lat = length a

    loop !n | n .==# lat = True
            | otherwise  = (unsafeIndex a n == unsafeIndex b n) && loop (n+o1)
    o1 = Offset (I# 1#)
{-# RULES "Block/Eq/Word8" [3]
   forall (a :: Block Word8) b . equal a b = equalMemcmp a b #-}
{-# INLINEABLE [2] equal #-}
-- {-# SPECIALIZE equal :: Block Word8 -> Block Word8 -> Bool #-}

equalMemcmp :: PrimMemoryComparable ty => Block ty -> Block ty -> Bool
equalMemcmp b1@(Block a) b2@(Block b)
    | la /= lb  = False
    | otherwise = unsafeDupablePerformIO (sysHsMemcmpBaBa a 0 b 0 la) == 0
  where
    la = lengthBytes b1
    lb = lengthBytes b2
{-# SPECIALIZE equalMemcmp :: Block Word8 -> Block Word8 -> Bool #-}

-- | Compare 2 blocks
internalCompare :: (Ord ty, PrimType ty) => Block ty -> Block ty -> Ordering
internalCompare a b = loop azero
  where
    !la = length a
    !lb = length b
    !end = sizeAsOffset (min la lb)
    loop !n
        | n == end  = la `compare` lb
        | v1 == v2  = loop (n + Offset (I# 1#))
        | otherwise = v1 `compare` v2
      where
        v1 = unsafeIndex a n
        v2 = unsafeIndex b n
{-# RULES "Block/Ord/Word8" [3] forall (a :: Block Word8) b . internalCompare a b = compareMemcmp a b #-}
{-# NOINLINE internalCompare #-}

compareMemcmp :: PrimMemoryComparable ty => Block ty -> Block ty -> Ordering
compareMemcmp b1@(Block a) b2@(Block b) =
    case unsafeDupablePerformIO (sysHsMemcmpBaBa a 0 b 0 sz) of
        0             -> la `compare` lb
        n | n > 0     -> GT
          | otherwise -> LT
  where
    la = lengthBytes b1
    lb = lengthBytes b2
    sz = min la lb
{-# SPECIALIZE [3] compareMemcmp :: Block Word8 -> Block Word8 -> Ordering #-}

-- | Append 2 blocks together by creating a new bigger block
append :: Block ty -> Block ty -> Block ty
append a b
    | la == azero = b
    | lb == azero = a
    | otherwise = runST $ do
        r  <- unsafeNew Unpinned (la+lb)
        unsafeCopyBytesRO r 0                 a 0 la
        unsafeCopyBytesRO r (sizeAsOffset la) b 0 lb
        unsafeFreeze r
  where
    !la = lengthBytes a
    !lb = lengthBytes b

concat :: [Block ty] -> Block ty
concat [] = empty
concat l  =
    case filterAndSum 0 [] l of
        (_,[])            -> empty
        (_,[x])           -> x
        (totalLen,chunks) -> runST $ do
            r <- unsafeNew Unpinned totalLen
            doCopy r 0 chunks
            unsafeFreeze r
  where
    -- TODO would go faster not to reverse but pack from the end instead
    filterAndSum !totalLen acc []     = (totalLen, Data.List.reverse acc)
    filterAndSum !totalLen acc (x:xs)
        | len == 0  = filterAndSum totalLen acc xs
        | otherwise = filterAndSum (len+totalLen) (x:acc) xs
      where len = lengthBytes x

    doCopy _ _ []     = return ()
    doCopy r i (x:xs) = do
        unsafeCopyBytesRO r i x 0 lx
        doCopy r (i `offsetPlusE` lx) xs
      where !lx = lengthBytes x

-- | A Mutable block of memory containing unpacked bytes representing values of type 'ty'
data MutableBlock ty st = MutableBlock (MutableByteArray# st)

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
unsafeNew :: PrimMonad prim
          => PinnedStatus
          -> CountOf Word8
          -> prim (MutableBlock ty (PrimState prim))
unsafeNew pinSt (CountOf (I# bytes)) = case pinSt of
    Unpinned -> primitive $ \s1 -> case newByteArray# bytes s1 of { (# s2, mba #) -> (# s2, MutableBlock mba #) }
    _        -> primitive $ \s1 -> case newAlignedPinnedByteArray# bytes 8# s1 of { (# s2, mba #) -> (# s2, MutableBlock mba #) }

-- | Create a new mutable block of a specific N size of 'ty' elements
new :: forall prim ty . (PrimMonad prim, PrimType ty) => CountOf ty -> prim (MutableBlock ty (PrimState prim))
new n = unsafeNew Unpinned (sizeOfE (primSizeInBytes (Proxy :: Proxy ty)) n)

newPinned :: forall prim ty . (PrimMonad prim, PrimType ty) => CountOf ty -> prim (MutableBlock ty (PrimState prim))
newPinned n = unsafeNew Pinned (sizeOfE (primSizeInBytes (Proxy :: Proxy ty)) n)

-- | Copy a number of elements from an array to another array with offsets
unsafeCopyElements :: forall prim ty . (PrimMonad prim, PrimType ty)
                   => MutableBlock ty (PrimState prim) -- ^ destination mutable block
                   -> Offset ty                        -- ^ offset at destination
                   -> MutableBlock ty (PrimState prim) -- ^ source mutable block
                   -> Offset ty                        -- ^ offset at source
                   -> CountOf ty                          -- ^ number of elements to copy
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
                     -> CountOf ty                          -- ^ number of elements to copy
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
                -> CountOf Word8                       -- ^ number of elements to copy
                -> prim ()
unsafeCopyBytes (MutableBlock dstMba) (Offset (I# d)) (MutableBlock srcBa) (Offset (I# s)) (CountOf (I# n)) =
    primitive $ \st -> (# copyMutableByteArray# srcBa s dstMba d n st, () #)
{-# INLINE unsafeCopyBytes #-}

-- | Copy a number of bytes from a Block to a MutableBlock with specific byte offsets
unsafeCopyBytesRO :: forall prim ty . PrimMonad prim
                  => MutableBlock ty (PrimState prim) -- ^ destination mutable block
                  -> Offset Word8                     -- ^ offset at destination
                  -> Block ty                         -- ^ source block
                  -> Offset Word8                     -- ^ offset at source
                  -> CountOf Word8                       -- ^ number of elements to copy
                  -> prim ()
unsafeCopyBytesRO (MutableBlock dstMba) (Offset (I# d)) (Block srcBa) (Offset (I# s)) (CountOf (I# n)) =
    primitive $ \st -> (# copyByteArray# srcBa s dstMba d n st, () #)
{-# INLINE unsafeCopyBytesRO #-}

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

touch :: PrimMonad prim => Block ty -> prim ()
touch (Block ba) = unsafePrimFromIO $ primitive $ \s -> case touch# ba s of { s2 -> (# s2, () #) }

mutableTouch :: PrimMonad prim => MutableBlock ty (PrimState prim) -> prim ()
mutableTouch (MutableBlock mba) = unsafePrimFromIO $ primitive $ \s -> case touch# mba s of { s2 -> (# s2, () #) }

