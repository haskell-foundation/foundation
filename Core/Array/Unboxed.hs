-- |
-- Module      : Core.Array.Unboxed
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
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}
module Core.Array.Unboxed
    ( UArray(..)
    , ByteArray
    , PrimType(..)
    -- * methods
    , copy
    , copyAtRO
    -- * internal methods
    , copyAddr
    , unsafeRecast
    , length
    , freeze
    , unsafeFreeze
    , thaw
    , unsafeThaw
    -- * Creation
    , create
    , sub
    , withPtr
    , withMutablePtr
    , unsafeFreezeShrink
    , freezeShrink
    , unsafeSlide
    -- * accessors
    , update
    , unsafeUpdate
    , unsafeIndex
    -- * Functions
    , map
    , mapIndex
    , index
    , null
    , take
    , drop
    , splitAt
    , revDrop
    , revTake
    , revSplitAt
    , splitOn
    , break
    , span
    , cons
    , snoc
    , uncons
    , unsnoc
    , find
    , sortBy
    , filter
    , reverse
    , foldl
    , foldr
    , foldl'
    , foreignMem
    , fromForeignPtr
    ) where

import           GHC.Prim
import           GHC.Types
import           GHC.ST
import           GHC.Ptr
import           GHC.ForeignPtr (ForeignPtr)
import qualified Prelude
import           Core.Internal.Base
import           Core.Internal.Primitive
import           Core.Internal.Proxy
import           Core.Primitive.Monad
import           Core.Primitive.Types
import           Core.Primitive.FinalPtr
import           Core.Primitive.Utils
import           Core.Array.Common
import           Core.Array.Unboxed.Mutable
import           Core.Number
import qualified Data.List

-- | An array of type built on top of GHC primitive.
--
-- The elements need to have fixed sized and the representation is a
-- packed contiguous array in memory that can easily be passed
-- to foreign interface
data UArray ty = UVecBA {-# UNPACK #-} !PinnedStatus {- unpinned / pinned flag -} ByteArray#
               | UVecAddr {-# UNPACK #-} !Int {- number of items of type ty -} (FinalPtr ty)
               | UVecSlice {-# UNPACK #-} !Int {-# UNPACK #-} !Int (UArray ty)

-- | Byte Array alias
type ByteArray = UArray Word8

instance (PrimType ty, Show ty) => Show (UArray ty) where
    show v = show (toList v)
instance (PrimType ty, Eq ty) => Eq (UArray ty) where
    (==) = equal
instance (PrimType ty, Ord ty) => Ord (UArray ty) where
    compare = vCompare

instance PrimType ty => Monoid (UArray ty) where
    mempty  = empty
    mappend = append
    mconcat = concat

instance PrimType ty => IsList (UArray ty) where
    type Item (UArray ty) = ty
    fromList = vFromList
    toList = vToList

{-
fmapUVec :: (PrimType a, PrimType b) => (a -> b) -> UArray a -> UArray b
fmapUVec mapper a = runST (new nbElems >>= copyMap (unsafeIndex a) mapper)
  where
    !nbElems = length a
    copyMap :: (PrimType a, PrimType b, PrimMonad prim)
            => (Int -> a) -> (a -> b) -> MUArray b (PrimState prim) -> prim (UArray b)
    copyMap get f ma = iter 0
      where
        iter i
            | i == nbElems = unsafeFreeze ma
            | otherwise    = unsafeWrite ma i (f $ get i) >> iter (i+1)

sizeInBytes :: PrimType ty => UArray ty -> Int
sizeInBytes     (UVecBA _ ba)  = I# (sizeofByteArray# ba)
sizeInBytes vec@(UVecAddr l _) = (I# l) `div` sizeInBytesOfContent vec

mutableSizeInBytes :: (PrimMonad prim, PrimType ty) => MUArray ty (PrimState prim) -> prim Int
mutableSizeInBytes (MUVecMA _ mba) = primitive $ \s ->
    case compatGetSizeofMutableByteArray# mba s of { (# s2, i #) -> (# s2, I# i #) }
mutableSizeInBytes muv@(MUVecAddr i _) =
    return (I# i * sizeInMutableBytesOfContent muv)
-}

vectorProxyTy :: UArray ty -> Proxy ty
vectorProxyTy _ = Proxy

-- rename to sizeInBitsOfCell
sizeInBytesOfContent :: PrimType ty => UArray ty -> Int
sizeInBytesOfContent = primSizeInBytes . vectorProxyTy
{-# INLINE sizeInBytesOfContent #-}

-- | Copy every cells of an existing array to a new array
copy :: PrimType ty => UArray ty -> UArray ty
copy array = runST (thaw array >>= unsafeFreeze)

-- | Thaw an array to a mutable array.
--
-- the array is not modified, instead a new mutable array is created
-- and every values is copied, before returning the mutable array.
thaw :: (PrimMonad prim, PrimType ty) => UArray ty -> prim (MUArray ty (PrimState prim))
thaw array@(UVecBA _ ba) = do
    ma@(MUVecMA _ mba) <- new (length array)
    primCopyFreezedBytes mba ba
    return ma
thaw array@(UVecAddr len fptr) = withFinalPtr fptr $ \(Ptr addr) -> do
    ma@(MUVecMA _ mba) <- new len
    let !(I# bytes#) = sizeInBytesOfContent array * len
    primitive $ \s -> (# compatCopyAddrToByteArray# addr mba 0# bytes# s, () #)
    return ma
thaw (UVecSlice start end parent) = do
    let sz = end - start
    ma <- new sz
    copyAtRO ma 0 parent start sz
    return ma
{-# INLINE thaw #-}

-- | Return the element at a specific index from an array.
--
-- If the index @n is out of bounds, an error is raised.
index :: PrimType ty => UArray ty -> Int -> ty
index array n
    | n < 0 || n >= len = throw (OutOfBound OOB_Index n len)
    | otherwise         = unsafeIndex array n
  where len = length array
{-# INLINE index #-}

-- | Return the element at a specific index from an array without bounds checking.
--
-- Reading from invalid memory can return unpredictable and invalid values.
-- use 'index' if unsure.
unsafeIndex :: PrimType ty => UArray ty -> Int -> ty
unsafeIndex (UVecBA _ ba) n = primBaIndex ba n
unsafeIndex v@(UVecAddr _ fptr) n = withUnsafeFinalPtr fptr (primAddrIndex' v)
  where
    primAddrIndex' :: PrimType ty => UArray ty -> Ptr a -> IO ty
    primAddrIndex' _ (Ptr addr) = return (primAddrIndex addr n)
unsafeIndex (UVecSlice start _ parent) n = unsafeIndex parent (n + start)
{-# INLINE unsafeIndex #-}

unsafeIndexer :: (PrimMonad prim, PrimType ty) => UArray ty -> ((Int -> ty) -> prim a) -> prim a
unsafeIndexer (UVecBA _ ba)     f = f (primBaIndex ba)
unsafeIndexer (UVecAddr _ fptr) f = withFinalPtr fptr (\ptr -> f (primAddrIndex' ptr))
  where
    primAddrIndex' :: PrimType ty => Ptr a -> (Int -> ty)
    primAddrIndex' (Ptr addr) = primAddrIndex addr
unsafeIndexer (UVecSlice start _ (UVecBA _ ba))     f = f (\n -> primBaIndex ba (start + n))
unsafeIndexer (UVecSlice start _ (UVecAddr _ fptr)) f = withFinalPtr fptr (f . primAddrIndex')
  where
    primAddrIndex' :: PrimType ty => Ptr a -> (Int -> ty)
    primAddrIndex' (Ptr addr) = \n -> primAddrIndex addr (start + n)
unsafeIndexer (UVecSlice _     _ _) _ = error "internal error: cannot happen"

foreignMem :: PrimType ty
           => FinalPtr ty -- ^ the start pointer with a finalizer
           -> Int         -- ^ the number of elements (in elements, not bytes)
           -> UArray ty
foreignMem fptr nb = UVecAddr nb fptr

fromForeignPtr :: PrimType ty
               => (ForeignPtr ty, Int, Int) -- ForeignPtr, an offset in prim elements, a size in prim elements
               -> UArray ty
fromForeignPtr (fptr, 0, len) = UVecAddr len (toFinalPtrForeign fptr)
fromForeignPtr (fptr, ofs, len) = UVecSlice ofs (ofs+len) (UVecAddr len (toFinalPtrForeign fptr))

-- | return the number of elements of the array.
length :: PrimType ty => UArray ty -> Int
length (UVecAddr len _) = len
length v@(UVecBA _ a) =
    let !(I# szBits) = primSizeInBytes (vectorProxyTy v)
        !elems       = quotInt# (sizeofByteArray# a) szBits
     in I# elems
length (UVecSlice start end _) = end - start
{-# INLINE[1] length #-}

-- fast case for ByteArray
{-# RULES "length/ByteArray"  length = lengthByteArray #-}

lengthByteArray :: UArray Word8 -> Int
lengthByteArray (UVecBA _ a) = I# (sizeofByteArray# a)
lengthByteArray (UVecAddr len _) = len
lengthByteArray (UVecSlice start end _) = end - start

-- TODO Optimise with copyByteArray#
copyAtRO :: (PrimMonad prim, PrimType ty)
         => MUArray ty (PrimState prim) -- ^ destination array
         -> Int                -- ^ offset at destination
         -> UArray ty         -- ^ source array
         -> Int                -- ^ offset at source
         -> Int                -- ^ number of elements to copy
         -> prim ()
copyAtRO (MUVecMA _ dstMba) ed uvec@(UVecBA _ srcBa) es n =
    primitive $ \st -> (# copyByteArray# srcBa os dstMba od nBytes st, () #)
  where
    sz = primSizeInBytes (vectorProxyTy uvec)
    !(I# os)     = es * sz
    !(I# od)     = ed * sz
    !(I# nBytes) = n * sz
copyAtRO (MUVecMA _ dstMba) ed uvec@(UVecAddr _ srcFptr) es n =
    withFinalPtr srcFptr $ \srcPtr ->
        let !(Ptr srcAddr) = srcPtr `plusPtr` os
         in primitive $ \s -> (# compatCopyAddrToByteArray# srcAddr dstMba od nBytes s, () #)
  where
    sz  = primSizeInBytes (vectorProxyTy uvec)
    !os = es * sz
    !(I# od)     = ed * sz
    !(I# nBytes) = n * sz
copyAtRO dst od src os n = loop od os
  where endIndex = os + n
        loop d i
            | i == endIndex = return ()
            | otherwise     = unsafeWrite dst d (unsafeIndex src i) >> loop (d+1) (i+1)

-- | Freeze a mutable array into an array.
--
-- the MUArray must not be changed after freezing.
unsafeFreeze :: PrimMonad prim => MUArray ty (PrimState prim) -> prim (UArray ty)
unsafeFreeze (MUVecMA pinnedState mba) = primitive $ \s1 ->
    case unsafeFreezeByteArray# mba s1 of
        (# s2, ba #) -> (# s2, UVecBA pinnedState ba #)
unsafeFreeze (MUVecAddr len fptr) = return $ UVecAddr len fptr
{-# INLINE unsafeFreeze #-}

unsafeFreezeShrink :: (PrimType ty, PrimMonad prim) => MUArray ty (PrimState prim) -> Int -> prim (UArray ty)
unsafeFreezeShrink muvec n = do
    let !(I# newSize) = n * (sizeInMutableBytesOfContent muvec)
    case muvec of
        MUVecMA _ mba -> do
            muvec2 <- primitive $ \s -> case compatShrinkMutableByteArray# mba newSize s of { (# s2, mba2 #) -> (# s2, MUVecMA pinned mba2 #) }
            unsafeFreeze muvec2
        MUVecAddr _ addr        -> unsafeFreeze (MUVecAddr n addr)

freeze :: (PrimType ty, PrimMonad prim) => MUArray ty (PrimState prim) -> prim (UArray ty)
freeze ma = do
    ma' <- new len
    copyAt ma' 0 ma 0 len
    unsafeFreeze ma'
  where len = mutableLength ma

freezeShrink :: (PrimType ty, PrimMonad prim) => MUArray ty (PrimState prim) -> Int -> prim (UArray ty)
freezeShrink ma n = do
    ma' <- new n
    copyAt ma' 0 ma 0 n
    unsafeFreeze ma'

unsafeSlide :: PrimMonad prim => MUArray ty (PrimState prim) -> Int -> Int -> prim ()
unsafeSlide (MUVecMA _ mba) start end    = primMutableByteArraySlideToStart mba start end
unsafeSlide (MUVecAddr _ fptr) start end = withFinalPtr fptr $ \(Ptr addr) ->
    primMutableAddrSlideToStart addr start end

-- | Thaw an immutable array.
--
-- The UArray must not be used after thawing.
unsafeThaw :: (PrimType ty, PrimMonad prim) => UArray ty -> prim (MUArray ty (PrimState prim))
unsafeThaw (UVecBA pinnedState ba) = primitive $ \st -> (# st, MUVecMA pinnedState (unsafeCoerce# ba) #)
unsafeThaw (UVecAddr len fptr) = return $ MUVecAddr len fptr
unsafeThaw v = thaw v
{-# INLINE unsafeThaw #-}

-- | Create a new array of size @n by settings each cells through the
-- function @f.
create :: PrimType ty
       => Int         -- ^ the size of the array
       -> (Int -> ty) -- ^ the function that set the value at the index
       -> UArray ty  -- ^ the array created
create n initializer = runST (new n >>= iter initializer)
  where
    iter :: (PrimType ty, PrimMonad prim) => (Int -> ty) -> MUArray ty (PrimState prim) -> prim (UArray ty)
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

empty :: PrimType ty => UArray ty
empty = runST (new 0 >>= unsafeFreeze)

-- | make an array from a list of elements.
vFromList :: PrimType ty => [ty] -> UArray ty
vFromList l = runST $ do
    ma <- new len
    iter 0 l $ \i x -> unsafeWrite ma i x
    unsafeFreeze ma
  where len = Data.List.length l
        iter _ [] _ = return ()
        iter i (x:xs) z = z i x >> iter (i+1) xs z

-- | transform an array to a list.
vToList :: PrimType ty => UArray ty -> [ty]
vToList a = runST (unsafeIndexer a go)
  where
    !len = length a
    go :: (Int -> ty) -> ST s [ty]
    go getIdx = return $ loop 0
      where
        loop i | i == len  = []
               | otherwise = getIdx i : loop (i+1)

-- | Check if two vectors are identical
equal :: (PrimType ty, Eq ty) => UArray ty -> UArray ty -> Bool
equal a b
    | la /= lb  = False
    | otherwise = loop 0
  where
    !la = length a
    !lb = length b
    loop n | n == la    = True
           | otherwise = (unsafeIndex a n == unsafeIndex b n) && loop (n+1)

{-
sizeEqual :: PrimType ty => UArray ty -> UArray ty -> Bool
sizeEqual a b = length a == length b -- TODO optimise with direct comparaison of bytes or elements when possible
-}

-- | Compare 2 vectors
vCompare :: (Ord ty, PrimType ty) => UArray ty -> UArray ty -> Ordering
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
append :: PrimType ty => UArray ty -> UArray ty -> UArray ty
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

concat :: PrimType ty => [UArray ty] -> UArray ty
concat (Data.List.dropWhile null -> l) =
  let lens = fmap length l
      count = Prelude.sum lens
  in case lens of
      [] -> empty
      len:_ | len == count -> Data.List.head l
      _ -> runST $ do
          r <- new count
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
       => UArray ty
       -> [(Int, ty)]
       -> UArray ty
update array modifiers = runST (thaw array >>= doUpdate modifiers)
  where doUpdate l ma = loop l
          where loop []         = unsafeFreeze ma
                loop ((i,v):xs) = write ma i v >> loop xs
                {-# INLINE loop #-}
        {-# INLINE doUpdate #-}

unsafeUpdate :: PrimType ty
             => UArray ty
             -> [(Int, ty)]
             -> UArray ty
unsafeUpdate array modifiers = runST (thaw array >>= doUpdate modifiers)
  where doUpdate l ma = loop l
          where loop []         = unsafeFreeze ma
                loop ((i,v):xs) = unsafeWrite ma i v >> loop xs
                {-# INLINE loop #-}
        {-# INLINE doUpdate #-}

withPtr :: PrimType ty
        => UArray ty
        -> (Ptr ty -> IO a)
        -> IO a
withPtr (UVecAddr _ fptr)  f = withFinalPtr fptr f
withPtr (UVecBA pstatus a) f
    | isPinned pstatus = f $ Ptr (byteArrayContents# a)
    | otherwise        = do
        let !sz# = sizeofByteArray# a
        a' <- primitive $ \s -> do
            case newAlignedPinnedByteArray# sz# 8# s of { (# s2, mba #) ->
            case copyByteArray# a 0# mba 0# sz# s2 of { s3 ->
            case unsafeFreezeByteArray# mba s3 of { (# s4, ba #) ->
                (# s4, Ptr (byteArrayContents# ba) #) }}}
        f a'
withPtr v@(UVecSlice start _ parent) f =
    withPtr parent $ \ptr -> f (ptr `plusPtr` (start * sizeInBytesOfContent v))

withMutablePtr :: PrimType ty
               => MUArray ty RealWorld
               -> (Ptr ty -> IO a)
               -> IO a
withMutablePtr muvec f = do
    v <- unsafeFreeze muvec
    withPtr v f

unsafeRecast :: (PrimType a, PrimType b) => UArray a -> UArray b
unsafeRecast (UVecBA i b) = UVecBA i b
unsafeRecast (UVecAddr len a) = UVecAddr len (castFinalPtr a)
unsafeRecast (UVecSlice start end parent) = sliceRecast Proxy Proxy parent
  where
    sliceRecast :: (PrimType a, PrimType b) => Proxy b -> Proxy a -> UArray a -> UArray b
    sliceRecast dstProxy srcProxy _ =
        let szB = primSizeInBytes dstProxy
            szA = primSizeInBytes srcProxy
         in UVecSlice ((start * szA) `div` szB) ((end * szA) `div` szB) (unsafeRecast parent)

null :: UArray ty -> Bool
null (UVecBA _ a) = bool# (sizeofByteArray# a ==# 0#)
null (UVecAddr l _) = l == 0
null (UVecSlice start end _) = start == end

take :: PrimType ty => Int -> UArray ty -> UArray ty
take nbElems v
    | nbElems <= 0 = empty
    | n == len     = v
    | otherwise    =
        case v of
            UVecSlice start _ parent -> UVecSlice start (start + n) parent
            _                        -> UVecSlice 0 n v
  where
    n = min nbElems len
    len = length v

drop :: PrimType ty => Int -> UArray ty -> UArray ty
drop nbElems v
    | nbElems <= 0 = v
    | n == len     = empty
    | otherwise    =
        case v of
            UVecSlice start end parent -> UVecSlice (start + n) end parent
            _                          -> UVecSlice n len v
  where
    n = min nbElems len
    len = length v

splitAt :: PrimType ty => Int -> UArray ty -> (UArray ty, UArray ty)
splitAt nbElems v
    | nbElems <= 0  = (empty, v)
    | n == length v = (v, empty)
    | otherwise     =
        case v of
            UVecSlice start end parent -> (UVecSlice start (start + n) parent, UVecSlice (start + n) end parent)
            _                          -> (UVecSlice 0 n v, UVecSlice n (length v) v)
  where
    n = min nbElems (length v)

revTake :: PrimType ty => Int -> UArray ty -> UArray ty
revTake nbElems v = drop (length v - nbElems) v

revDrop :: PrimType ty => Int -> UArray ty -> UArray ty
revDrop nbElems v = take (length v - nbElems) v

revSplitAt :: PrimType ty => Int -> UArray ty -> (UArray ty, UArray ty)
revSplitAt n v = (drop idx v, take idx v)
  where idx = length v - n

splitOn :: PrimType ty => (ty -> Bool) -> UArray ty -> [UArray ty]
splitOn xpredicate ivec
    | len == 0  = []
    | otherwise = runST $ unsafeIndexer ivec (go ivec xpredicate)
  where
    !len = length ivec
    go :: PrimType ty => UArray ty -> (ty -> Bool) -> (Int -> ty) -> ST s [UArray ty]
    go v predicate getIdx = return (loop 0 0)
      where
        loop !prevIdx !idx
            | idx == len = [sub v prevIdx idx]
            | otherwise  =
                let e = getIdx idx
                    idx' = idx + 1
                 in if predicate e
                        then sub v prevIdx idx : loop idx' idx'
                        else loop prevIdx idx'

sub :: PrimType ty => UArray ty -> Int -> Int -> UArray ty
sub vec startIdx expectedEndIdx
    | startIdx >= endIdx = empty
    | otherwise          =
        case vec of
            UVecSlice start _ parent -> UVecSlice (start+startIdx) (start+endIdx) parent
            _                        -> UVecSlice startIdx expectedEndIdx vec
  where
    endIdx = min expectedEndIdx len
    len = length vec
{-
sub :: (PrimType ty, PrimMonad prim) => UArray ty -> Int -> Int -> prim (UArray ty)
sub vec startIdx expectedEndIdx
    | startIdx == endIdx     = return empty
    | startIdx >= length vec = return empty
    | otherwise              = do
        muv <- newNative (endIdx - startIdx) $ \mba ->
            case vec of
                UVecBA _ ba      -> primitive $ \s -> (# copyByteArray# ba start mba 0# (end -# start) s, () #)
                UVecAddr _ fptr  ->
                    withFinalPtr fptr $ \(Ptr addr) -> primitive $ \s ->
                        (# compatCopyAddrToByteArray# addr mba 0# (end -# start) s, () #)
                UVecSlice startSlice _ parent ->
                    -- copy from parent at index start+startIdx until index end-endIdx
                    copyAtRO (MUVecMA unpinned mba) 0 parent (startIdx + startSlice) (endIdx - startIdx)
        unsafeFreeze muv
  where
    endIdx = min expectedEndIdx (length vec)
    !(I# end) = endIdx * bytes
    !(I# start) = startIdx * bytes
    bytes = sizeInBytesOfContent vec
    -}

break :: PrimType ty => (ty -> Bool) -> UArray ty -> (UArray ty, UArray ty)
break xpredicate iniv = runST $ unsafeIndexer iniv (go iniv xpredicate)
  where
    !len = length iniv
    go :: PrimType ty => UArray ty -> (ty -> Bool) -> (Int -> ty) -> ST s (UArray ty, UArray ty)
    go v predicate getIdx = return (findBreak 0)
      where
        findBreak !i
            | i == len             = (v, empty)
            | predicate (getIdx i) = splitAt i v
            | otherwise            = findBreak (i+1)

span :: PrimType ty => (ty -> Bool) -> UArray ty -> (UArray ty, UArray ty)
span p = break (not . p)

map :: (PrimType a, PrimType b) => (a -> b) -> UArray a -> UArray b
map f a = create (length a) (\i -> f $ unsafeIndex a i)

mapIndex :: (PrimType a, PrimType b) => (Int -> a -> b) -> UArray a -> UArray b
mapIndex f a = create (length a) (\i -> f i $ unsafeIndex a i)

cons :: PrimType ty => ty -> UArray ty -> UArray ty
cons e vec = runST $ do
    muv <- newNative (len + 1) $ \mba ->
        case vec of
            UVecBA _ ba      -> primCopyFreezedBytesOffset mba bytes ba (len# *# bytes)
            UVecAddr _ fptr  ->
                withFinalPtr fptr $ \(Ptr addr) -> primitive $ \s ->
                    (# compatCopyAddrToByteArray# addr mba bytes (len# *# bytes) s, () #)
            UVecSlice start _ parent ->
                copyAtRO (MUVecMA unpinned mba) 1 parent start len
    unsafeWrite muv 0 e
    unsafeFreeze muv
  where
    !(I# bytes) = sizeInBytesOfContent vec
    !len@(I# len#) = length vec

snoc :: PrimType ty => UArray ty -> ty -> UArray ty
snoc vec e = runST $ do
    muv <- newNative (len + 1) $ \mba ->
        case vec of
            UVecBA _ ba      -> primCopyFreezedBytes mba ba
            UVecAddr _ fptr  ->
                withFinalPtr fptr $ \(Ptr addr) -> primitive $ \s ->
                    (# compatCopyAddrToByteArray# addr mba 0# (len# *# bytes) s, () #)
            UVecSlice start _ parent ->
                copyAtRO (MUVecMA unpinned mba) 0 parent start len
    unsafeWrite muv len e
    unsafeFreeze muv
  where
    !(I# bytes) = sizeInBytesOfContent vec
    !len@(I# len#) = length vec

uncons :: PrimType ty => UArray ty -> Maybe (ty, UArray ty)
uncons vec
    | nbElems == 0 = Nothing
    | otherwise    = Just (unsafeIndex vec 0, sub vec 1 nbElems)
  where
    !nbElems = length vec

unsnoc :: PrimType ty => UArray ty -> Maybe (UArray ty, ty)
unsnoc vec
    | nbElems == 0 = Nothing
    | otherwise    = Just (sub vec 0 lastElem, unsafeIndex vec lastElem)
  where
    !lastElem = nbElems - 1
    !nbElems = length vec

find :: PrimType ty => (ty -> Bool) -> UArray ty -> Maybe ty
find predicate vec = loop 0
  where
    !len = length vec
    loop i
        | i == len  = Nothing
        | otherwise =
            let e = unsafeIndex vec i
             in if predicate e then Just e else loop (i+1)

sortBy :: PrimType ty => (ty -> ty -> Ordering) -> UArray ty -> UArray ty
sortBy xford vec = runST (thaw vec >>= doSort xford)
  where
    len = length vec
    doSort :: (PrimType ty, PrimMonad prim) => (ty -> ty -> Ordering) -> MUArray ty (PrimState prim) -> prim (UArray ty)
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

filter :: PrimType ty => (ty -> Bool) -> UArray ty -> UArray ty
filter predicate vec = vFromList $ Data.List.filter predicate $ vToList vec

reverse :: PrimType ty => UArray ty -> UArray ty
reverse a = create len toEnd
  where
    len = length a
    toEnd i = unsafeIndex a (len - i - 1)

foldl :: PrimType ty => (a -> ty -> a) -> a -> UArray ty -> a
foldl f initialAcc vec = loop 0 initialAcc
  where
    len = length vec
    loop i acc
        | i == len  = acc
        | otherwise = loop (i+1) (f acc (unsafeIndex vec i))

foldr :: PrimType ty => (ty -> a -> a) -> a -> UArray ty -> a
foldr f initialAcc vec = loop 0
  where
    len = length vec
    loop i
        | i == len  = initialAcc
        | otherwise = unsafeIndex vec i `f` loop (i+1)

foldl' :: PrimType ty => (a -> ty -> a) -> a -> UArray ty -> a
foldl' f initialAcc vec = loop 0 initialAcc
  where
    len = length vec
    loop i !acc
        | i == len  = acc
        | otherwise = loop (i+1) (f acc (unsafeIndex vec i))
