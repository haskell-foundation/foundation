-- |
-- Module      : Foundation.Array.Unboxed
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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module Foundation.Array.Unboxed
    ( UArray(..)
    , PrimType(..)
    -- * methods
    , copy
    , unsafeCopyAtRO
    -- * internal methods
    -- , copyAddr
    , recast
    , unsafeRecast
    , length
    , freeze
    , unsafeFreeze
    , thaw
    , unsafeThaw
    -- * Creation
    , new
    , create
    , createFromIO
    , createFromPtr
    , sub
    , copyToPtr
    , withPtr
    , withMutablePtr
    , unsafeFreezeShrink
    , freezeShrink
    , unsafeSlide
    -- * accessors
    , update
    , unsafeUpdate
    , unsafeIndex
    , unsafeIndexer
    , unsafeDewrap
    , unsafeRead
    , unsafeWrite
    -- * Functions
    , equalMemcmp
    , singleton
    , replicate
    , map
    , mapIndex
    , findIndex
    , index
    , null
    , take
    , unsafeTake
    , drop
    , unsafeDrop
    , splitAt
    , revDrop
    , revTake
    , revSplitAt
    , splitOn
    , splitElem
    , break
    , breakElem
    , elem
    , indices
    , intersperse
    , span
    , cons
    , snoc
    , uncons
    , unsnoc
    , find
    , sortBy
    , filter
    , reverse
    , replace
    , foldr
    , foldl'
    , foldr1
    , foldl1'
    , all
    , any
    , foreignMem
    , fromForeignPtr
    , builderAppend
    , builderBuild
    , builderBuild_
    , toHexadecimal
    , toBase64Internal
    ) where

import           Control.Monad (when)
import           GHC.Prim
import           GHC.Types
import           GHC.Word
import           GHC.ST
import           GHC.Ptr
import           GHC.ForeignPtr (ForeignPtr)
import           Foreign.Marshal.Utils (copyBytes)
import           Foundation.Internal.Base
import           Foundation.Internal.Primitive
import           Foundation.Internal.Proxy
import           Foundation.Primitive.Types.OffsetSize
import           Foundation.Internal.MonadTrans
import           Foundation.Collection.NonEmpty
import           Foundation.Primitive.Monad
import           Foundation.Primitive.Types
import           Foundation.Primitive.FinalPtr
import           Foundation.Primitive.Utils
import           Foundation.Primitive.Exception
import           Foundation.Primitive.UArray.Base
import           Foundation.Array.Unboxed.Mutable hiding (sub, copyToPtr)
import           Foundation.Numerical
import           Foundation.Boot.Builder
import qualified Foundation.Boot.List as List
import qualified Foundation.Primitive.Base16 as Base16
import qualified Foundation.Primitive.UArray.BA as PrimBA
import qualified Foundation.Primitive.UArray.Addr as PrimAddr

-- | Copy every cells of an existing array to a new array
copy :: PrimType ty => UArray ty -> UArray ty
copy array = runST (thaw array >>= unsafeFreeze)

-- | Thaw an array to a mutable array.
--
-- the array is not modified, instead a new mutable array is created
-- and every values is copied, before returning the mutable array.
thaw :: (PrimMonad prim, PrimType ty) => UArray ty -> prim (MUArray ty (PrimState prim))
thaw array = do
    ma <- new (length array)
    unsafeCopyAtRO ma azero array (Offset 0) (length array)
    pure ma
{-# INLINE thaw #-}

-- | Return the element at a specific index from an array.
--
-- If the index @n is out of bounds, an error is raised.
index :: PrimType ty => UArray ty -> Offset ty -> ty
index array n
    | isOutOfBound n len = outOfBound OOB_Index n len
    | otherwise          = unsafeIndex array n
  where
    !len = length array
{-# INLINE index #-}

foreignMem :: PrimType ty
           => FinalPtr ty -- ^ the start pointer with a finalizer
           -> CountOf ty  -- ^ the number of elements (in elements, not bytes)
           -> UArray ty
foreignMem fptr nb = UArrayAddr (Offset 0) nb fptr

fromForeignPtr :: PrimType ty
               => (ForeignPtr ty, Int, Int) -- ForeignPtr, an offset in prim elements, a size in prim elements
               -> UArray ty
fromForeignPtr (fptr, ofs, len)   = UArrayAddr (Offset ofs) (CountOf len) (toFinalPtrForeign fptr)


-- | Allocate a new array with a fill function that has access to the elements of
--   the source array.
unsafeCopyFrom :: (PrimType a, PrimType b)
               => UArray a -- ^ Source array
               -> CountOf b -- ^ Length of the destination array
               -> (UArray a -> Offset a -> MUArray b s -> ST s ())
               -- ^ Function called for each element in the source array
               -> ST s (UArray b) -- ^ Returns the filled new array
unsafeCopyFrom v' newLen f = new newLen >>= fill 0 >>= unsafeFreeze
  where len = length v'
        fill i r'
            | i .==# len = pure r'
            | otherwise  = do f v' i r'
                              fill (i + 1) r'

freeze :: (PrimType ty, PrimMonad prim) => MUArray ty (PrimState prim) -> prim (UArray ty)
freeze ma = do
    ma' <- new len
    copyAt ma' (Offset 0) ma (Offset 0) len
    unsafeFreeze ma'
  where len = CountOf $ mutableLength ma

freezeShrink :: (PrimType ty, PrimMonad prim) => MUArray ty (PrimState prim) -> CountOf ty -> prim (UArray ty)
freezeShrink ma n = do
    ma' <- new n
    copyAt ma' (Offset 0) ma (Offset 0) n
    unsafeFreeze ma'

unsafeSlide :: (PrimType ty, PrimMonad prim) => MUArray ty (PrimState prim) -> Offset ty -> Offset ty -> prim ()
unsafeSlide mua s e = doSlide mua s e
  where
    doSlide :: (PrimType ty, PrimMonad prim) => MUArray ty (PrimState prim) -> Offset ty -> Offset ty -> prim ()
    doSlide (MUArrayMBA mbStart _ mba) start end  =
        primMutableByteArraySlideToStart mba (offsetInBytes $ mbStart+start) (offsetInBytes end)
    doSlide (MUArrayAddr mbStart _ fptr) start end = withFinalPtr fptr $ \(Ptr addr) ->
        primMutableAddrSlideToStart addr (offsetInBytes $ mbStart+start) (offsetInBytes end)

-- | Create a new array of size @n by settings each cells through the
-- function @f.
create :: forall ty . PrimType ty
       => CountOf ty           -- ^ the size of the array
       -> (Offset ty -> ty) -- ^ the function that set the value at the index
       -> UArray ty         -- ^ the array created
create n initializer
    | n == 0    = mempty
    | otherwise = runST (new n >>= iter initializer)
  where
    iter :: (PrimType ty, PrimMonad prim) => (Offset ty -> ty) -> MUArray ty (PrimState prim) -> prim (UArray ty)
    iter f ma = loop 0
      where
        loop i
            | i .==# n  = unsafeFreeze ma
            | otherwise = unsafeWrite ma i (f i) >> loop (i+1)
        {-# INLINE loop #-}
    {-# INLINE iter #-}

-- | Create a pinned array that is filled by a 'filler' function (typically an IO call like hGetBuf)
createFromIO :: PrimType ty
             => CountOf ty                  -- ^ the size of the array
             -> (Ptr ty -> IO (CountOf ty)) -- ^ filling function that
             -> IO (UArray ty)
createFromIO size filler
    | size == 0 = pure mempty
    | otherwise = do
        mba <- newPinned size
        r   <- withMutablePtr mba $ \p -> filler p
        case r of
            0             -> pure mempty -- make sure we don't keep our array referenced by using empty
            _ | r < 0     -> error "filler returned negative number"
              | otherwise -> unsafeFreezeShrink mba r

-- | Freeze a chunk of memory pointed, of specific size into a new unboxed array
createFromPtr :: PrimType ty
              => Ptr ty
              -> CountOf ty
              -> IO (UArray ty)
createFromPtr p s = do
    ma <- new s
    copyFromPtr p s ma
    unsafeFreeze ma

-----------------------------------------------------------------------
-- higher level collection implementation
-----------------------------------------------------------------------

singleton :: PrimType ty => ty -> UArray ty
singleton ty = create 1 (const ty)

replicate :: PrimType ty => CountOf ty -> ty -> UArray ty
replicate sz ty = create sz (const ty)

-- | update an array by creating a new array with the updates.
--
-- the operation copy the previous array, modify it in place, then freeze it.
update :: PrimType ty
       => UArray ty
       -> [(Offset ty, ty)]
       -> UArray ty
update array modifiers = runST (thaw array >>= doUpdate modifiers)
  where doUpdate l ma = loop l
          where loop []         = unsafeFreeze ma
                loop ((i,v):xs) = write ma i v >> loop xs
                {-# INLINE loop #-}
        {-# INLINE doUpdate #-}

unsafeUpdate :: PrimType ty
             => UArray ty
             -> [(Offset ty, ty)]
             -> UArray ty
unsafeUpdate array modifiers = runST (thaw array >>= doUpdate modifiers)
  where doUpdate l ma = loop l
          where loop []         = unsafeFreeze ma
                loop ((i,v):xs) = unsafeWrite ma i v >> loop xs
                {-# INLINE loop #-}
        {-# INLINE doUpdate #-}

-- | Copy all the block content to the memory starting at the destination address
copyToPtr :: forall ty prim . (PrimType ty, PrimMonad prim)
          => UArray ty -- ^ the source array to copy
          -> Ptr ty    -- ^ The destination address where the copy is going to start
          -> prim ()
copyToPtr arr dst@(Ptr dst#) = onBackendPrim copyBa copyPtr arr
  where
    !(Offset os@(I# os#)) = offsetInBytes $ offset arr
    !(CountOf szBytes@(I# szBytes#)) = sizeInBytes $ length arr
    copyBa ba = primitive $ \s1 -> (# compatCopyByteArrayToAddr# ba os# dst# szBytes# s1, () #)
    copyPtr fptr = unsafePrimFromIO $ withFinalPtr fptr $ \ptr -> copyBytes dst (ptr `plusPtr` os) szBytes

withPtr :: forall ty prim a . (PrimMonad prim, PrimType ty)
        => UArray ty
        -> (Ptr ty -> prim a)
        -> prim a
withPtr a f
    | isPinned a == Pinned =
        onBackendPrim (\ba -> f (Ptr (byteArrayContents# ba) `plusPtr` os))
                      (\fptr -> withFinalPtr fptr $ \ptr -> f (ptr `plusPtr` os))
                      a
    | otherwise = do
        arr <- do
            trampoline <- newPinned (length a)
            unsafeCopyAtRO trampoline 0 a 0 (length a)
            unsafeFreeze trampoline
        r <- withPtr arr f
        touch arr
        pure r
  where
    !sz          = primSizeInBytes (Proxy :: Proxy ty)
    !(Offset os) = offsetOfE sz $ offset a
{-# INLINE withPtr #-}

-- | Recast an array of type a to an array of b
--
-- a and b need to have the same size otherwise this
-- raise an async exception
recast :: forall a b . (PrimType a, PrimType b) => UArray a -> UArray b
recast array
    | aTypeSize == bTypeSize = unsafeRecast array
    | missing   == 0         = unsafeRecast array
    | otherwise = throw $ InvalidRecast
                      (RecastSourceSize      alen)
                      (RecastDestinationSize $ alen + missing)
  where
    aTypeSize = primSizeInBytes (Proxy :: Proxy a)
    bTypeSize@(CountOf bs) = primSizeInBytes (Proxy :: Proxy b)
    (CountOf alen) = sizeInBytes (length array)
    missing = alen `mod` bs

unsafeRecast :: (PrimType a, PrimType b) => UArray a -> UArray b
unsafeRecast (UArrayBA start len b) = UArrayBA (primOffsetRecast start) (sizeRecast len) b
unsafeRecast (UArrayAddr start len a) = UArrayAddr (primOffsetRecast start) (sizeRecast len) (castFinalPtr a)
{-# INLINE [1] unsafeRecast #-}
{-# RULES "unsafeRecast from Word8" [2] forall a . unsafeRecast a = unsafeRecastBytes a #-}

unsafeRecastBytes :: PrimType a => UArray Word8 -> UArray a
unsafeRecastBytes (UArrayBA start len b) = UArrayBA (primOffsetRecast start) (sizeRecast len) b
unsafeRecastBytes (UArrayAddr start len a) = UArrayAddr (primOffsetRecast start) (sizeRecast len) (castFinalPtr a)
{-# INLINE [1] unsafeRecastBytes #-}

null :: UArray ty -> Bool
null arr = length arr == 0

-- | Take a count of elements from the array and create an array with just those elements
take :: PrimType ty => CountOf ty -> UArray ty -> UArray ty
take n v
    | n <= 0    = mempty
    | n >= vlen = v
    | otherwise =
        case v of
            UArrayBA start _ ba     -> UArrayBA start n ba
            UArrayAddr start _ fptr -> UArrayAddr start n fptr
  where
    vlen = length v

unsafeTake :: PrimType ty => CountOf ty -> UArray ty -> UArray ty
unsafeTake sz (UArrayBA start _ ba)     = UArrayBA start sz ba
unsafeTake sz (UArrayAddr start _ fptr) = UArrayAddr start sz fptr

-- | Drop a count of elements from the array and return the new array minus those dropped elements
drop :: PrimType ty => CountOf ty -> UArray ty -> UArray ty
drop n v
    | n <= 0    = v
    | n >= vlen = mempty
    | otherwise =
        case v of
            UArrayBA start len ba     -> UArrayBA (start `offsetPlusE` n) (len - n) ba
            UArrayAddr start len fptr -> UArrayAddr (start `offsetPlusE` n) (len - n) fptr
  where
    vlen = length v

unsafeDrop :: PrimType ty => CountOf ty -> UArray ty -> UArray ty
unsafeDrop n (UArrayBA start sz ba)     = UArrayBA (start `offsetPlusE` sz) (sz `sizeSub` n) ba
unsafeDrop n (UArrayAddr start sz fptr) = UArrayAddr (start `offsetPlusE` sz) (sz `sizeSub` n) fptr

-- | Split an array into two, with a count of at most N elements in the first one
-- and the remaining in the other.
splitAt :: PrimType ty => CountOf ty -> UArray ty -> (UArray ty, UArray ty)
splitAt nbElems v
    | nbElems <= 0 = (mempty, v)
    | n == vlen    = (v, mempty)
    | otherwise    =
        case v of
            UArrayBA start len ba     -> ( UArrayBA start n ba     , UArrayBA (start `offsetPlusE` n) (len - n) ba)
            UArrayAddr start len fptr -> ( UArrayAddr start n fptr , UArrayAddr (start `offsetPlusE` n) (len - n) fptr)
  where
    n    = min nbElems vlen
    vlen = length v

splitElem :: PrimType ty => ty -> UArray ty -> (UArray ty, UArray ty)
splitElem !ty arr = onBackend goBa (\fptr -> pure . goAddr fptr) arr
  where
    !len = length arr
    !start = offset arr
    !end = start `offsetPlusE` len
    goBa ba
        | k == end   = (arr, mempty)
        | k == start = (mempty, arr)
        | otherwise  = ( UArrayBA start (offsetAsSize k - offsetAsSize start) ba
                       , UArrayBA k     (len - (offsetAsSize k - offsetAsSize start)) ba)
      where !k = PrimBA.findIndexElem ty ba start (start `offsetPlusE` len)
    goAddr fptr (Ptr addr)
        | k == end   = (arr, mempty)
        | k == start = (mempty, arr)
        | otherwise  = ( UArrayAddr start (offsetAsSize k - offsetAsSize start) fptr
                       , UArrayAddr k     (len - (offsetAsSize k - offsetAsSize start)) fptr)
      where !k = PrimAddr.findIndexElem ty addr start (start `offsetPlusE` len)
{-# SPECIALIZE [3] splitElem :: Word8 -> UArray Word8 -> (UArray Word8, UArray Word8) #-}
{-# SPECIALIZE [3] splitElem :: Word32 -> UArray Word32 -> (UArray Word32, UArray Word32) #-}

-- inverse a CountOf that is specified from the end (e.g. take n elements from the end)
countFromStart :: UArray ty -> CountOf ty -> CountOf ty
countFromStart v sz@(CountOf sz')
    | sz >= len = CountOf 0
    | otherwise = CountOf (len' - sz')
  where len@(CountOf len') = length v

-- | Take the N elements from the end of the array
revTake :: PrimType ty => CountOf ty -> UArray ty -> UArray ty
revTake n v = drop (countFromStart v n) v

-- | Drop the N elements from the end of the array
revDrop :: PrimType ty => CountOf ty -> UArray ty -> UArray ty
revDrop n v = take (countFromStart v n) v

-- | Split an array at the N element from the end, and return
-- the last N elements in the first part of the tuple, and whatever first
-- elements remaining in the second
revSplitAt :: PrimType ty => CountOf ty -> UArray ty -> (UArray ty, UArray ty)
revSplitAt n v = (drop sz v, take sz v) where sz = countFromStart v n

splitOn :: PrimType ty => (ty -> Bool) -> UArray ty -> [UArray ty]
splitOn xpredicate ivec
    | len == 0  = [mempty]
    | otherwise = runST $ unsafeIndexer ivec (pureST . go ivec xpredicate)
  where
    !len = length ivec
    go v predicate getIdx = loop 0 0
      where
        loop !prevIdx !idx
            | idx .==# len = [sub v prevIdx idx]
            | otherwise    =
                let e = getIdx idx
                    idx' = idx + 1
                 in if predicate e
                        then sub v prevIdx idx : loop idx' idx'
                        else loop prevIdx idx'
    {-# INLINE go #-}

sub :: PrimType ty => UArray ty -> Offset ty -> Offset ty -> UArray ty
sub vec startIdx expectedEndIdx
    | startIdx >= endIdx = mempty
    | otherwise          =
        case vec of
            UArrayBA start _ ba     -> UArrayBA (start + startIdx) newLen ba
            UArrayAddr start _ fptr -> UArrayAddr (start + startIdx) newLen fptr
  where
    newLen = endIdx - startIdx
    endIdx = min expectedEndIdx (0 `offsetPlusE` len)
    len = length vec

findIndex :: forall ty . PrimType ty => ty -> UArray ty -> Maybe (Offset ty)
findIndex tyOuter ba = runST $ unsafeIndexer ba (go tyOuter)
  where
    !len = length ba

    go :: PrimType ty => ty -> (Offset ty -> ty) -> ST s (Maybe (Offset ty))
    go ty getIdx = loop (Offset 0)
      where
        loop ofs
            | ofs .==# len     = pure Nothing
            | getIdx ofs == ty = pure $ Just ofs
            | otherwise        = loop (ofs + Offset 1)
{-# SPECIALIZE [3] findIndex :: Word8 -> UArray Word8 -> Maybe (Offset Word8) #-}

break :: forall ty . PrimType ty => (ty -> Bool) -> UArray ty -> (UArray ty, UArray ty)
break xpredicate xv
    | len == 0  = (mempty, mempty)
    | otherwise = runST $ unsafeIndexer xv (go xv xpredicate)
  where
    !len = length xv
    go :: PrimType ty => UArray ty -> (ty -> Bool) -> (Offset ty -> ty) -> ST s (UArray ty, UArray ty)
    go v predicate getIdx = pure (findBreak $ Offset 0)
      where
        findBreak !i
            | i .==# len           = (v, mempty)
            | predicate (getIdx i) = splitAt (offsetAsSize i) v
            | otherwise            = findBreak (i + Offset 1)
        {-# INLINE findBreak #-}
    {-# INLINE go #-}
{-# NOINLINE [2] break #-}
{-# SPECIALIZE [2] break :: (Word8 -> Bool) -> UArray Word8 -> (UArray Word8, UArray Word8) #-}

{-
{-# RULES "break (== ty)" [3] forall (x :: forall ty . PrimType ty => ty) . break (== x) = breakElem x #-}
{-# RULES "break (ty ==)" [3] forall (x :: forall ty . PrimType ty => ty) . break (x ==) = breakElem x #-}
{-# RULES "break (== ty)" [3] forall (x :: Word8) . break (== x) = breakElem x #-}
-}

breakElem :: PrimType ty => ty -> UArray ty -> (UArray ty, UArray ty)
breakElem xelem xv = splitElem xelem xv
{-# SPECIALIZE [2] breakElem :: Word8 -> UArray Word8 -> (UArray Word8, UArray Word8) #-}
{-# SPECIALIZE [2] breakElem :: Word32 -> UArray Word32 -> (UArray Word32, UArray Word32) #-}

elem :: PrimType ty => ty -> UArray ty -> Bool
elem !ty arr = onBackend goBa (\_ -> pure . goAddr) arr /= end
  where
    !start = offset arr
    !end = start `offsetPlusE` length arr
    goBa ba = PrimBA.findIndexElem ty ba start end
    goAddr (Ptr addr) = PrimAddr.findIndexElem ty addr start end
{-# SPECIALIZE [2] elem :: Word8 -> UArray Word8 -> Bool #-}

intersperse :: forall ty . PrimType ty => ty -> UArray ty -> UArray ty
intersperse sep v
    | len <= 1  = v
    | otherwise = runST $ unsafeCopyFrom v newSize (go sep)
  where
    len = length v
    newSize = (scale (2:: Word) len) - 1

    go :: PrimType ty => ty -> UArray ty -> Offset ty -> MUArray ty s -> ST s ()
    go sep' oldV oldI newV
        | oldI .==# (len - 1) = unsafeWrite newV newI e
        | otherwise           = do
            unsafeWrite newV newI e
            unsafeWrite newV (newI + 1) sep'
      where
        e = unsafeIndex oldV oldI
        newI = scale (2 :: Word) oldI

span :: PrimType ty => (ty -> Bool) -> UArray ty -> (UArray ty, UArray ty)
span p = break (not . p)

map :: (PrimType a, PrimType b) => (a -> b) -> UArray a -> UArray b
map f a = create lenB (\i -> f $ unsafeIndex a (offsetCast Proxy i))
  where !lenB = sizeCast (Proxy :: Proxy (a -> b)) (length a)

mapIndex :: (PrimType a, PrimType b) => (Offset b -> a -> b) -> UArray a -> UArray b
mapIndex f a = create (sizeCast Proxy $ length a) (\i -> f i $ unsafeIndex a (offsetCast Proxy i))

cons :: PrimType ty => ty -> UArray ty -> UArray ty
cons e vec
    | len == CountOf 0 = singleton e
    | otherwise     = runST $ do
        muv <- new (len + 1)
        unsafeCopyAtRO muv 1 vec 0 len
        unsafeWrite muv 0 e
        unsafeFreeze muv
  where
    !len = length vec

snoc :: PrimType ty => UArray ty -> ty -> UArray ty
snoc vec e
    | len == CountOf 0 = singleton e
    | otherwise     = runST $ do
        muv <- new (len + CountOf 1)
        unsafeCopyAtRO muv (Offset 0) vec (Offset 0) len
        unsafeWrite muv (0 `offsetPlusE` length vec) e
        unsafeFreeze muv
  where
     !len = length vec

uncons :: PrimType ty => UArray ty -> Maybe (ty, UArray ty)
uncons vec
    | nbElems == 0 = Nothing
    | otherwise    = Just (unsafeIndex vec 0, sub vec 1 (0 `offsetPlusE` nbElems))
  where
    !nbElems = length vec

unsnoc :: PrimType ty => UArray ty -> Maybe (UArray ty, ty)
unsnoc vec
    | nbElems == 0 = Nothing
    | otherwise    = Just (sub vec 0 lastElem, unsafeIndex vec lastElem)
  where
    !lastElem = 0 `offsetPlusE` (nbElems - 1)
    !nbElems = length vec

find :: PrimType ty => (ty -> Bool) -> UArray ty -> Maybe ty
find predicate vec = loop 0
  where
    !len = length vec
    loop i
        | i .==# len = Nothing
        | otherwise  =
            let e = unsafeIndex vec i
             in if predicate e then Just e else loop (i+1)

sortBy :: forall ty . PrimType ty => (ty -> ty -> Ordering) -> UArray ty -> UArray ty
sortBy xford vec
    | len == 0  = mempty
    | otherwise = runST (thaw vec >>= doSort xford)
  where
    len = length vec
    doSort :: (PrimType ty, PrimMonad prim) => (ty -> ty -> Ordering) -> MUArray ty (PrimState prim) -> prim (UArray ty)
    doSort ford ma = qsort 0 (sizeLastOffset len) >> unsafeFreeze ma
      where
        qsort lo hi
            | lo >= hi  = pure ()
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

filter :: forall ty . PrimType ty => (ty -> Bool) -> UArray ty -> UArray ty
filter predicate arr = runST $ do
    (newLen, ma) <- newNative (length arr) $ \mba ->
                case arr of
                    (UArrayAddr start len fptr) -> withFinalPtr fptr $ \(Ptr addr) ->
                                                    PrimAddr.filter predicate mba addr start (start `offsetPlusE` len)
                    (UArrayBA start len ba)     -> PrimBA.filter predicate mba ba start (start `offsetPlusE` len)
    unsafeFreezeShrink ma newLen

reverse :: PrimType ty => UArray ty -> UArray ty
reverse a
    | len == CountOf 0 = mempty
    | otherwise     = runST $ do
        ((), ma) <- newNative len $ \mba -> onBackendPrim (goNative mba)
                                                          (\fptr -> withFinalPtr fptr $ goAddr mba)
                                                          a
        unsafeFreeze ma
  where
    !len = length a
    !end = Offset 0 `offsetPlusE` len
    !start = offset a
    !endI = sizeAsOffset ((start + end) - Offset 1)

    goNative :: MutableByteArray# s -> ByteArray# -> ST s ()
    goNative !ma !ba = loop (Offset 0)
      where
        loop !i
            | i == end  = pure ()
            | otherwise = primMbaWrite ma i (primBaIndex ba (sizeAsOffset (endI - i))) >> loop (i+1)
    goAddr :: MutableByteArray# s -> Ptr ty -> ST s ()
    goAddr !ma (Ptr addr) = loop (Offset 0)
      where
        loop !i
            | i == end  = pure ()
            | otherwise = primMbaWrite ma i (primAddrIndex addr (sizeAsOffset (endI - i))) >> loop (i+1)
{-# SPECIALIZE [3] reverse :: UArray Word8 -> UArray Word8 #-}

-- Finds where are the insertion points when we search for a `needle`
-- within an `haystack`.
-- Throws an error in case `needle` is empty.
indices :: PrimType ty => UArray ty -> UArray ty -> [Offset ty]
indices needle hy
  | needleLen <= 0 = error "Foundation.Array.Unboxed.indices: needle is empty."
  | otherwise = case haystackLen < needleLen of
                  True  -> []
                  False -> go (Offset 0) []
  where
    !haystackLen = length hy

    !needleLen = length needle

    go currentOffset ipoints
      | (currentOffset `offsetPlusE` needleLen) > (sizeAsOffset haystackLen) = ipoints
      | otherwise =
        let matcher = take needleLen . drop (offsetAsSize currentOffset) $ hy
        in case matcher == needle of
             -- TODO: Move away from right-appending as it's gonna be slow.
             True  -> go (currentOffset `offsetPlusE` needleLen) (ipoints <> [currentOffset])
             False -> go (currentOffset + Offset 1) ipoints

-- | Replace all the occurrencies of `needle` with `replacement` in
-- the `haystack` string.
replace :: PrimType ty => UArray ty -> UArray ty -> UArray ty -> UArray ty
replace (needle :: UArray ty) replacement haystack = runST $ do
    case null needle of
      True -> error "Foundation.Array.Unboxed.replace: empty needle"
      False -> do
        let insertionPoints = indices needle haystack
        let !occs           = List.length insertionPoints
        let !newLen         = haystackLen - (multBy needleLen occs) + (multBy replacementLen occs)
        ms <- new newLen
        loop ms (Offset 0) (Offset 0) insertionPoints
  where

    multBy (CountOf x) y = CountOf (x * y)

    !needleLen = length needle

    !replacementLen = length replacement

    !haystackLen = length haystack

    -- Go through each insertion point and copy things over.
    -- We keep around the offset to the original string to
    -- be able to copy bytes which didn't change.
    loop :: PrimMonad prim
         => MUArray ty (PrimState prim)
         -> Offset ty
         -> Offset ty
         -> [Offset ty]
         -> prim (UArray ty)
    loop mba currentOffset offsetInOriginalString [] = do
      -- Finalise the string
      let !unchangedDataLen = sizeAsOffset haystackLen - offsetInOriginalString
      unsafeCopyAtRO mba currentOffset haystack offsetInOriginalString unchangedDataLen
      freeze mba
    loop mba currentOffset offsetInOriginalString (x:xs) = do
        -- 1. Copy from the old string.
        let !unchangedDataLen = (x - offsetInOriginalString)
        unsafeCopyAtRO mba currentOffset haystack offsetInOriginalString unchangedDataLen
        let !newOffset = currentOffset `offsetPlusE` unchangedDataLen
        -- 2. Copy the replacement.
        unsafeCopyAtRO mba newOffset replacement (Offset 0) replacementLen
        let !offsetInOriginalString' = offsetInOriginalString `offsetPlusE` unchangedDataLen `offsetPlusE` needleLen
        loop mba (newOffset `offsetPlusE` replacementLen) offsetInOriginalString' xs
{-# SPECIALIZE [3] replace :: UArray Word8 -> UArray Word8 -> UArray Word8 -> UArray Word8 #-}

foldr :: PrimType ty => (ty -> a -> a) -> a -> UArray ty -> a
foldr f initialAcc vec = loop 0
  where
    !len = length vec
    loop i
        | i .==# len = initialAcc
        | otherwise  = unsafeIndex vec i `f` loop (i+1)

foldl' :: PrimType ty => (a -> ty -> a) -> a -> UArray ty -> a
foldl' f initialAcc vec = loop 0 initialAcc
  where
    !len = length vec
    loop i !acc
        | i .==# len = acc
        | otherwise  = loop (i+1) (f acc (unsafeIndex vec i))

foldl1' :: PrimType ty => (ty -> ty -> ty) -> NonEmpty (UArray ty) -> ty
foldl1' f arr = let (initialAcc, rest) = splitAt 1 $ getNonEmpty arr
               in foldl' f (unsafeIndex initialAcc 0) rest

foldr1 :: PrimType ty => (ty -> ty -> ty) -> NonEmpty (UArray ty) -> ty
foldr1 f arr = let (initialAcc, rest) = revSplitAt 1 $ getNonEmpty arr
               in foldr f (unsafeIndex initialAcc 0) rest

all :: PrimType ty => (ty -> Bool) -> UArray ty -> Bool
all p uv = loop 0
  where
    len = length uv
    loop !i
      | i .==# len = True
      | not $ p (unsafeIndex uv i) = False
      | otherwise = loop (i + 1)

any :: PrimType ty => (ty -> Bool) -> UArray ty -> Bool
any p uv = loop 0
  where
    len = length uv
    loop !i
      | i .==# len = False
      | p (unsafeIndex uv i) = True
      | otherwise = loop (i + 1)

builderAppend :: (PrimType ty, PrimMonad state) => ty -> Builder (UArray ty) (MUArray ty) ty state err ()
builderAppend v = Builder $ State $ \(i, st, e) ->
    if offsetAsSize i == chunkSize st
        then do
            cur      <- unsafeFreeze (curChunk st)
            newChunk <- new (chunkSize st)
            unsafeWrite newChunk 0 v
            pure ((), (Offset 1, st { prevChunks     = cur : prevChunks st
                                      , prevChunksSize = chunkSize st + prevChunksSize st
                                      , curChunk       = newChunk
                                      }, e))
        else do
            unsafeWrite (curChunk st) i v
            pure ((), (i + 1, st, e))

builderBuild :: (PrimType ty, PrimMonad m) => Int -> Builder (UArray ty) (MUArray ty) ty m err () -> m (Either err (UArray ty))
builderBuild sizeChunksI ab
    | sizeChunksI <= 0 = builderBuild 64 ab
    | otherwise        = do
        first         <- new sizeChunks
        ((), (i, st, e)) <- runState (runBuilder ab) (Offset 0, BuildingState [] (CountOf 0) first sizeChunks, Nothing)
        case e of
          Just err -> pure (Left err)
          Nothing -> do
            cur <- unsafeFreezeShrink (curChunk st) (offsetAsSize i)
            -- Build final array
            let totalSize = prevChunksSize st + offsetAsSize i
            bytes <- new totalSize >>= fillFromEnd totalSize (cur : prevChunks st) >>= unsafeFreeze
            pure (Right bytes)
  where
      sizeChunks = CountOf sizeChunksI

      fillFromEnd _   []     mua = pure mua
      fillFromEnd !end (x:xs) mua = do
          let sz = length x
          unsafeCopyAtRO mua (sizeAsOffset (end - sz)) x (Offset 0) sz
          fillFromEnd (end - sz) xs mua

builderBuild_ :: (PrimType ty, PrimMonad m) => Int -> Builder (UArray ty) (MUArray ty) ty m () () -> m (UArray ty)
builderBuild_ sizeChunksI ab = either (\() -> internalError "impossible output") id <$> builderBuild sizeChunksI ab

toHexadecimal :: PrimType ty => UArray ty -> UArray Word8
toHexadecimal ba
    | len == CountOf 0 = mempty
    | otherwise     = runST $ do
        ma <- new (len `scale` 2)
        unsafeIndexer b8 (go ma)
        unsafeFreeze ma
  where
    b8 = unsafeRecast ba
    !len = length b8
    !endOfs = Offset 0 `offsetPlusE` len

    go :: MUArray Word8 s -> (Offset Word8 -> Word8) -> ST s ()
    go !ma !getAt = loop 0 0
      where
        loop !dIdx !sIdx
            | sIdx == endOfs = pure ()
            | otherwise      = do
                let !(W8# !w)      = getAt sIdx
                    (# wHi, wLo #) = Base16.unsafeConvertByte w
                unsafeWrite ma dIdx     (W8# wHi)
                unsafeWrite ma (dIdx+1) (W8# wLo)
                loop (dIdx + 2) (sIdx+1)

toBase64Internal :: PrimType ty => Addr# -> UArray ty -> Bool -> UArray Word8
toBase64Internal table src padded
    | len == CountOf 0 = mempty
    | otherwise = runST $ do
        ma <- new dstLen
        unsafeIndexer b8 (go ma)
        unsafeFreeze ma
  where
    b8 = unsafeRecast src
    !len = length b8
    !dstLen = outputLengthBase64 padded len
    !endOfs = Offset 0 `offsetPlusE` len
    !dstEndOfs = Offset 0 `offsetPlusE` dstLen

    go :: MUArray Word8 s -> (Offset Word8 -> Word8) -> ST s ()
    go !ma !getAt = loop 0 0
      where
        eqChar = 0x3d :: Word8

        loop !sIdx !dIdx
            | sIdx == endOfs = when padded $ do
                when (dIdx `offsetPlusE` CountOf 1 <= dstEndOfs) $ unsafeWrite ma dIdx eqChar
                when (dIdx `offsetPlusE` CountOf 2 == dstEndOfs) $ unsafeWrite ma (dIdx `offsetPlusE` CountOf 1) eqChar
            | otherwise = do
                let !b2Idx = sIdx `offsetPlusE` CountOf 1
                    !b3Idx = sIdx `offsetPlusE` CountOf 2

                    !b2Available = b2Idx < endOfs
                    !b3Available = b3Idx < endOfs

                    !b1 = getAt sIdx
                    !b2 = if b2Available then getAt b2Idx else 0
                    !b3 = if b3Available then getAt b3Idx else 0

                    (w,x,y,z) = convert3 table b1 b2 b3

                    sNextIncr = 1 + fromEnum b2Available + fromEnum b3Available
                    dNextIncr = 1 + sNextIncr

                unsafeWrite ma dIdx w
                unsafeWrite ma (dIdx `offsetPlusE` CountOf 1) x

                when b2Available $ unsafeWrite ma (dIdx `offsetPlusE` CountOf 2) y
                when b3Available $ unsafeWrite ma (dIdx `offsetPlusE` CountOf 3) z

                loop (sIdx `offsetPlusE` CountOf sNextIncr) (dIdx `offsetPlusE` CountOf dNextIncr)

outputLengthBase64 :: Bool -> CountOf Word8 -> CountOf Word8
outputLengthBase64 padding (CountOf inputLenInt) = outputLength
  where
    outputLength = if padding then CountOf lenWithPadding else CountOf lenWithoutPadding
    lenWithPadding
        | m == 0    = 4 * d
        | otherwise = 4 * (d + 1)
    lenWithoutPadding
        | m == 0    = 4 * d
        | otherwise = 4 * d + m + 1
    (d,m) = inputLenInt `divMod` 3

convert3 :: Addr# -> Word8 -> Word8 -> Word8 -> (Word8, Word8, Word8, Word8)
convert3 table (W8# a) (W8# b) (W8# c) =
    let !w = narrow8Word# (uncheckedShiftRL# a 2#)
        !x = or# (and# (uncheckedShiftL# a 4#) 0x30##) (uncheckedShiftRL# b 4#)
        !y = or# (and# (uncheckedShiftL# b 2#) 0x3c##) (uncheckedShiftRL# c 6#)
        !z = and# c 0x3f##
     in (idx w, idx x, idx y, idx z)
  where
    idx :: Word# -> Word8
    idx i = W8# (indexWord8OffAddr# table (word2Int# i))
