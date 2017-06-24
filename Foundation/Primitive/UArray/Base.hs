{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Foundation.Primitive.UArray.Base
    ( MUArray(..)
    , UArray(..)
    -- * New mutable array creation
    , newUnpinned
    , newPinned
    , newNative
    , new
    -- * Pinning status
    , isPinned
    , isMutablePinned
    -- * Mutable array accessor
    , unsafeRead
    , unsafeWrite
    -- * Freezing routines
    , unsafeFreezeShrink
    , unsafeFreeze
    , unsafeThaw
    -- * Array accessor
    , unsafeIndex
    , unsafeIndexer
    , unsafeDewrap
    , unsafeDewrap2
    -- * Basic lowlevel functions
    , length
    , equal
    , equalMemcmp
    , compare
    , copyAt
    , unsafeCopyAtRO
    -- * temporary
    , pureST
    ) where

import           GHC.Prim
import           GHC.Types
import           GHC.Ptr
import           GHC.ST
import           Foundation.Internal.Primitive
import           Foundation.Primitive.Monad
import           Foundation.Primitive.Types
import           Foundation.Internal.Base
import qualified Foundation.Primitive.Runtime as Runtime
import           Foundation.Internal.Proxy
import qualified Foundation.Boot.List as List
import           Foundation.Primitive.Types.OffsetSize
import           Foundation.Primitive.FinalPtr
import           Foundation.Primitive.NormalForm
import           Foundation.Numerical
import           Foundation.System.Bindings.Hs
import           Foreign.C.Types
import           System.IO.Unsafe (unsafeDupablePerformIO)

-- | A Mutable array of types built on top of GHC primitive.
--
-- Element in this array can be modified in place.
data MUArray ty st =
      MUVecMA {-# UNPACK #-} !(Offset ty)
              {-# UNPACK #-} !(CountOf ty)
                             !PinnedStatus
                             (MutableByteArray# st)
    | MUVecAddr {-# UNPACK #-} !(Offset ty)
                {-# UNPACK #-} !(CountOf ty)
                               !(FinalPtr ty)

-- | An array of type built on top of GHC primitive.
--
-- The elements need to have fixed sized and the representation is a
-- packed contiguous array in memory that can easily be passed
-- to foreign interface
data UArray ty =
      UVecBA {-# UNPACK #-} !(Offset ty)
             {-# UNPACK #-} !(CountOf ty)
             {-# UNPACK #-} !PinnedStatus {- unpinned / pinned flag -}
                            !ByteArray#
    | UVecAddr {-# UNPACK #-} !(Offset ty)
               {-# UNPACK #-} !(CountOf ty)
                              !(FinalPtr ty)
    deriving (Typeable)

instance Data ty => Data (UArray ty) where
    dataTypeOf _ = arrayType
    toConstr _   = error "toConstr"
    gunfold _ _  = error "gunfold"

arrayType :: DataType
arrayType = mkNoRepType "Foundation.UArray"

instance NormalForm (UArray ty) where
    toNormalForm (UVecBA _ _ _ !_) = ()
    toNormalForm (UVecAddr {}) = ()
instance (PrimType ty, Show ty) => Show (UArray ty) where
    show v = show (toList v)
instance (PrimType ty, Eq ty) => Eq (UArray ty) where
    (==) = equal
instance (PrimType ty, Ord ty) => Ord (UArray ty) where
    {-# SPECIALIZE instance Ord (UArray Word8) #-}
    compare = vCompare

instance PrimType ty => Monoid (UArray ty) where
    mempty  = empty
    mappend = append
    mconcat = concat

instance PrimType ty => IsList (UArray ty) where
    type Item (UArray ty) = ty
    fromList = vFromList
    toList = vToList

length :: UArray ty -> CountOf ty
length (UVecAddr _ len _) = len
length (UVecBA _ len _ _) = len
{-# INLINE[1] length #-}

-- | Return if the array is pinned in memory
--
-- note that Foreign array are considered pinned
isPinned :: UArray ty -> PinnedStatus
isPinned (UVecAddr {})     = Pinned
isPinned (UVecBA _ _ _ ba) = toPinnedStatus# (compatIsByteArrayPinned# ba)

-- | Return if a mutable array is pinned in memory
isMutablePinned :: MUArray ty st -> PinnedStatus
isMutablePinned (MUVecAddr {})      = Pinned
isMutablePinned (MUVecMA _ _ _ mba) = toPinnedStatus# (compatIsMutableByteArrayPinned# mba)

-- | Create a new pinned mutable array of size @n.
--
-- all the cells are uninitialized and could contains invalid values.
--
-- All mutable arrays are allocated on a 64 bits aligned addresses
newPinned :: (PrimMonad prim, PrimType ty) => CountOf ty -> prim (MUArray ty (PrimState prim))
newPinned n = newFake n Proxy
  where newFake :: (PrimMonad prim, PrimType ty) => CountOf ty -> Proxy ty -> prim (MUArray ty (PrimState prim))
        newFake sz ty = primitive $ \s1 ->
            case newAlignedPinnedByteArray# bytes 8# s1 of
                (# s2, mba #) -> (# s2, MUVecMA (Offset 0) sz Pinned mba #)
          where
                !(CountOf (I# bytes)) = sizeOfE (primSizeInBytes ty) sz
        {-# INLINE newFake #-}

newUnpinned :: (PrimMonad prim, PrimType ty) => CountOf ty -> prim (MUArray ty (PrimState prim))
newUnpinned n = newFake n Proxy
  where newFake :: (PrimMonad prim, PrimType ty) => CountOf ty -> Proxy ty -> prim (MUArray ty (PrimState prim))
        newFake sz ty = primitive $ \s1 ->
            case newByteArray# bytes s1 of
                (# s2, mba #) -> (# s2, MUVecMA (Offset 0) sz Unpinned mba #)
          where
                !(CountOf (I# bytes)) = sizeOfE (primSizeInBytes ty) sz
        {-# INLINE newFake #-}

newNative :: (PrimMonad prim, PrimType ty)
          => CountOf ty
          -> (MutableByteArray# (PrimState prim) -> prim a)
          -> prim (a, MUArray ty (PrimState prim))
newNative n f = do
    muvec <- new n
    case muvec of
        (MUVecMA _ _ _ mba) -> f mba >>= \a -> pure (a, muvec)
        MUVecAddr {}        -> error "internal error: unboxed new only supposed to allocate natively"

-- | Create a new mutable array of size @n.
--
-- When memory for a new array is allocated, we decide if that memory region
-- should be pinned (will not be copied around by GC) or unpinned (can be
-- moved around by GC) depending on its size.
--
-- You can change the threshold value used by setting the environment variable
-- @HS_FOUNDATION_UARRAY_UNPINNED_MAX@.
new :: (PrimMonad prim, PrimType ty) => CountOf ty -> prim (MUArray ty (PrimState prim))
new sz
    | sizeRecast sz <= maxSizeUnpinned = newUnpinned sz
    | otherwise                        = newPinned sz
  where
    -- Safe to use here: If the value changes during runtime, this will only
    -- have an impact on newly created arrays.
    maxSizeUnpinned = Runtime.unsafeUArrayUnpinnedMaxSize
{-# INLINE new #-}

-- | read from a cell in a mutable array without bounds checking.
--
-- Reading from invalid memory can return unpredictable and invalid values.
-- use 'read' if unsure.
unsafeRead :: (PrimMonad prim, PrimType ty) => MUArray ty (PrimState prim) -> Offset ty -> prim ty
unsafeRead (MUVecMA start _ _ mba) i = primMbaRead mba (start + i)
unsafeRead (MUVecAddr start _ fptr) i = withFinalPtr fptr $ \(Ptr addr) -> primAddrRead addr (start + i)
{-# INLINE unsafeRead #-}


-- | write to a cell in a mutable array without bounds checking.
--
-- Writing with invalid bounds will corrupt memory and your program will
-- become unreliable. use 'write' if unsure.
unsafeWrite :: (PrimMonad prim, PrimType ty) => MUArray ty (PrimState prim) -> Offset ty -> ty -> prim ()
unsafeWrite (MUVecMA start _ _ mba)  i v = primMbaWrite mba (start+i) v
unsafeWrite (MUVecAddr start _ fptr) i v = withFinalPtr fptr $ \(Ptr addr) -> primAddrWrite addr (start+i) v
{-# INLINE unsafeWrite #-}

-- | Return the element at a specific index from an array without bounds checking.
--
-- Reading from invalid memory can return unpredictable and invalid values.
-- use 'index' if unsure.
unsafeIndex :: forall ty . PrimType ty => UArray ty -> Offset ty -> ty
unsafeIndex (UVecBA start _ _ ba) n = primBaIndex ba (start + n)
unsafeIndex (UVecAddr start _ fptr) n = withUnsafeFinalPtr fptr (\(Ptr addr) -> return (primAddrIndex addr (start+n)) :: IO ty)
{-# INLINE unsafeIndex #-}

unsafeIndexer :: (PrimMonad prim, PrimType ty) => UArray ty -> ((Offset ty -> ty) -> prim a) -> prim a
unsafeIndexer (UVecBA start _ _ ba) f = f (\n -> primBaIndex ba (start + n))
unsafeIndexer (UVecAddr start _ fptr) f = withFinalPtr fptr $ \(Ptr addr) -> f (\n -> primAddrIndex addr (start + n))
{-# INLINE unsafeIndexer #-}

-- | Freeze a mutable array into an array.
--
-- the MUArray must not be changed after freezing.
unsafeFreeze :: PrimMonad prim => MUArray ty (PrimState prim) -> prim (UArray ty)
unsafeFreeze (MUVecMA start len pinnedState mba) = primitive $ \s1 ->
    case unsafeFreezeByteArray# mba s1 of
        (# s2, ba #) -> (# s2, UVecBA start len pinnedState ba #)
unsafeFreeze (MUVecAddr start len fptr) = return $ UVecAddr start len fptr
{-# INLINE unsafeFreeze #-}

unsafeFreezeShrink :: (PrimType ty, PrimMonad prim) => MUArray ty (PrimState prim) -> CountOf ty -> prim (UArray ty)
unsafeFreezeShrink (MUVecMA start _ pinnedState mba) n = unsafeFreeze (MUVecMA start n pinnedState mba)
unsafeFreezeShrink (MUVecAddr start _ fptr) n = unsafeFreeze (MUVecAddr start n fptr)
{-# INLINE unsafeFreezeShrink #-}

-- | Thaw an immutable array.
--
-- The UArray must not be used after thawing.
unsafeThaw :: (PrimType ty, PrimMonad prim) => UArray ty -> prim (MUArray ty (PrimState prim))
unsafeThaw (UVecBA start len pinnedState ba) = primitive $ \st -> (# st, MUVecMA start len pinnedState (unsafeCoerce# ba) #)
unsafeThaw (UVecAddr start len fptr) = return $ MUVecAddr start len fptr
{-# INLINE unsafeThaw #-}


unsafeDewrap :: (ByteArray# -> Offset ty -> a)
             -> (Ptr ty -> Offset ty -> ST s a)
             -> UArray ty
             -> a
unsafeDewrap _ g (UVecAddr start _ fptr) = withUnsafeFinalPtr fptr $ \ptr -> g ptr start
unsafeDewrap f _ (UVecBA start _ _ ba)   = f ba start
{-# INLINE unsafeDewrap #-}

unsafeDewrap2 :: (ByteArray# -> Offset ty -> ByteArray# -> Offset ty -> a)
              -> (Ptr ty -> Offset ty -> Ptr ty -> Offset ty -> ST s a)
              -> (ByteArray# -> Offset ty -> Ptr ty -> Offset ty -> ST s a)
              -> (Ptr ty -> Offset ty -> ByteArray# -> Offset ty -> ST s a)
              -> UArray ty
              -> UArray ty
              -> a
unsafeDewrap2 f _ _ _ (UVecBA start1 _ _ ba1)   (UVecBA start2 _ _ ba2)   = f ba1 start1 ba2 start2
unsafeDewrap2 _ f _ _ (UVecAddr start1 _ fptr1) (UVecAddr start2 _ fptr2) = withUnsafeFinalPtr fptr1 $ \ptr1 ->
                                                                                  withFinalPtr fptr2 $ \ptr2 -> f ptr1 start1 ptr2 start2
unsafeDewrap2 _ _ f _ (UVecBA start1 _ _ ba1)   (UVecAddr start2 _ fptr2) = withUnsafeFinalPtr fptr2 $ \ptr2 -> f ba1 start1 ptr2 start2
unsafeDewrap2 _ _ _ f (UVecAddr start1 _ fptr1) (UVecBA start2 _ _ ba2)   = withUnsafeFinalPtr fptr1 $ \ptr1 -> f ptr1 start1 ba2 start2
{-# INLINE [2] unsafeDewrap2 #-}

pureST :: a -> ST s a
pureST = pure

-- | make an array from a list of elements.
vFromList :: PrimType ty => [ty] -> UArray ty
vFromList l = runST $ do
    ma <- new (CountOf len)
    iter azero l $ \i x -> unsafeWrite ma i x
    unsafeFreeze ma
  where len = List.length l
        iter _  []     _ = return ()
        iter !i (x:xs) z = z i x >> iter (i+1) xs z

-- | transform an array to a list.
vToList :: forall ty . PrimType ty => UArray ty -> [ty]
vToList a
    | len == 0  = []
    | otherwise = unsafeDewrap goBa goPtr a
  where
    !len = length a
    goBa ba start = loop start
      where
        !end = start `offsetPlusE` len
        loop !i | i == end  = []
                | otherwise = primBaIndex ba i : loop (i+1)
    goPtr (Ptr addr) start = pureST (loop start)
      where
        !end = start `offsetPlusE` len
        loop !i | i == end  = []
                | otherwise = primAddrIndex addr i : loop (i+1)

-- | Check if two vectors are identical
equal :: (PrimType ty, Eq ty) => UArray ty -> UArray ty -> Bool
equal a b
    | la /= lb  = False
    | otherwise = unsafeDewrap2 goBaBa goPtrPtr goBaPtr goPtrBa a b
  where
    !la = length a
    !lb = length b
    goBaBa ba1 start1 ba2 start2 = loop start1 start2
      where
        !end = start1 `offsetPlusE` la
        loop !i !o | i == end  = True
                   | otherwise = primBaIndex ba1 i == primBaIndex ba2 o && loop (i+o1) (o+o1)
    goPtrPtr (Ptr addr1) start1 (Ptr addr2) start2 = pureST (loop start1 start2)
      where
        !end = start1 `offsetPlusE` la
        loop !i !o | i == end  = True
                   | otherwise = primAddrIndex addr1 i == primAddrIndex addr2 o && loop (i+o1) (o+o1)
    goBaPtr ba1 start1 (Ptr addr2) start2 = pureST (loop start1 start2)
      where
        !end = start1 `offsetPlusE` la
        loop !i !o | i == end  = True
                   | otherwise = primBaIndex ba1 i == primAddrIndex addr2 o && loop (i+o1) (o+o1)
    goPtrBa (Ptr addr1) start1 ba2 start2 = pureST (loop start1 start2)
      where
        !end = start1 `offsetPlusE` la
        loop !i !o | i == end  = True
                   | otherwise = primAddrIndex addr1 i == primBaIndex ba2 o && loop (i+o1) (o+o1)

    o1 = Offset (I# 1#)
{-# RULES "UArray/Eq/Word8" [3] equal = equalBytes #-}
{-# INLINEABLE [2] equal #-}

equalBytes :: UArray Word8 -> UArray Word8 -> Bool
equalBytes a b
    | la /= lb  = False
    | otherwise = memcmp a b (csizeOfSize $ sizeInBytes la) == 0
  where
    !la = length a
    !lb = length b

equalMemcmp :: PrimType ty => UArray ty -> UArray ty -> Bool
equalMemcmp a b
    | la /= lb  = False
    | otherwise = memcmp a b (csizeOfSize $ sizeInBytes la) == 0
  where
    !la = length a
    !lb = length b

-- | Compare 2 vectors
vCompare :: (Ord ty, PrimType ty) => UArray ty -> UArray ty -> Ordering
vCompare a b = unsafeDewrap2 goBaBa goPtrPtr goBaPtr goPtrBa a b
  where
    !la = length a
    !lb = length b
    o1 = Offset (I# 1#)
    goBaBa ba1 start1 ba2 start2 = loop start1 start2
      where
        !end = start1 `offsetPlusE` min la lb
        loop !i !o | i == end   = la `compare` lb
                   | v1 == v2   = loop (i + o1) (o + o1)
                   | otherwise  = v1 `compare` v2
          where v1 = primBaIndex ba1 i
                v2 = primBaIndex ba2 o
    goPtrPtr (Ptr addr1) start1 (Ptr addr2) start2 = pureST (loop start1 start2)
      where
        !end = start1 `offsetPlusE` min la lb
        loop !i !o | i == end   = la `compare` lb
                   | v1 == v2   = loop (i + o1) (o + o1)
                   | otherwise  = v1 `compare` v2
          where v1 = primAddrIndex addr1 i
                v2 = primAddrIndex addr2 o
    goBaPtr ba1 start1 (Ptr addr2) start2 = pureST (loop start1 start2)
      where
        !end  = start1 `offsetPlusE` min la lb
        loop !i !o | i == end   = la `compare` lb
                   | v1 == v2   = loop (i + o1) (o + o1)
                   | otherwise  = v1 `compare` v2
          where v1 = primBaIndex ba1 i
                v2 = primAddrIndex addr2 o
    goPtrBa (Ptr addr1) start1 ba2 start2 = pureST (loop start1 start2)
      where
        !end  = start1 `offsetPlusE` min la lb
        loop !i !o | i == end   = la `compare` lb
                   | v1 == v2   = loop (i + o1) (o + o1)
                   | otherwise  = v1 `compare` v2
          where v1 = primAddrIndex addr1 i
                v2 = primBaIndex ba2 o
-- {-# SPECIALIZE [3] vCompare :: UArray Word8 -> UArray Word8 -> Ordering = vCompareBytes #-}
{-# RULES "UArray/Ord/Word8" [3] vCompare = vCompareBytes #-}
{-# INLINEABLE [2] vCompare #-}

vCompareBytes :: UArray Word8 -> UArray Word8 -> Ordering
vCompareBytes = vCompareMemcmp

vCompareMemcmp :: (Ord ty, PrimType ty) => UArray ty -> UArray ty -> Ordering
vCompareMemcmp a b = cintToOrdering $ memcmp a b sz
  where
    la = length a
    lb = length b
    sz = csizeOfSize $ sizeInBytes $ min la lb
    cintToOrdering :: CInt -> Ordering
    cintToOrdering 0 = la `compare` lb
    cintToOrdering r | r < 0     = LT
                     | otherwise = GT
{-# SPECIALIZE [3] vCompareMemcmp :: UArray Word8 -> UArray Word8 -> Ordering #-}

memcmp :: PrimType ty => UArray ty -> UArray ty -> CSize -> CInt
memcmp a b sz = unsafeDewrap2
    (\s1 o1 s2 o2 -> unsafeDupablePerformIO $ sysHsMemcmpBaBa s1 (offsetToCSize o1) s2 (offsetToCSize o2) sz)
    (\s1 o1 s2 o2 -> unsafePrimToST $ sysHsMemcmpPtrPtr s1 (offsetToCSize o1) s2 (offsetToCSize o2) sz)
    (\s1 o1 s2 o2 -> unsafePrimToST $ sysHsMemcmpBaPtr s1 (offsetToCSize o1) s2 (offsetToCSize o2) sz)
    (\s1 o1 s2 o2 -> unsafePrimToST $ sysHsMemcmpPtrBa s1 (offsetToCSize o1) s2 (offsetToCSize o2) sz)
    a b
  where
    offsetToCSize ofs = csizeOfOffset $ offsetInBytes ofs
{-# SPECIALIZE [3] memcmp :: UArray Word8 -> UArray Word8 -> CSize -> CInt #-}

-- | Copy a number of elements from an array to another array with offsets
copyAt :: forall prim ty . (PrimMonad prim, PrimType ty)
       => MUArray ty (PrimState prim) -- ^ destination array
       -> Offset ty                  -- ^ offset at destination
       -> MUArray ty (PrimState prim) -- ^ source array
       -> Offset ty                  -- ^ offset at source
       -> CountOf ty                    -- ^ number of elements to copy
       -> prim ()
copyAt (MUVecMA dstStart _ _ dstMba) ed (MUVecMA srcStart _ _ srcBa) es n =
    primitive $ \st -> (# copyMutableByteArray# srcBa os dstMba od nBytes st, () #)
  where
    !sz                 = primSizeInBytes (Proxy :: Proxy ty)
    !(Offset (I# os))   = offsetOfE sz (srcStart + es)
    !(Offset (I# od))   = offsetOfE sz (dstStart + ed)
    !(CountOf (I# nBytes)) = sizeOfE sz n
copyAt (MUVecMA dstStart _ _ dstMba) ed (MUVecAddr srcStart _ srcFptr) es n =
    withFinalPtr srcFptr $ \srcPtr ->
        let !(Ptr srcAddr) = srcPtr `plusPtr` os
         in primitive $ \s -> (# compatCopyAddrToByteArray# srcAddr dstMba od nBytes s, () #)
  where
    !sz                 = primSizeInBytes (Proxy :: Proxy ty)
    !(Offset os)        = offsetOfE sz (srcStart + es)
    !(Offset (I# od))   = offsetOfE sz (dstStart + ed)
    !(CountOf (I# nBytes)) = sizeOfE sz n
copyAt dst od src os n = loop od os
  where
    !endIndex = os `offsetPlusE` n
    loop !d !i
        | i == endIndex = return ()
        | otherwise     = unsafeRead src i >>= unsafeWrite dst d >> loop (d+1) (i+1)

-- TODO Optimise with copyByteArray#
-- | Copy @n@ sequential elements from the specified offset in a source array
--   to the specified position in a destination array.
--
--   This function does not check bounds. Accessing invalid memory can return
--   unpredictable and invalid values.
unsafeCopyAtRO :: forall prim ty . (PrimMonad prim, PrimType ty)
               => MUArray ty (PrimState prim) -- ^ destination array
               -> Offset ty                   -- ^ offset at destination
               -> UArray ty                   -- ^ source array
               -> Offset ty                   -- ^ offset at source
               -> CountOf ty                     -- ^ number of elements to copy
               -> prim ()
unsafeCopyAtRO (MUVecMA dstStart _ _ dstMba) ed (UVecBA srcStart _ _ srcBa) es n =
    primitive $ \st -> (# copyByteArray# srcBa os dstMba od nBytes st, () #)
  where
    sz = primSizeInBytes (Proxy :: Proxy ty)
    !(Offset (I# os))   = offsetOfE sz (srcStart+es)
    !(Offset (I# od))   = offsetOfE sz (dstStart+ed)
    !(CountOf (I# nBytes)) = sizeOfE sz n
unsafeCopyAtRO (MUVecMA dstStart _ _ dstMba) ed (UVecAddr srcStart _ srcFptr) es n =
    withFinalPtr srcFptr $ \srcPtr ->
        let !(Ptr srcAddr) = srcPtr `plusPtr` os
         in primitive $ \s -> (# compatCopyAddrToByteArray# srcAddr dstMba od nBytes s, () #)
  where
    sz  = primSizeInBytes (Proxy :: Proxy ty)
    !(Offset os)        = offsetOfE sz (srcStart+es)
    !(Offset (I# od))   = offsetOfE sz (dstStart+ed)
    !(CountOf (I# nBytes)) = sizeOfE sz n
unsafeCopyAtRO dst od src os n = loop od os
  where
    !endIndex = os `offsetPlusE` n
    loop d i
        | i == endIndex = return ()
        | otherwise     = unsafeWrite dst d (unsafeIndex src i) >> loop (d+1) (i+1)


data BA0 = BA0 !ByteArray# -- zero ba

empty_ :: BA0
empty_ = runST $ primitive $ \s1 ->
    case newByteArray# 0# s1           of { (# s2, mba #) ->
    case unsafeFreezeByteArray# mba s2 of { (# s3, ba  #) ->
        (# s3, BA0 ba #) }}

empty :: UArray ty
empty = UVecBA 0 0 Unpinned ba where !(BA0 ba) = empty_

-- | Append 2 arrays together by creating a new bigger array
append :: PrimType ty => UArray ty -> UArray ty -> UArray ty
append a b
    | la == azero = b
    | lb == azero = a
    | otherwise = runST $ do
        r  <- new (la+lb)
        ma <- unsafeThaw a
        mb <- unsafeThaw b
        copyAt r (Offset 0) ma (Offset 0) la
        copyAt r (sizeAsOffset la) mb (Offset 0) lb
        unsafeFreeze r
  where
    !la = length a
    !lb = length b

concat :: PrimType ty => [UArray ty] -> UArray ty
concat [] = empty
concat l  =
    case filterAndSum (CountOf 0) [] l of
        (_,[])            -> empty
        (_,[x])           -> x
        (totalLen,chunks) -> runST $ do
            r <- new totalLen
            doCopy r (Offset 0) chunks
            unsafeFreeze r
  where
    -- TODO would go faster not to reverse but pack from the end instead
    filterAndSum !totalLen acc []     = (totalLen, List.reverse acc)
    filterAndSum !totalLen acc (x:xs)
        | len == CountOf 0 = filterAndSum totalLen acc xs
        | otherwise      = filterAndSum (len+totalLen) (x:acc) xs
      where len = length x

    doCopy _ _ []     = return ()
    doCopy r i (x:xs) = do
        unsafeCopyAtRO r i x (Offset 0) lx
        doCopy r (i `offsetPlusE` lx) xs
      where lx = length x
