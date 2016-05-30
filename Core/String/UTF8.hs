-- |
-- Module      : Core.String.UTF8
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
module Core.String.UTF8
    ( String(..)
    --, Buffer
    , replicate
    ) where

import           GHC.Prim
import           GHC.Types
import           GHC.ST
import qualified Prelude
import           Core.Internal.Base
import           Core.Internal.Primitive
import qualified Core.Collection as C
import           Core.Primitive.Monad
import           Core.String.UTF8Table

import qualified Data.List -- temporary

-- | Opaque packed array of characters in the UTF8 encoding
data String = String ByteArray#

data Buffer st = Buffer (MutableByteArray# st)

instance Show String where
    show = show . sToList
instance Eq String where
    (==) = equal
instance Ord String where
    compare = compareString
instance Monoid String where
    mempty = empty
    mappend = append
instance IsString String where
    fromString = sFromList
instance IsList String where
    type Item String = Char
    fromList = sFromList
    toList = sToList

type instance C.Element String = Char

instance C.InnerFunctor String where
    imap = charMap
instance C.SemiOrderedCollection String where
    snoc = snoc
    cons = cons
    find = find
    sortBy = sortBy
    length = length
    singleton = fromList . (:[])
instance C.OrderedCollection String where
    null = null
    take = take
    drop = drop
    splitAt = splitAt
    splitOn = splitOn
    break = break
    span = span
    filter = filter
    reverse = reverse

{-
validate :: ByteArray# -> Int# -> (# Bool, Int# #)
validate ba n =
    case getNbBytes# h of
        0# -> (# True, n +# 1# #)
        1# -> (# isContinuation# (indexWord8Array# ba (n +# 1#)) , n +# 2# #)
        2# -> (# isContinuation# (indexWord8Array# ba (n +# 1#)) &&
                 isContinuation# (indexWord8Array# ba (n +# 2#)) , n +# 3# #)
        3# -> (# isContinuation# (indexWord8Array# ba (n +# 1#)) &&
                 isContinuation# (indexWord8Array# ba (n +# 2#)) &&
                 isContinuation# (indexWord8Array# ba (n +# 3#)) , n +# 4# #)
        _  -> (# False, n #)
  where
    !h = indexWord8Array# ba n
-}

skipNext :: ByteArray# -> Int# -> Int#
skipNext ba n = n +# 1# +# getNbBytes# h
  where
    !h = indexWord8Array# ba n

next :: ByteArray# -> Int# -> (# Char, Int# #)
next ba n =
    case getNbBytes# h of
        0# -> (# toChar h, n +# 1# #)
        1# -> (# toChar (decode2 (indexWord8Array# ba (n +# 1#))) , n +# 2# #)
        2# -> (# toChar (decode3 (indexWord8Array# ba (n +# 1#))
                                 (indexWord8Array# ba (n +# 2#))) , n +# 3# #)
        3# -> (# toChar (decode4 (indexWord8Array# ba (n +# 1#))
                                 (indexWord8Array# ba (n +# 2#))
                                 (indexWord8Array# ba (n +# 3#))) , n +# 4# #)
        r -> error ("next: internal error: invalid input: " <> show (I# r) <> " " <> show (W# h))
  where
    !h = indexWord8Array# ba n

    toChar :: Word# -> Char
    toChar w = C# (chr# (word2Int# w))

    decode2 :: Word# -> Word#
    decode2 c1 =
        or# (uncheckedShiftL# (and# h 0x1f##) 6#)
            (and# c1 0x3f##)

    decode3 :: Word# -> Word# -> Word#
    decode3 c1 c2 =
        or# (uncheckedShiftL# (and# h 0xf##) 12#)
            (or# (uncheckedShiftL# (and# c1 0x3f##) 6#)
                 (and# c2 0x3f##))

    decode4 :: Word# -> Word# -> Word# -> Word#
    decode4 c1 c2 c3 =
        or# (uncheckedShiftL# (and# h 0x7##) 18#)
            (or# (uncheckedShiftL# (and# c1 0x3f##) 12#)
                (or# (uncheckedShiftL# (and# c2 0x3f##) 6#)
                    (and# c3 0x3f##))
            )

write :: MutableByteArray# st -> Int# -> Char -> State# st -> (# State# st, Int# #)
write mba idx c =
    if      bool# (ltWord# x 0x80##   ) then encode1
    else if bool# (ltWord# x 0x800##  ) then encode2
    else if bool# (ltWord# x 0x10000##) then encode3
    else                                     encode4
  where
    !(I# xi) = fromEnum c
    !x       = int2Word# xi

    encode1 st =
        let st' = writeWord8Array# mba idx x st
         in (# st', idx +# 1# #)

    encode2 st =
        let x1  = or# (uncheckedShiftRL# x 6#) 0xc0##
            x2  = toContinuation x
            st2 = writeWord8Array# mba idx         x1 st
            st3 = writeWord8Array# mba (idx +# 1#) x2 st2
         in (# st3, idx +# 2# #)

    encode3 st =
        let x1  = or# (uncheckedShiftRL# x 12#) 0xe0##
            x2  = toContinuation (uncheckedShiftRL# x 6#)
            x3  = toContinuation x
            st2 = writeWord8Array# mba idx         x1 st
            st3 = writeWord8Array# mba (idx +# 1#) x2 st2
            st4 = writeWord8Array# mba (idx +# 2#) x3 st3
         in (# st4, idx +# 3# #)

    encode4 st =
        let x1  = or# (uncheckedShiftRL# x 18#) 0xf0##
            x2  = toContinuation (uncheckedShiftRL# x 12#)
            x3  = toContinuation (uncheckedShiftRL# x 6#)
            x4  = toContinuation x
            st2 = writeWord8Array# mba idx         x1 st
            st3 = writeWord8Array# mba (idx +# 1#) x2 st2
            st4 = writeWord8Array# mba (idx +# 2#) x3 st3
            st5 = writeWord8Array# mba (idx +# 3#) x4 st4
         in (# st5, idx +# 4# #)

    toContinuation :: Word# -> Word#
    toContinuation w = or# (and# w 0x3f##) 0x80##

freeze :: MutableByteArray# st -> State# st -> (# State# st, String #)
freeze mba st = let (# st', ba  #) = unsafeFreezeByteArray# mba st in (# st', String ba #)
{-# INLINE freeze #-}

------------------------------------------------------------------------
-- real functions

sToList :: String -> [Char]
sToList (String ba) = loop 0#
  where
    !nbBytes = sizeofByteArray# ba
    loop n
        | bool# (n ==# nbBytes) = []
        | otherwise             = let (# c , n' #) = next ba n in c : loop n'

sFromList :: [Char] -> String
sFromList l = runST $ primitive $ \st1 ->
    let (# st2, mba #) = newByteArray# bytes st1
     in loop mba 0# st2 l
  where
    -- count how many bytes
    !(I# bytes) = Prelude.sum $ fmap (charToBytes . fromEnum) l

    -- write those bytes
    loop :: MutableByteArray# st -> Int# -> State# st -> [Char] -> (# State# st, String #)
    loop mba _   st []     = freeze mba st
    loop mba idx st (c:xs) =
        let (# st', idx' #) = write mba idx c st
         in loop mba idx' st' xs

empty :: String
empty = runST $ primitive $ \st1 -> do
    let (# st2, mba #) = newByteArray# 0# st1
        (# st3, ba  #) = unsafeFreezeByteArray# mba st2
     in (# st3, String ba #)

null :: String -> Bool
null (String ba) = bool# (sizeofByteArray# ba ==# 0#)

append :: String -> String -> String
append (String a) (String b) = runST $ primitive $ \st1 ->
    let (# st2, mba #) = newByteArray# destSize st1
        st3            = copyByteArray# a 0# mba 0#    aSize st2
        st4            = copyByteArray# b 0# mba aSize bSize st3
     in freeze mba st4
  where
    !aSize = sizeofByteArray# a
    !bSize = sizeofByteArray# b
    !destSize = aSize +# bSize

equal :: String -> String -> Bool
equal (String a) (String b)
    | bool# (aSize ==# bSize) = eqBytes 0#
    | otherwise               = False
  where
    !aSize = sizeofByteArray# a
    !bSize = sizeofByteArray# b
    eqBytes idx
        | bool# (idx ==# aSize) = True
        | otherwise             =
            let w1 = indexWord8Array# a idx
                w2 = indexWord8Array# b idx
             in if bool# (eqWord# w1 w2) then eqBytes (idx +# 1#) else False

compareString :: String -> String -> Ordering
compareString (String a) (String b) = compBytes 0#
  where
    !aSize = sizeofByteArray# a
    !bSize = sizeofByteArray# b

    !sameSize = bool# (aSize ==# bSize)

    compBytes idx
        | bool# (idx ==# aSize) = if sameSize then EQ else LT
        | bool# (idx ==# bSize) = GT
        | otherwise             =
            let w1 = indexWord8Array# a idx
                w2 = indexWord8Array# b idx
             in if bool# (eqWord# w1 w2)
                    then compBytes (idx +# 1#)
                    else if bool# (geWord# w1 w2) then GT else LT

-- | Create a string composed of a number @n of Chars (Unicode code points).
--
-- if the input @s contains less characters than required, then
take :: Int -> String -> String
take n s = fst $ splitAt n s -- TODO specialize

-- | Create a string with the remaining Chars after dropping @n Chars from the beginning
drop :: Int -> String -> String
drop (I# n) s@(String ba)
    | bool# (n <=# 0#) = s
    | otherwise        = loop 0# 0#
  where
    !sz = sizeofByteArray# ba
    loop idx i
        | bool# (idx >=# sz) = mempty
        | bool# (i ==# n)    = runST $ primitive $ \st1 ->
            let dSize          = sz -# idx
                (# st2, dst #) = newByteArray# dSize st1
                st3            = copyByteArray# ba idx dst 0# dSize st2
             in freeze dst st3
        | otherwise                = loop (skipNext ba idx) (i +# 1#)

splitAt :: Int -> String -> (String, String)
splitAt (I# n) s@(String ba)
    | bool# (n <=# 0#) = (empty, s)
    | otherwise        = loop 0# 0#
  where
    !sz = sizeofByteArray# ba
    loop idx i
        | bool# (idx >=# sz) = (s, mempty)
        | bool# (i ==# n)    = splitIndex idx ba
        | otherwise          = loop (skipNext ba idx) (i +# 1#)

revTake :: Int -> String -> String
revTake nbElems v = drop (length v - nbElems) v

revDrop :: Int -> String -> String
revDrop nbElems v = take (length v - nbElems) v

revSplitAt :: Int -> String -> (String, String)
revSplitAt n v = (drop idx v, take idx v)
  where idx = length v - n

-- | Split on the input string using the predicate as separator
--
-- e.g.
--
-- > splitOn (== ',') ","          == ["",""]
-- > splitOn (== ',') ",abc,"      == ["","abc",""]
-- > splitOn (== ':') "abc"        == ["abc"]
-- > splitOn (== ':') "abc::def"   == ["abc","","def"]
-- > splitOn (== ':') "::abc::def" == ["","","abc","","def"]
--
splitOn :: (Char -> Bool) -> String -> [String]
splitOn predicate (String ba)
    | bool# (sz ==# 0#) = []
    | otherwise         = loop 0# 0#
  where
    !sz = sizeofByteArray# ba
    loop prevIdx idx
        | bool# (idx ==# sz) = [sub ba prevIdx idx]
        | otherwise          =
            let (# c, idx' #) = next ba idx
             in if predicate c
                    then sub ba prevIdx idx : loop idx' idx'
                    else loop prevIdx idx'

sub :: ByteArray# -> Int# -> Int# -> String
sub ba start end
    | bool# (start ==# end) = empty
    | otherwise             = runST $ primitive $ \st ->
        let sz             = end -# start
            (# st2, mba #) = newByteArray# sz st
            st3            = copyByteArray# ba start mba 0# sz st2
         in freeze mba st3

-- | Split at a given index.
splitIndex :: Int# -> ByteArray# -> (String, String)
splitIndex idx ba =
    runST $ primitive $ \st1 ->
        let (# st2, dst1 #) = newByteArray# idx    st1
            (# st3, dst2 #) = newByteArray# dst2Sz st2
            st4 = copyByteArray# ba 0#  dst1 0# idx    st3
            st5 = copyByteArray# ba idx dst2 0# dst2Sz st4
            (# st6, s1 #) = freeze dst1 st5
            (# st7, s2 #) = freeze dst2 st6
         in (# st7, (s1, s2) #)
  where
    !sz     = sizeofByteArray# ba
    !dst2Sz = sz -# idx

break :: (Char -> Bool) -> String -> (String, String)
break predicate s@(String ba) = loop 0#
  where
    !sz     = sizeofByteArray# ba
    loop idx
        | bool# (idx ==# sz) = (s, empty)
        | otherwise          =
            let (# c, idx' #) = next ba idx
             in case predicate c of
                    True  -> splitIndex idx ba
                    False -> loop idx'

span :: (Char -> Bool) -> String -> (String, String)
span predicate s = break (not . predicate) s

length :: String -> Int
length (String ba) = I# (loop 0# 0#)
  where
    !sz     = sizeofByteArray# ba
    loop idx i
        | bool# (idx ==# sz) = i
        | otherwise =
            let idx' = skipNext ba idx
             in loop idx' (i +# 1#)

replicate :: Int -> Char -> String
replicate n c = runST (new nbBytes >>= fill)
  where
    !nbBytes@(I# bytes) = sz Prelude.* n
    sz          = charToBytes (fromEnum c)
    fill :: PrimMonad prim => Buffer (PrimState prim) -> prim String
    fill (Buffer mba) = primitive (loop 0#)
      where
        loop idx st
            | bool# (idx ==# bytes) = freeze mba st
            | otherwise             =
                let (# st', idx' #) = write mba idx c st
                 in loop idx' st'

{-
sizeBytes :: String -> Int
sizeBytes (String ba) = I# (sizeofByteArray# ba)
-}

new :: PrimMonad prim => Int -> prim (Buffer (PrimState prim))
new (I# n) = primitive $ \st -> let (# st2, mba #) = newByteArray# n st in (# st2, Buffer mba #)

charToBytes :: Int -> Int
charToBytes c
    | c < 0x80     = 1
    | c < 0x800    = 2
    | c < 0x10000  = 3
    | c < 0x110000 = 4
    | otherwise    = error ("invalid code point: " `mappend` show c)

charMap :: (Char -> Char) -> String -> String
charMap f (String srcBa) =
    let !(elems, nbBytes) = allocateAndFill [] 0 0
     in runST $ do
            (Buffer dest) <- new nbBytes
            copyLoop dest elems nbBytes
            primitive (freeze dest)
  where
    !srcSz = sizeofByteArray# srcBa

    allocateAndFill :: [(String, Int)]
                    -> Int
                    -> Int
                    -> ([(String,Int)], Int)
    allocateAndFill acc idx@(I# idx#) bytesWritten
        | bool# (idx# ==# srcSz) = (acc, bytesWritten)
        | otherwise              =
            let (el@(_,addBytes), idx') = runST $ do
                        -- make sure we allocate at least 4 bytes for the destination for the last few bytes
                        -- otherwise allocating less would bring the danger of spinning endlessly
                        -- and never succeeding.
                        let !diffBytes = srcSz -# idx#
                            !allocatedBytes = if bool# (diffBytes <=# 4#) then 4# else diffBytes
                        (Buffer mba) <- new (I# allocatedBytes)
                        primitive $ \st ->
                            let (# st2, (dstIdx, srcIdx) #) = fill mba idx st
                                (# st3, s #) = freeze mba st2
                             in (# st3, ((s, dstIdx), srcIdx) #)
             in allocateAndFill (el : acc) idx' (bytesWritten Prelude.+ addBytes)

    fill :: MutableByteArray# st
         -> Int
         -> State# st
         -> (# State# st, (Int, Int) #)
    fill mba (I# srcIdxOrig) =
        loop 0# srcIdxOrig
      where
        !dsz = sizeofMutableByteArray# mba
        loop dstIdx srcIdx st
            | bool# (srcIdx ==# srcSz) = (# st, (I# dstIdx, I# srcIdx) #)
            | bool# (dstIdx ==# dsz)   = (# st, (I# dstIdx, I# srcIdx) #)
            | otherwise                =
                let (# c, srcIdx' #) = next srcBa srcIdx
                    c' = f c -- the mapped char
                    !(I# nbBytes) = charToBytes (fromEnum c')
                 in -- check if we have room in the destination buffer
                    if bool# (dstIdx +# nbBytes <=# dsz)
                        then let (# st', dstIdx' #) = write mba dstIdx c' st
                              in loop dstIdx' srcIdx' st'
                        else (# st, (I# dstIdx, I# srcIdx) #)

    copyLoop _   []     0        = return ()
    copyLoop _   []     n        = error ("charMap invalid: " <> show n)
    copyLoop mba ((String ba, (I# sz)):xs) (I# end) = do
        let start = end -# sz
        primitive $ \st -> (# copyByteArray# ba 0# mba start sz st, () #)
        copyLoop mba xs (I# start)

snoc :: String -> Char -> String
snoc (String ba) c = runST $ do
    (Buffer buf) <- new (I# (len +# nbBytes))
    primitive $ \st ->
        let st2          = copyByteArray# ba 0# buf 0# len st
            (# st3, _ #) = write buf len c st2
         in freeze buf st3
  where
    !len          = sizeofByteArray# ba
    !(I# nbBytes) = charToBytes (fromEnum c)

cons :: Char -> String -> String
cons c (String ba) = runST $ do
    (Buffer buf) <- new (I# (len +# nbBytes))
    primitive $ \st ->
        let (# st2, idx #) = write buf 0# c st
            st3            = copyByteArray# ba 0# buf idx len st2
         in freeze buf st3
  where
    !len          = sizeofByteArray# ba
    !(I# nbBytes) = charToBytes (fromEnum c)

find :: (Char -> Bool) -> String -> Maybe Char
find predicate (String ba) = loop 0#
  where
    !sz     = sizeofByteArray# ba
    loop idx
        | bool# (idx ==# sz) = Nothing
        | otherwise          =
            let (# c, idx' #) = next ba idx
             in case predicate c of
                    True  -> Just c
                    False -> loop idx'

sortBy :: (Char -> Char -> Ordering) -> String -> String
sortBy sortF s = fromList $ Data.List.sortBy sortF $ toList s -- FIXME for tests

filter :: (Char -> Bool) -> String -> String
filter p s = fromList $ Data.List.filter p $ toList s

reverse :: String -> String
reverse (String ba) = runST $ do
    (Buffer b) <- new (I# len)
    primitive $ \s1 -> loop b 0# len s1
  where
    !len = sizeofByteArray# ba
    -- write those bytes
    loop :: MutableByteArray# st -> Int# -> Int# -> State# st -> (# State# st, String #)
    loop mba sidx didx st
        | bool# (didx ==# 0#) = freeze mba st
        | otherwise =
            let !h = indexWord8Array# ba sidx
                !nb = getNbBytes# h +# 1#
                d = didx -# nb
                st' = case nb of
                        1# -> writeWord8Array# mba d         h st
                        2# ->
                            let st2 = writeWord8Array# mba d         h st
                                st3 = writeWord8Array# mba (d +# 1#) (indexWord8Array# ba (sidx +# 1#)) st2
                             in st3
                        3# ->
                            let st2 = writeWord8Array# mba d         h st
                                st3 = writeWord8Array# mba (d +# 1#) (indexWord8Array# ba (sidx +# 1#)) st2
                                st4 = writeWord8Array# mba (d +# 2#) (indexWord8Array# ba (sidx +# 2#)) st3
                             in st4
                        4# ->
                            let st2 = writeWord8Array# mba d         h st
                                st3 = writeWord8Array# mba (d +# 1#) (indexWord8Array# ba (sidx +# 1#)) st2
                                st4 = writeWord8Array# mba (d +# 2#) (indexWord8Array# ba (sidx +# 2#)) st3
                                st5 = writeWord8Array# mba (d +# 3#) (indexWord8Array# ba (sidx +# 3#)) st4
                             in st5
                        _  -> st -- impossible
             in loop mba (sidx +# nb) d st'
