-- |
-- Module      : Core.String.UTF8
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- A String type backed by a UTF8 encoded byte array and all the necessary
-- functions to manipulate the string.
--
-- You can think of String as a specialization of a byte array that
-- have element of type Char.
--
-- The String data must contain UTF8 valid data.
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
module Core.String.UTF8
    ( String(..)
    --, Buffer
    , create
    , replicate
    -- * Binary conversion
    , Encoding(..)
    , fromBytes
    , fromChunkBytes
    , fromBytesUnsafe
    , toBytes
    , mutableValidate
    , ValidationFailure(..)
    ) where

import           GHC.Prim
import           GHC.Types
import           GHC.ST
import           GHC.Word
import qualified Prelude
import           Core.Internal.Base
import           Core.Internal.Primitive
import qualified Core.Collection as C
import           Core.Primitive.Monad
import           Core.String.UTF8Table
import           Core.Array.Unboxed (ByteArray)
import           Core.Array.Unboxed.ByteArray (MutableByteArray)
import qualified Core.Array.Unboxed as Vec
import qualified Core.Array.Unboxed.Mutable as MVec
import           Core.Number

import qualified Data.List -- temporary

-- | Opaque packed array of characters in the UTF8 encoding
newtype String = String ByteArray
    deriving (Typeable)

newtype MutableString st = MutableString (MutableByteArray st)
    deriving (Typeable)

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
instance C.Sequential String where
    null = null
    take = take
    drop = drop
    splitAt = splitAt
    revTake = revTake
    revDrop = revDrop
    revSplitAt = revSplitAt
    splitOn = splitOn
    break = break
    span = span
    filter = filter
    reverse = reverse
    unsnoc = unsnoc
    uncons = uncons
    snoc = snoc
    cons = cons
    find = find
    sortBy = sortBy
    length = length
    singleton = fromList . (:[])

data ValidationFailure = InvalidHeader
                       | InvalidContinuation
                       | MissingByte
                       deriving (Show,Eq)

-- | Validate a bytearray for UTF8'ness
--
-- On success Nothing is returned
-- On Failure the position along with the failure reason
validate :: ByteArray
         -> Int
         -> Int
         -> (Int, Maybe ValidationFailure)
validate ba ofsStart sz = loop ofsStart
  where
    end = ofsStart + sz

    loop ofs
        | ofs > end  = error "validate: internal error: went pass offset"
        | ofs == end = (end, Nothing)
        | otherwise  =
            case one ofs of
                (nextOfs, Nothing)  -> loop nextOfs
                (pos, Just failure) -> (pos, Just failure)

    one pos = do
        let h = Vec.unsafeIndex ba pos
            nbConts = getNbBytes h
        if nbConts == 0xff
            then (pos, Just InvalidHeader)
            else if pos + 1 + nbConts > end
                then (pos, Just MissingByte)
                else do
                    case nbConts of
                        0 -> (pos + 1, Nothing)
                        1 ->
                            let c1 = Vec.unsafeIndex ba (pos + 1)
                             in if isContinuation c1
                                    then (pos + 2, Nothing)
                                    else (pos, Just InvalidContinuation)
                        2 ->
                            let c1 = Vec.unsafeIndex ba (pos + 1)
                                c2 = Vec.unsafeIndex ba (pos + 2)
                             in if isContinuation c1 && isContinuation c2
                                    then (pos + 3, Nothing)
                                    else (pos, Just InvalidContinuation)
                        3 ->
                            let c1 = Vec.unsafeIndex ba (pos + 1)
                                c2 = Vec.unsafeIndex ba (pos + 2)
                                c3 = Vec.unsafeIndex ba (pos + 3)
                             in if isContinuation c1 && isContinuation c2 && isContinuation c3
                                    then (pos + 4, Nothing)
                                    else (pos, Just InvalidContinuation)
                        _ -> error "internal error"

mutableValidate :: PrimMonad prim
                => MutableByteArray (PrimState prim)
                -> Int
                -> Int
                -> prim (Int, Maybe ValidationFailure)
mutableValidate mba ofsStart sz = do
    loop ofsStart
  where
    end = ofsStart + sz

    loop ofs
        | ofs > end  = error "mutableValidate: internal error: went pass offset"
        | ofs == end = return (end, Nothing)
        | otherwise  = do
            r <- one ofs
            case r of
                (nextOfs, Nothing)  -> loop nextOfs
                (pos, Just failure) -> return (pos, Just failure)

    one pos = do
        h <- C.mutUnsafeRead mba pos
        let nbConts = getNbBytes h
        if nbConts == 0xff
            then return (pos, Just InvalidHeader)
            else if pos + 1 + nbConts > end
                then return (pos, Just MissingByte)
                else do
                    case nbConts of
                        0 -> return (pos + 1, Nothing)
                        1 -> do
                            c1 <- C.mutUnsafeRead mba (pos + 1)
                            if isContinuation c1
                                then return (pos + 2, Nothing)
                                else return (pos, Just InvalidContinuation)
                        2 -> do
                            c1 <- C.mutUnsafeRead mba (pos + 1)
                            c2 <- C.mutUnsafeRead mba (pos + 2)
                            if isContinuation c1 && isContinuation c2
                                then return (pos + 3, Nothing)
                                else return (pos, Just InvalidContinuation)
                        3 -> do
                            c1 <- C.mutUnsafeRead mba (pos + 1)
                            c2 <- C.mutUnsafeRead mba (pos + 2)
                            c3 <- C.mutUnsafeRead mba (pos + 3)
                            if isContinuation c1 && isContinuation c2 && isContinuation c3
                                then return (pos + 4, Nothing)
                                else return (pos, Just InvalidContinuation)
                        _ -> error "internal error"

skipNext :: String -> Int -> Int
skipNext (String ba) n = n + 1 + getNbBytes h
  where
    !h = Vec.unsafeIndex ba n

next :: String -> Int -> (# Char, Int #)
next (String ba) n =
    case getNbBytes# h of
        0# -> (# toChar h, n + 1 #)
        1# -> (# toChar (decode2 (Vec.unsafeIndex ba (n + 1))) , n + 2 #)
        2# -> (# toChar (decode3 (Vec.unsafeIndex ba (n + 1))
                                 (Vec.unsafeIndex ba (n + 2))) , n + 3 #)
        3# -> (# toChar (decode4 (Vec.unsafeIndex ba (n + 1))
                                 (Vec.unsafeIndex ba (n + 2))
                                 (Vec.unsafeIndex ba (n + 3))) , n + 4 #)
        r -> error ("next: internal error: invalid input: " <> show (I# r) <> " " <> show (W# h))
  where
    !(W8# h) = Vec.unsafeIndex ba n

    toChar :: Word# -> Char
    toChar w = C# (chr# (word2Int# w))

    decode2 :: Word8 -> Word#
    decode2 (W8# c1) =
        or# (uncheckedShiftL# (and# h 0x1f##) 6#)
            (and# c1 0x3f##)

    decode3 :: Word8 -> Word8 -> Word#
    decode3 (W8# c1) (W8# c2) =
        or# (uncheckedShiftL# (and# h 0xf##) 12#)
            (or# (uncheckedShiftL# (and# c1 0x3f##) 6#)
                 (and# c2 0x3f##))

    decode4 :: Word8 -> Word8 -> Word8 -> Word#
    decode4 (W8# c1) (W8# c2) (W8# c3) =
        or# (uncheckedShiftL# (and# h 0x7##) 18#)
            (or# (uncheckedShiftL# (and# c1 0x3f##) 12#)
                (or# (uncheckedShiftL# (and# c2 0x3f##) 6#)
                    (and# c3 0x3f##))
            )

write :: PrimMonad prim => MutableString (PrimState prim) -> Int -> Char -> prim Int
write (MutableString mba) idx c =
    if      bool# (ltWord# x 0x80##   ) then encode1
    else if bool# (ltWord# x 0x800##  ) then encode2
    else if bool# (ltWord# x 0x10000##) then encode3
    else                                     encode4
  where
    !(I# xi) = fromEnum c
    !x       = int2Word# xi

    encode1 = C.mutUnsafeWrite mba idx (W8# x) >> return (idx + 1)

    encode2 = do
        let x1  = or# (uncheckedShiftRL# x 6#) 0xc0##
            x2  = toContinuation x
        C.mutUnsafeWrite mba idx     (W8# x1)
        C.mutUnsafeWrite mba (idx+1) (W8# x2)
        return (idx + 2)

    encode3 = do
        let x1  = or# (uncheckedShiftRL# x 12#) 0xe0##
            x2  = toContinuation (uncheckedShiftRL# x 6#)
            x3  = toContinuation x
        C.mutUnsafeWrite mba idx     (W8# x1)
        C.mutUnsafeWrite mba (idx+1) (W8# x2)
        C.mutUnsafeWrite mba (idx+2) (W8# x3)
        return (idx + 3)

    encode4 = do
        let x1  = or# (uncheckedShiftRL# x 18#) 0xf0##
            x2  = toContinuation (uncheckedShiftRL# x 12#)
            x3  = toContinuation (uncheckedShiftRL# x 6#)
            x4  = toContinuation x
        C.mutUnsafeWrite mba idx     (W8# x1)
        C.mutUnsafeWrite mba (idx+1) (W8# x2)
        C.mutUnsafeWrite mba (idx+2) (W8# x3)
        C.mutUnsafeWrite mba (idx+3) (W8# x4)
        return (idx + 4)

    toContinuation :: Word# -> Word#
    toContinuation w = or# (and# w 0x3f##) 0x80##

freeze :: PrimMonad prim => MutableString (PrimState prim) -> prim String
freeze (MutableString mba) = String `fmap` C.unsafeFreeze mba
{-# INLINE freeze #-}

------------------------------------------------------------------------
-- real functions

sToList :: String -> [Char]
sToList s = loop 0
  where
    !nbBytes = size s
    loop n
        | n == nbBytes = []
        | otherwise    =
            let (# c , n' #) = next s n in c : loop n'

sFromList :: [Char] -> String
sFromList l = runST (new bytes >>= copy)
  where
    -- count how many bytes
    !bytes = Prelude.sum $ fmap (charToBytes . fromEnum) l

    copy :: MutableString (PrimState (ST st)) -> ST st String
    copy ms = loop 0 l
      where
        loop _   []     = freeze ms
        loop idx (c:xs) = write ms idx c >>= \idx' -> loop idx' xs
    -- write those bytes
    --loop :: MutableByteArray# st -> Int# -> State# st -> [Char] -> (# State# st, String #)

empty :: String
empty = String mempty

null :: String -> Bool
null (String ba) = C.length ba == 0

append :: String -> String -> String
append (String a) (String b) = String (mappend a b)

equal :: String -> String -> Bool
equal (String a) (String b) = a == b

compareString :: String -> String -> Ordering
compareString (String a) (String b) = compare a b

-- | Create a string composed of a number @n of Chars (Unicode code points).
--
-- if the input @s contains less characters than required, then
take :: Int -> String -> String
take n s = fst $ splitAt n s -- TODO specialize

-- | Create a string with the remaining Chars after dropping @n Chars from the beginning
drop :: Int -> String -> String
drop n s@(String ba)
    | n <= 0    = s
    | otherwise = loop 0 0
  where
    !sz = C.length ba
    loop idx i
        | idx >= sz = mempty
        | i == n    = String $ C.drop idx ba
        | otherwise = loop (skipNext s idx) (i + 1)

splitAt :: Int -> String -> (String, String)
splitAt n s@(String ba)
    | n <= 0    = (empty, s)
    | otherwise = loop 0 0
  where
    !sz = C.length ba
    loop idx i
        | idx >= sz = (s, mempty)
        | i == n    = let (v1,v2) = C.splitAt idx ba in (String v1, String v2)
        | otherwise = loop (skipNext s idx) (i + 1)

-- rev{Take,Drop,SplitAt} TODO optimise:
-- we can process the string from the end using a skipPrev instead of getting the length

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
splitOn predicate s@(String ba)
    | sz == 0   = []
    | otherwise = loop 0 0
  where
    !sz = C.length ba
    loop prevIdx idx
        | idx == sz = [sub s prevIdx idx]
        | otherwise =
            let (# c, idx' #) = next s idx
             in if predicate c
                    then sub s prevIdx idx : loop idx' idx'
                    else loop prevIdx idx'

sub :: String -> Int -> Int -> String
sub (String ba) start end = String $ Vec.sub ba start end

-- | Split at a given index.
splitIndex :: Int -> String -> (String, String)
splitIndex idx (String ba) = (String v1, String v2)
  where (v1,v2) = C.splitAt idx ba

break :: (Char -> Bool) -> String -> (String, String)
break predicate s@(String ba) = loop 0
  where
    !sz = C.length ba
    loop idx
        | idx == sz = (s, empty)
        | otherwise          =
            let (# c, idx' #) = next s idx
             in case predicate c of
                    True  -> splitIndex idx s
                    False -> loop idx'

span :: (Char -> Bool) -> String -> (String, String)
span predicate s = break (not . predicate) s

-- | size in bytes
size :: String -> Int
size (String ba) = C.length ba

length :: String -> Int
length s@(String ba) = loop 0 0
  where
    !sz     = C.length ba
    loop idx !i
        | idx == sz = i
        | otherwise =
            let idx' = skipNext s idx
             in loop idx' (i + 1)

replicate :: Int -> Char -> String
replicate n c = runST (new nbBytes >>= fill)
  where
    !nbBytes = sz * n
    sz       = charToBytes (fromEnum c)
    fill :: PrimMonad prim => MutableString (PrimState prim) -> prim String
    fill ms = loop 0
      where
        loop idx
            | idx == nbBytes = freeze ms
            | otherwise      = write ms idx c >>= loop

{-
sizeBytes :: String -> Int
sizeBytes (String ba) = I# (sizeofByteArray# ba)
-}

-- | Allocate a MutableString of a specific size in bytes.
new :: PrimMonad prim
    => Int -- ^ in number of bytes, not of elements.
    -> prim (MutableString (PrimState prim))
new n = MutableString `fmap` MVec.new n

create :: PrimMonad prim => Int -> (MutableString (PrimState prim) -> prim Int) -> prim String
create sz f = do
    ms     <- new sz
    filled <- f ms
    if filled == sz
        then freeze ms
        else C.take filled `fmap` freeze ms

charToBytes :: Int -> Int
charToBytes c
    | c < 0x80     = 1
    | c < 0x800    = 2
    | c < 0x10000  = 3
    | c < 0x110000 = 4
    | otherwise    = error ("invalid code point: " `mappend` show c)

charMap :: (Char -> Char) -> String -> String
charMap f src@(String srcBa) =
    let !(elems, nbBytes) = allocateAndFill [] 0 0
     in runST $ do
            dest <- new nbBytes
            copyLoop dest elems nbBytes
            freeze dest
  where
    !srcSz = C.length srcBa

    allocateAndFill :: [(String, Int)]
                    -> Int
                    -> Int
                    -> ([(String,Int)], Int)
    allocateAndFill acc idx bytesWritten
        | idx == srcSz = (acc, bytesWritten)
        | otherwise    =
            let (el@(_,addBytes), idx') = runST $ do
                    -- make sure we allocate at least 4 bytes for the destination for the last few bytes
                    -- otherwise allocating less would bring the danger of spinning endlessly
                    -- and never succeeding.
                    let !diffBytes = srcSz - idx
                        !allocatedBytes = if diffBytes <= 4 then 4 else diffBytes
                    ms <- new allocatedBytes
                    (dstIdx, srcIdx) <- fill ms allocatedBytes idx
                    s <- freeze ms
                    return ((s, dstIdx), srcIdx)
             in allocateAndFill (el : acc) idx' (bytesWritten + addBytes)

    fill :: PrimMonad prim
         => MutableString (PrimState prim)
         -> Int
         -> Int
         -> prim (Int, Int)
    fill mba dsz srcIdxOrig =
        loop 0 srcIdxOrig
      where
        loop dstIdx srcIdx
            | srcIdx == srcSz = return (dstIdx, srcIdx)
            | dstIdx == dsz   = return (dstIdx, srcIdx)
            | otherwise                =
                let (# c, srcIdx' #) = next src srcIdx
                    c' = f c -- the mapped char
                    !nbBytes = charToBytes (fromEnum c')
                 in -- check if we have room in the destination buffer
                    if dstIdx + nbBytes <= dsz
                        then do dstIdx' <- write mba dstIdx c'
                                loop dstIdx' srcIdx'
                        else return (dstIdx, srcIdx)

    copyLoop _   []     0        = return ()
    copyLoop _   []     n        = error ("charMap invalid: " <> show n)
    copyLoop ms@(MutableString mba) ((String ba, sz):xs) end = do
        let start = end - sz
        Vec.copyAtRO mba start ba 0 sz
        copyLoop ms xs start

snoc :: String -> Char -> String
snoc (String ba) c = runST $ do
    ms@(MutableString mba) <- new (len + nbBytes)
    Vec.copyAtRO mba 0 ba 0 len
    _ <- write ms len c
    freeze ms
  where
    !len     = C.length ba
    !nbBytes = charToBytes (fromEnum c)

cons :: Char -> String -> String
cons c s@(String ba) = runST $ do
    ms@(MutableString mba) <- new (len + nbBytes)
    idx <- write ms 0 c
    Vec.copyAtRO mba idx ba 0 len
    freeze ms
  where
    !len     = size s
    !nbBytes = charToBytes (fromEnum c)

unsnoc :: String -> Maybe (String, Char)
unsnoc s
    | null s    = Nothing
    | otherwise =
        let (s1,s2) = revSplitAt 1 s
         in case toList s1 of -- TODO use index instead of toList
                [c] -> Just (s2, c)
                _   -> error "impossible"

uncons :: String -> Maybe (Char, String)
uncons s
    | null s    = Nothing
    | otherwise =
        let (s1,s2) = splitAt 1 s
         in case toList s1 of -- TODO use index instead of ToList
                [c] -> Just (c, s2)
                _   -> error "impossible"

find :: (Char -> Bool) -> String -> Maybe Char
find predicate s = loop 0
  where
    !sz = size s
    loop idx
        | idx == sz = Nothing
        | otherwise =
            let (# c, idx' #) = next s idx
             in case predicate c of
                    True  -> Just c
                    False -> loop idx'

sortBy :: (Char -> Char -> Ordering) -> String -> String
sortBy sortF s = fromList $ Data.List.sortBy sortF $ toList s -- FIXME for tests

filter :: (Char -> Bool) -> String -> String
filter p s = fromList $ Data.List.filter p $ toList s

reverse :: String -> String
reverse s@(String ba) = runST $ do
    ms <- new len
    loop ms 0 len
  where
    !len = size s
    -- write those bytes
    loop :: PrimMonad prim => MutableString (PrimState prim) -> Int -> Int -> prim String
    loop ms@(MutableString mba) sidx didx
        | didx == 0 = freeze ms
        | otherwise = do
            let !h = Vec.unsafeIndex ba sidx
                !nb = getNbBytes h + 1
                d = didx - nb
            case nb of
                1 -> C.mutUnsafeWrite mba d      h
                2 -> do
                    C.mutUnsafeWrite mba d       h
                    C.mutUnsafeWrite mba (d + 1) (Vec.unsafeIndex ba (sidx + 1))
                3 -> do
                    C.mutUnsafeWrite mba d       h
                    C.mutUnsafeWrite mba (d + 1) (Vec.unsafeIndex ba (sidx + 1))
                    C.mutUnsafeWrite mba (d + 2) (Vec.unsafeIndex ba (sidx + 2))
                4 -> do
                    C.mutUnsafeWrite mba d       h
                    C.mutUnsafeWrite mba (d + 1) (Vec.unsafeIndex  ba (sidx + 1))
                    C.mutUnsafeWrite mba (d + 2) (Vec.unsafeIndex ba (sidx + 2))
                    C.mutUnsafeWrite mba (d + 3) (Vec.unsafeIndex ba (sidx + 3))
                _  -> return () -- impossible
            loop ms (sidx + nb) d

-- | String encoding
data Encoding = UTF8
    deriving (Show,Eq)

{-
-- | Convert a Byte Array to a string and check UTF8 validity
fromBytes :: Encoding -> ByteArray -> Maybe String
fromBytes UTF8 bytes =
    case validate bytes 0 (C.length bytes) of
        (_, Nothing) -> Just $ fromBytesUnsafe bytes
        (_, Just _)  -> Nothing
        -}

fromBytes :: Encoding -> ByteArray -> (String, ByteArray)
fromBytes UTF8 bytes =
    case validate bytes 0 (C.length bytes) of
        (_, Nothing)  -> (fromBytesUnsafe bytes, mempty)
        (pos, Just _) ->
            let (b1, b2) = C.splitAt pos bytes
             in (fromBytesUnsafe b1, b2)

fromChunkBytes :: [ByteArray] -> [String]
fromChunkBytes l = loop l
  where
    loop []         = []
    loop (bytes:[]) =
        case validate bytes 0 (C.length bytes) of
            (_, Nothing)  -> [fromBytesUnsafe bytes]
            (_, Just err) -> doErr err
    loop (bytes:cs@(c1:c2)) =
        case validate bytes 0 (C.length bytes) of
            (_, Nothing) -> fromBytesUnsafe bytes : loop cs
            (pos, Just MissingByte) ->
                let (b1,b2) = C.splitAt pos bytes
                 in fromBytesUnsafe b1 : loop ((b2 `mappend` c1) : c2)
            (_, Just err) -> doErr err
    doErr err = error ("fromChunkBytes: " <> show err)

-- | Convert a Byte Array directly to a string without checking for UTF8 validity
fromBytesUnsafe :: ByteArray -> String
fromBytesUnsafe = String

-- | Convert a String to a bytearray
toBytes :: Encoding -> String -> ByteArray
toBytes UTF8 (String ba) = ba
