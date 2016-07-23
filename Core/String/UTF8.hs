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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import           Core.Internal.Types
import           Core.Internal.Primitive
import qualified Core.Collection as C
import           Core.Primitive.Types
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
    deriving (Typeable, Monoid, Eq, Ord)

newtype MutableString st = MutableString (MutableByteArray st)
    deriving (Typeable)

instance Show String where
    show = show . sToList
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
    intersperse = intersperse
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
                       deriving (Show,Eq,Typeable)

instance Exception ValidationFailure

-- | Validate a bytearray for UTF8'ness
--
-- On success Nothing is returned
-- On Failure the position along with the failure reason
validate :: ByteArray
         -> Offset8
         -> Size Word8
         -> (Offset8, Maybe ValidationFailure)
validate ba ofsStart sz = runST (Vec.unsafeIndexer ba go)
  where
    end = ofsStart `offsetPlusE` sz

    go :: (Offset Word8 -> Word8) -> ST s (Offset Word8, Maybe ValidationFailure)
    go getIdx = return $ loop ofsStart
      where
        loop ofs
            | ofs > end  = error "validate: internal error: went pass offset"
            | ofs == end = (end, Nothing)
            | otherwise  =
                case {-# SCC "validate.one" #-} one ofs of
                    (nextOfs, Nothing)  -> loop nextOfs
                    (pos, Just failure) -> (pos, Just failure)

        one pos =
            case nbConts of
                0    -> (pos + o1, Nothing)
                0xff -> (pos, Just InvalidHeader)
                _ | (pos + o1) `offsetPlusE` nbContsE > end -> (pos, Just MissingByte)
                1    ->
                    let c1 = getIdx (pos + o1)
                     in if isContinuation c1
                            then (pos + Offset 2, Nothing)
                            else (pos, Just InvalidContinuation)
                2 ->
                    let c1 = getIdx (pos + o1)
                        c2 = getIdx (pos + Offset 2)
                     in if isContinuation c1 && isContinuation c2
                            then (pos + Offset 3, Nothing)
                            else (pos, Just InvalidContinuation)
                3 ->
                    let c1 = getIdx (pos + Offset 1)
                        c2 = getIdx (pos + Offset 2)
                        c3 = getIdx (pos + Offset 3)
                     in if isContinuation c1 && isContinuation c2 && isContinuation c3
                            then (pos + Offset 4, Nothing)
                            else (pos, Just InvalidContinuation)
                _ -> error "internal error"
          where
            !h = getIdx pos
            !nbContsE@(Size nbConts) = Size $ getNbBytes h
    {-# INLINE go #-}

    o1 = Offset 1

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

next :: String -> Offset8 -> (# Char, Offset8 #)
next (String ba) ofs@(Offset n) =
    case getNbBytes# h of
        0# -> (# toChar h, Offset $ n + 1 #)
        1# -> (# toChar (decode2 (Vec.unsafeIndex ba (n + 1))) , Offset $ n + 2 #)
        2# -> (# toChar (decode3 (Vec.unsafeIndex ba (n + 1))
                                 (Vec.unsafeIndex ba (n + 2))) , Offset $ n + 3 #)
        3# -> (# toChar (decode4 (Vec.unsafeIndex ba (n + 1))
                                 (Vec.unsafeIndex ba (n + 2))
                                 (Vec.unsafeIndex ba (n + 3))) , Offset $ n + 4 #)
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

write :: PrimMonad prim => MutableString (PrimState prim) -> Offset8 -> Char -> prim Offset8
write (MutableString mba) idx@(Offset i) c =
    if      bool# (ltWord# x 0x80##   ) then encode1
    else if bool# (ltWord# x 0x800##  ) then encode2
    else if bool# (ltWord# x 0x10000##) then encode3
    else                                     encode4
  where
    !(I# xi) = fromEnum c
    !x       = int2Word# xi

    encode1 = C.mutUnsafeWrite mba i (W8# x) >> return (Offset $ i + 1)

    encode2 = do
        let x1  = or# (uncheckedShiftRL# x 6#) 0xc0##
            x2  = toContinuation x
        C.mutUnsafeWrite mba i     (W8# x1)
        C.mutUnsafeWrite mba (i+1) (W8# x2)
        return $ Offset (i + 2)

    encode3 = do
        let x1  = or# (uncheckedShiftRL# x 12#) 0xe0##
            x2  = toContinuation (uncheckedShiftRL# x 6#)
            x3  = toContinuation x
        C.mutUnsafeWrite mba i     (W8# x1)
        C.mutUnsafeWrite mba (i+1) (W8# x2)
        C.mutUnsafeWrite mba (i+2) (W8# x3)
        return $ Offset (i + 3)

    encode4 = do
        let x1  = or# (uncheckedShiftRL# x 18#) 0xf0##
            x2  = toContinuation (uncheckedShiftRL# x 12#)
            x3  = toContinuation (uncheckedShiftRL# x 6#)
            x4  = toContinuation x
        C.mutUnsafeWrite mba i     (W8# x1)
        C.mutUnsafeWrite mba (i+1) (W8# x2)
        C.mutUnsafeWrite mba (i+2) (W8# x3)
        C.mutUnsafeWrite mba (i+3) (W8# x4)
        return $ Offset (i + 4)

    toContinuation :: Word# -> Word#
    toContinuation w = or# (and# w 0x3f##) 0x80##

freeze :: PrimMonad prim => MutableString (PrimState prim) -> prim String
freeze (MutableString mba) = String `fmap` C.unsafeFreeze mba
{-# INLINE freeze #-}

------------------------------------------------------------------------
-- real functions

sToList :: String -> [Char]
sToList s = loop azero
  where
    !nbBytes = size s
    end = azero `offsetPlusE` nbBytes
    loop idx
        | idx == end = []
        | otherwise  =
            let (# c , idx' #) = next s idx in c : loop idx'

sFromList :: [Char] -> String
sFromList l = runST (new bytes >>= copy)
  where
    -- count how many bytes
    !bytes = C.foldl' (+) (Size 0) $ fmap (charToBytes . fromEnum) l

    copy :: MutableString (PrimState (ST st)) -> ST st String
    copy ms = loop azero l
      where
        loop _   []     = freeze ms
        loop idx (c:xs) = write ms idx c >>= \idx' -> loop idx' xs
    -- write those bytes
    --loop :: MutableByteArray# st -> Int# -> State# st -> [Char] -> (# State# st, String #)

null :: String -> Bool
null (String ba) = C.length ba == 0

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
    | n <= 0    = (mempty, s)
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
splitOn predicate s
    | sz == Size 0 = []
    | otherwise    = loop azero azero
  where
    !sz = size s
    end = azero `offsetPlusE` sz
    loop prevIdx idx
        | idx == end = [sub s prevIdx idx]
        | otherwise =
            let (# c, idx' #) = next s idx
             in if predicate c
                    then sub s prevIdx idx : loop idx' idx'
                    else loop prevIdx idx'

sub :: String -> Offset8 -> Offset8 -> String
sub (String ba) (Offset start) (Offset end) = String $ Vec.sub ba start end

-- | Split at a given index.
splitIndex :: Offset8 -> String -> (String, String)
splitIndex (Offset idx) (String ba) = (String v1, String v2)
  where (v1,v2) = C.splitAt idx ba

break :: (Char -> Bool) -> String -> (String, String)
break predicate s@(String ba) = loop (Offset 0)
  where
    !sz = size s
    end = azero `offsetPlusE` sz
    loop idx
        | idx == end = (s, mempty)
        | otherwise  =
            let (# c, idx' #) = next s idx
             in case predicate c of
                    True  -> splitIndex idx s
                    False -> loop idx'

intersperse :: Char -> String -> String
intersperse sep src
    | srcLen <= 1 = src
    | otherwise   = runST $ unsafeCopyFrom src dstBytes (go sep)
  where
    !srcBytes = size src
    !srcLen   = length src
    !dstBytes = srcBytes + ((srcLen - 1) * charToBytes (fromEnum sep))
    go :: Char -> String -> Int -> Int -> MutableString s -> Int -> ST s (Int, Int)
    go sep' src' srcI srcIdx dst dstIdx
        | srcI == srcLen - 1 = do
            nextDstIdx <- write dst dstIdx c
            return (nextSrcIdx, nextDstIdx)
        | otherwise          = do
            nextDstIdx  <- write dst dstIdx c
            nextDstIdx' <- write dst nextDstIdx sep'
            return (nextSrcIdx, nextDstIdx')
      where
        (# c, nextSrcIdx #) = next src' srcIdx

-- | Allocate a new @String@ with a fill function that has access to the characters of
--   the source @String@.
unsafeCopyFrom :: String -- ^ Source string
               -> Int -- ^ Length of the destination string in bytes
               -> (String -> Int -> Int -> MutableString s -> Int -> ST s (Int, Int))
               -- ^ Function called for each character in the source String
               -> ST s String -- ^ Returns the filled new string
unsafeCopyFrom src dstBytes f = new dstBytes >>= fill 0 0 0 f >>= freeze
  where srcLen = length src
        fill srcI srcIdx dstIdx f' dst'
            | srcI == srcLen = return dst'
            | otherwise = do (nextSrcIdx, nextDstIdx) <- f' src srcI srcIdx dst' dstIdx
                             fill (srcI + 1) nextSrcIdx nextDstIdx f' dst'

span :: (Char -> Bool) -> String -> (String, String)
span predicate s = break (not . predicate) s

-- | size in bytes
size :: String -> Size8
size (String ba) = Size $ C.length ba

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
    end       = azero `offsetPlusE` nbBytes
    nbBytes   = Size $ sz * n
    (Size sz) = charToBytes (fromEnum c)
    fill :: PrimMonad prim => MutableString (PrimState prim) -> prim String
    fill ms = loop (Offset 0)
      where
        loop idx
            | idx == end = freeze ms
            | otherwise  = write ms idx c >>= loop

{-
sizeBytes :: String -> Int
sizeBytes (String ba) = I# (sizeofByteArray# ba)
-}

-- | Allocate a MutableString of a specific size in bytes.
new :: PrimMonad prim
    => Size8 -- ^ in number of bytes, not of elements.
    -> prim (MutableString (PrimState prim))
new n = MutableString `fmap` MVec.new n

create :: PrimMonad prim => Int -> (MutableString (PrimState prim) -> prim Int) -> prim String
create sz f = do
    ms     <- new (Size sz)
    filled <- f ms
    if filled == sz
        then freeze ms
        else C.take filled `fmap` freeze ms

charToBytes :: Int -> Size8
charToBytes c
    | c < 0x80     = Size 1
    | c < 0x800    = Size 2
    | c < 0x10000  = Size 3
    | c < 0x110000 = Size 4
    | otherwise    = error ("invalid code point: " `mappend` show c)

charMap :: (Char -> Char) -> String -> String
charMap f src@(String srcBa) =
    let !(elems, nbBytes) = allocateAndFill [] (Offset 0) (Size 0)
     in runST $ do
            dest <- new nbBytes
            copyLoop dest elems (Offset 0 `offsetPlusE` nbBytes)
            freeze dest
  where
    !srcSz = size src
    end    = azero `offsetPlusE` srcSz

    allocateAndFill :: [(String, Size8)]
                    -> Offset8
                    -> Size8
                    -> ([(String,Size8)], Size8)
    allocateAndFill acc idx bytesWritten
        | idx == end = (acc, bytesWritten)
        | otherwise  =
            let (el@(_,addBytes), idx') = runST $ do
                    -- make sure we allocate at least 4 bytes for the destination for the last few bytes
                    -- otherwise allocating less would bring the danger of spinning endlessly
                    -- and never succeeding.
                    let !diffBytes = end - idx
                        !allocatedBytes = if diffBytes <= Size 4 then Size 4 else diffBytes
                    ms <- new allocatedBytes
                    (dstIdx, srcIdx) <- fill ms allocatedBytes idx
                    s <- freeze ms
                    return ((s, dstIdx), srcIdx)
             in allocateAndFill (el : acc) idx' (bytesWritten + addBytes)

    fill :: PrimMonad prim
         => MutableString (PrimState prim)
         -> Size8
         -> Offset8
         -> prim (Size8, Offset8)
    fill mba dsz srcIdxOrig =
        loop (Offset 0) srcIdxOrig
      where
        endDst = (Offset 0) `offsetPlusE` dsz
        loop dstIdx srcIdx
            | srcIdx == end    = return (offsetAsSize dstIdx, srcIdx)
            | dstIdx == endDst = return (offsetAsSize dstIdx, srcIdx)
            | otherwise        =
                let (# c, srcIdx' #) = next src srcIdx
                    c' = f c -- the mapped char
                    !nbBytes = charToBytes (fromEnum c')
                 in -- check if we have room in the destination buffer
                    if dstIdx `offsetPlusE` nbBytes <= sizeAsOffset dsz
                        then do dstIdx' <- write mba dstIdx c'
                                loop dstIdx' srcIdx'
                        else return (offsetAsSize dstIdx, srcIdx)

    copyLoop _   []     (Offset 0) = return ()
    copyLoop _   []     n          = error ("charMap invalid: " <> show n)
    copyLoop ms@(MutableString mba) ((String ba, sz):xs) end = do
        let start = end `offsetMinusE` sz
        Vec.copyAtRO mba start ba (Offset 0) sz
        copyLoop ms xs start

snoc :: String -> Char -> String
snoc s@(String ba) c = runST $ do
    ms@(MutableString mba) <- new (len + nbBytes)
    Vec.copyAtRO mba (Offset 0) ba (Offset 0) len
    _ <- write ms (azero `offsetPlusE` len) c
    freeze ms
  where
    !len     = size s
    !nbBytes = charToBytes (fromEnum c)

cons :: Char -> String -> String
cons c s@(String ba) = runST $ do
    ms@(MutableString mba) <- new (len + nbBytes)
    idx <- write ms (Offset 0) c
    Vec.copyAtRO mba idx ba (Offset 0) len
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
find predicate s = loop (Offset 0)
  where
    !sz = size s
    end = Offset 0 `offsetPlusE` sz
    loop idx
        | idx == end = Nothing
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
    loop ms (Offset 0) (Offset 0 `offsetPlusE` len)
  where
    !len = size s
    -- write those bytes
    loop :: PrimMonad prim => MutableString (PrimState prim) -> Offset8 -> Offset8 -> prim String
    loop ms@(MutableString mba) sidx@(Offset si) didx
        | didx == Offset 0 = freeze ms
        | otherwise = do
            let !h = Vec.unsafeIndex ba si
                !nb = Size (getNbBytes h + 1)
                didx'@(Offset d) = didx `offsetMinusE` nb
            case nb of
                Size 1 -> C.mutUnsafeWrite mba d      h
                Size 2 -> do
                    C.mutUnsafeWrite mba d       h
                    C.mutUnsafeWrite mba (d + 1) (Vec.unsafeIndex ba (si + 1))
                Size 3 -> do
                    C.mutUnsafeWrite mba d       h
                    C.mutUnsafeWrite mba (d + 1) (Vec.unsafeIndex ba (si + 1))
                    C.mutUnsafeWrite mba (d + 2) (Vec.unsafeIndex ba (si + 2))
                Size 4 -> do
                    C.mutUnsafeWrite mba d       h
                    C.mutUnsafeWrite mba (d + 1) (Vec.unsafeIndex  ba (si + 1))
                    C.mutUnsafeWrite mba (d + 2) (Vec.unsafeIndex ba (si + 2))
                    C.mutUnsafeWrite mba (d + 3) (Vec.unsafeIndex ba (si + 3))
                _  -> return () -- impossible
            loop ms (sidx `offsetPlusE` nb) didx'

-- | String encoding
data Encoding = UTF8
    -- UTF8_Lenient
    deriving (Show,Eq)

{-
-- | Convert a Byte Array to a string and check UTF8 validity
fromBytes :: Encoding -> ByteArray -> Maybe String
fromBytes UTF8 bytes =
    case validate bytes 0 (C.length bytes) of
        (_, Nothing) -> Just $ fromBytesUnsafe bytes
        (_, Just _)  -> Nothing
        -}

fromBytes :: Encoding -> ByteArray -> (String, Maybe ValidationFailure, ByteArray)
fromBytes UTF8 bytes
    | C.null bytes = (mempty, Nothing, mempty)
    | otherwise    =
        case validate bytes (Offset 0) (Size $ C.length bytes) of
            (_, Nothing)  -> (fromBytesUnsafe bytes, Nothing, mempty)
            (Offset pos, Just vf) ->
                let (b1, b2) = C.splitAt pos bytes
                 in (fromBytesUnsafe b1, toErr vf, b2)
  where
    toErr MissingByte         = Nothing
    toErr InvalidHeader       = Just InvalidHeader
    toErr InvalidContinuation = Just InvalidContinuation

fromChunkBytes :: [ByteArray] -> [String]
fromChunkBytes l = loop l
  where
    loop []         = []
    loop (bytes:[]) =
        case validate bytes (Offset 0) (Size $ C.length bytes) of
            (_, Nothing)  -> [fromBytesUnsafe bytes]
            (_, Just err) -> doErr err
    loop (bytes:cs@(c1:c2)) =
        case validate bytes (Offset 0) (Size $ C.length bytes) of
            (_, Nothing) -> fromBytesUnsafe bytes : loop cs
            (Offset pos, Just MissingByte) ->
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
