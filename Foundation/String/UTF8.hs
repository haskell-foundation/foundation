-- |
-- Module      : Foundation.String.UTF8
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
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE FlexibleContexts           #-}
module Foundation.String.UTF8
    ( String(..)
    --, Buffer
    , create
    , replicate
    -- * Binary conversion
    , Encoding(..)
    , fromBytes
    , fromChunkBytes
    , fromBytesUnsafe
    , fromBytesLenient
    , toBytes
    , mutableValidate
    , copy
    , ValidationFailure(..)
    -- * Legacy utility
    , lines
    , words
    ) where

import           Foundation.Array.Unboxed           (UArray)
import qualified Foundation.Array.Unboxed           as Vec
import           Foundation.Array.Unboxed.ByteArray (MutableByteArray)
import qualified Foundation.Array.Unboxed.Mutable   as MVec
import qualified Foundation.Collection              as C
import           Foundation.Internal.Base
import           Foundation.Internal.Primitive
import           Foundation.Internal.Types
import           Foundation.Number
import           Foundation.Primitive.Monad
import           Foundation.Primitive.Types
import           Foundation.String.UTF8Table
import           GHC.Prim
import           GHC.ST
import           GHC.Types
import           GHC.Word
import           Foundation.Array.Unboxed.Builder (ArrayBuilder, appendTy)

 -- temporary
import qualified Data.List
import           Data.Data
import qualified Prelude

import           Foundation.String.ModifiedUTF8     (fromModified)
import           GHC.CString                  (unpackCString#,
                                               unpackCStringUtf8#)

import qualified Foundation.String.Encoding.Encoding   as Encoder
import qualified Foundation.String.Encoding.ASCII7     as Encoder
import qualified Foundation.String.Encoding.UTF16      as Encoder
import qualified Foundation.String.Encoding.UTF32      as Encoder
import qualified Foundation.String.Encoding.ISO_8859_1 as Encoder

-- | Opaque packed array of characters in the UTF8 encoding
newtype String = String (UArray Word8)
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
instance C.Collection String where
    null = null
    length = length
instance C.Sequential String where
    take = take
    drop = drop
    splitAt = splitAt
    revTake = revTake
    revDrop = revDrop
    revSplitAt = revSplitAt
    splitOn = splitOn
    break = break
    breakElem = breakElem
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
    singleton = fromList . (:[])

instance C.Zippable String where
  -- TODO Use a string builder once available
  zipWith f a b = sFromList (C.zipWith f a b)

data ValidationFailure = InvalidHeader
                       | InvalidContinuation
                       | MissingByte
                       deriving (Show,Eq,Typeable)

instance Exception ValidationFailure

data EncoderUTF8 = EncoderUTF8

instance Encoder.Encoding EncoderUTF8 where
    type Unit EncoderUTF8 = Word8
    type Error EncoderUTF8 = ValidationFailure
    encodingNext  _ = \ofs -> Right . nextWithIndexer ofs
    encodingWrite _ = writeWithBuilder

-- | Validate a bytearray for UTF8'ness
--
-- On success Nothing is returned
-- On Failure the position along with the failure reason
validate :: UArray Word8
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

skipNextHeaderValue :: Word8 -> Size Word8
skipNextHeaderValue !x
    | x < 0xC0  = Size 1 -- 0b11000000
    | x < 0xE0  = Size 2 -- 0b11100000
    | x < 0xF0  = Size 3 -- 0b11110000
    | otherwise = Size 4
{-# INLINE skipNextHeaderValue #-}

nextWithIndexer :: (Offset Word8 -> Word8)
                -> Offset Word8
                -> (Char, Offset Word8)
nextWithIndexer getter off =
    case getNbBytes# h of
        0# -> (toChar h, off + aone)
        1# -> (toChar (decode2 (getter $ off + aone)), off + atwo)
        2# -> (toChar (decode3 (getter $ off + aone) (getter $ off + atwo)), off + athree)
        3# -> (toChar (decode4 (getter $ off + aone) (getter $ off + atwo) (getter $ off + athree))
              , off + afour)
        r -> error ("next: internal error: invalid input: " <> show (I# r) <> " " <> show (W# h))
  where
    aone = Offset 1
    atwo = Offset 2
    athree = Offset 3
    afour = Offset 4
    !(W8# h) = getter off

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

writeWithBuilder :: (PrimMonad st, Monad st)
                 => Char
                 ->  ArrayBuilder Word8 st ()
writeWithBuilder c =
    if      bool# (ltWord# x 0x80##   ) then encode1
    else if bool# (ltWord# x 0x800##  ) then encode2
    else if bool# (ltWord# x 0x10000##) then encode3
    else                                     encode4
  where
    !(I# xi) = fromEnum c
    !x       = int2Word# xi

    encode1 = appendTy (W8# x)

    encode2 = do
        let x1  = or# (uncheckedShiftRL# x 6#) 0xc0##
            x2  = toContinuation x
        appendTy (W8# x1) >> appendTy (W8# x2)

    encode3 = do
        let x1  = or# (uncheckedShiftRL# x 12#) 0xe0##
            x2  = toContinuation (uncheckedShiftRL# x 6#)
            x3  = toContinuation x
        appendTy (W8# x1) >> appendTy (W8# x2) >> appendTy (W8# x3)

    encode4 = do
        let x1  = or# (uncheckedShiftRL# x 18#) 0xf0##
            x2  = toContinuation (uncheckedShiftRL# x 12#)
            x3  = toContinuation (uncheckedShiftRL# x 6#)
            x4  = toContinuation x
        appendTy (W8# x1) >> appendTy (W8# x2) >> appendTy (W8# x3) >> appendTy (W8# x4)

    toContinuation :: Word# -> Word#
    toContinuation w = or# (and# w 0x3f##) 0x80##


next :: String -> Offset8 -> (# Char, Offset8 #)
next (String ba) (Offset n) =
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

-- | Different way to encode a Character in UTF8 represented as an ADT
data UTF8Char =
      UTF8_1 {-# UNPACK #-} !Word8
    | UTF8_2 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
    | UTF8_3 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
    | UTF8_4 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8

writeBytes :: Char -> UTF8Char
writeBytes c =
    if      bool# (ltWord# x 0x80##   ) then encode1
    else if bool# (ltWord# x 0x800##  ) then encode2
    else if bool# (ltWord# x 0x10000##) then encode3
    else                                     encode4
  where
    !(I# xi) = fromEnum c
    !x       = int2Word# xi
    encode1 = UTF8_1 (W8# x)
    encode2 =
        let !x1  = W8# (or# (uncheckedShiftRL# x 6#) 0xc0##)
            !x2  = toContinuation x
         in UTF8_2 x1 x2
    encode3 =
        let !x1  = W8# (or# (uncheckedShiftRL# x 12#) 0xe0##)
            !x2  = toContinuation (uncheckedShiftRL# x 6#)
            !x3  = toContinuation x
         in UTF8_3 x1 x2 x3
    encode4 =
        let !x1  = W8# (or# (uncheckedShiftRL# x 18#) 0xf0##)
            !x2  = toContinuation (uncheckedShiftRL# x 12#)
            !x3  = toContinuation (uncheckedShiftRL# x 6#)
            !x4  = toContinuation x
         in UTF8_4 x1 x2 x3 x4
    toContinuation :: Word# -> Word8
    toContinuation w = W8# (or# (and# w 0x3f##) 0x80##)
    {-# INLINE toContinuation #-}

write :: PrimMonad prim => MutableString (PrimState prim) -> Offset8 -> Char -> prim Offset8
write (MutableString mba) (Offset i) c =
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


{-# RULES
"String sFromList" forall s .
  sFromList (unpackCString# s) = String $ fromModified s
  #-}
{-# RULES
"String sFromList" forall s .
  sFromList (unpackCStringUtf8# s) = String $ fromModified s
  #-}

sFromList :: [Char] -> String
sFromList l = runST (new bytes >>= startCopy)
  where
    -- count how many bytes
    !bytes = C.foldl' (+) (Size 0) $ fmap (charToBytes . fromEnum) l

    startCopy :: MutableString (PrimState (ST st)) -> ST st String
    startCopy ms = loop azero l
      where
        loop _   []     = freeze ms
        loop idx (c:xs) = write ms idx c >>= \idx' -> loop idx' xs
{-# INLINE [0] sFromList #-}

null :: String -> Bool
null (String ba) = C.length ba == 0

-- | Create a string composed of a number @n of Chars (Unicode code points).
--
-- if the input @s contains less characters than required, then
take :: Int -> String -> String
take n s@(String ba)
    | n <= 0           = mempty
    | n >= C.length ba = s
    | otherwise        = let (Offset o) = indexN n s in String $ Vec.take o ba

-- | Create a string with the remaining Chars after dropping @n Chars from the beginning
drop :: Int -> String -> String
drop n s@(String ba)
    | n <= 0           = s
    | n >= C.length ba = mempty
    | otherwise        = let (Offset o) = indexN n s in String $ Vec.drop o ba

splitAt :: Int -> String -> (String, String)
splitAt nI s@(String ba)
    | nI <= 0           = (mempty, s)
    | nI >= C.length ba = (s, mempty)
    | otherwise =
        let (Offset k) = indexN nI s
            (v1,v2)    = C.splitAt k ba
         in (String v1, String v2)

-- | Return the offset (in bytes) of the N'th sequence in an UTF8 String
indexN :: Int -> String -> Offset Word8
indexN nI (String ba) = Vec.unsafeDewrap goVec goAddr ba
  where
    !n = Size nI
    end :: Offset Char
    !end = Offset 0 `offsetPlusE` n

    goVec :: ByteArray# -> Offset Word8 -> Offset Word8
    goVec !ma !start = loop start (Offset 0)
      where
        !len = start `offsetPlusE` Vec.lengthSize ba
        loop :: Offset Word8 -> Offset Char -> Offset Word8
        loop !idx !i
            | idx >= len || i >= end = sizeAsOffset (idx - start)
            | otherwise              = loop (idx `offsetPlusE` d) (i + Offset 1)
          where d = skipNextHeaderValue (primBaIndex ma idx)
    {-# INLINE goVec #-}

    goAddr :: Ptr Word8 -> Offset Word8 -> ST s (Offset Word8)
    goAddr !(Ptr ptr) !start = return $ loop start (Offset 0)
      where
        !len = start `offsetPlusE` Vec.lengthSize ba
        loop :: Offset Word8 -> Offset Char -> Offset Word8
        loop !idx !i
            | idx >= len || i >= end = sizeAsOffset (idx - start)
            | otherwise              = loop (idx `offsetPlusE` d) (i + Offset 1)
          where d = skipNextHeaderValue (primAddrIndex ptr idx)
    {-# INLINE goAddr #-}
{-# INLINE indexN #-}

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
break predicate s@(String ba) = runST $ Vec.unsafeIndexer ba go
  where
    !sz = size s
    end = azero `offsetPlusE` sz

    go :: (Offset Word8 -> Word8) -> ST st (String, String)
    go getIdx = loop (Offset 0)
      where
        !nextI = nextWithIndexer getIdx
        loop idx
            | idx == end = return (s, mempty)
            | otherwise  = do
                let (c, idx') = nextI idx
                case predicate c of
                    True  -> return $ splitIndex idx s
                    False -> loop idx'
        {-# INLINE loop #-}
{-# INLINE [2] break #-}

{-# RULES "break (== 'c')" [3] forall c . break (== c) = breakElem c #-}

breakElem :: Char -> String -> (String, String)
breakElem !el s@(String ba) =
    case writeBytes el of
        UTF8_1 w -> let (# v1,v2 #) = Vec.splitElem w ba in (String v1, String v2)
        _        -> runST $ Vec.unsafeIndexer ba go
  where
    sz = size s
    end = azero `offsetPlusE` sz

    go :: (Offset Word8 -> Word8) -> ST st (String, String)
    go getIdx = loop (Offset 0)
      where
        !nextI = nextWithIndexer getIdx
        loop idx
            | idx == end = return (s, mempty)
            | otherwise  = do
                let (c, idx') = nextI idx
                case el == c of
                    True  -> return $ splitIndex idx s
                    False -> loop idx'

intersperse :: Char -> String -> String
intersperse sep src
    | srcLen <= 1 = src
    | otherwise   = runST $ unsafeCopyFrom src dstBytes (go sep)
  where
    !srcBytes = size src
    !srcLen   = length src
    dstBytes = srcBytes + ((srcLen - 1) `scale` charToBytes (fromEnum sep))

    lastSrc :: Offset Char
    lastSrc = Offset 0 `offsetPlusE` Size srcLen

    go :: Char -> String -> Offset Char -> Offset8 -> MutableString s -> Offset8 -> ST s (Offset8, Offset8)
    go sep' src' srcI srcIdx dst dstIdx
        | srcI == lastSrc = do
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
               -> Size8  -- ^ Length of the destination string in bytes
               -> (String -> Offset Char -> Offset8 -> MutableString s -> Offset8 -> ST s (Offset8, Offset8))
               -- ^ Function called for each character in the source String
               -> ST s String -- ^ Returns the filled new string
unsafeCopyFrom src dstBytes f = new dstBytes >>= fill (Offset 0) (Offset 0) (Offset 0) f >>= freeze
  where
    srcLen = length src
    end = Offset 0 `offsetPlusE` Size srcLen
    fill srcI srcIdx dstIdx f' dst'
        | srcI == end = return dst'
        | otherwise = do (nextSrcIdx, nextDstIdx) <- f' src srcI srcIdx dst' dstIdx
                         fill (srcI + Offset 1) nextSrcIdx nextDstIdx f' dst'

span :: (Char -> Bool) -> String -> (String, String)
span predicate s = break (not . predicate) s

-- | size in bytes
size :: String -> Size8
size (String ba) = Size $ C.length ba

lengthSize :: String -> Size Word8
lengthSize (String ba) = Vec.unsafeDewrap goVec goAddr ba
  where
    goVec ma start = loop start (Size 0)
      where
        !end = start `offsetPlusE` Vec.lengthSize ba
        loop !idx !i
            | idx >= end = i
            | otherwise  = loop (idx `offsetPlusE` d) (i + Size 1)
          where d = skipNextHeaderValue (primBaIndex ma idx)

    goAddr (Ptr ptr) start = return $ loop start (Size 0)
      where
        !end = start `offsetPlusE` Vec.lengthSize ba
        loop !idx !i
            | idx >= end = i
            | otherwise  = loop (idx `offsetPlusE` d) (i + Size 1)
          where d = skipNextHeaderValue (primAddrIndex ptr idx)

length :: String -> Int
length s = let (Size sz) = lengthSize s in sz

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

-- | Copy the String
copy :: String -> String
copy (String s) = String (Vec.copy s)

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
charMap f src =
    let !(elems, nbBytes) = allocateAndFill [] (Offset 0) (Size 0)
     in runST $ do
            dest <- new nbBytes
            copyLoop dest elems (Offset 0 `offsetPlusE` nbBytes)
            freeze dest
  where
    !srcSz = size src
    srcEnd = azero `offsetPlusE` srcSz

    allocateAndFill :: [(String, Size8)]
                    -> Offset8
                    -> Size8
                    -> ([(String,Size8)], Size8)
    allocateAndFill acc idx bytesWritten
        | idx == srcEnd = (acc, bytesWritten)
        | otherwise     =
            let (el@(_,addBytes), idx') = runST $ do
                    -- make sure we allocate at least 4 bytes for the destination for the last few bytes
                    -- otherwise allocating less would bring the danger of spinning endlessly
                    -- and never succeeding.
                    let !diffBytes = srcEnd - idx
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
            | srcIdx == srcEnd = return (offsetAsSize dstIdx, srcIdx)
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
        Vec.unsafeCopyAtRO mba start ba (Offset 0) sz
        copyLoop ms xs start

snoc :: String -> Char -> String
snoc s@(String ba) c
    | len == Size 0 = C.singleton c
    | otherwise     = runST $ do
        ms@(MutableString mba) <- new (len + nbBytes)
        Vec.unsafeCopyAtRO mba (Offset 0) ba (Offset 0) len
        _ <- write ms (azero `offsetPlusE` len) c
        freeze ms
  where
    !len     = size s
    !nbBytes = charToBytes (fromEnum c)

cons :: Char -> String -> String
cons c s@(String ba)
  | len == Size 0 = C.singleton c
  | otherwise     = runST $ do
      ms@(MutableString mba) <- new (len + nbBytes)
      idx <- write ms (Offset 0) c
      Vec.unsafeCopyAtRO mba idx ba (Offset 0) len
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
                _   -> internalError "unsnoc"

uncons :: String -> Maybe (Char, String)
uncons s
    | null s    = Nothing
    | otherwise =
        let (s1,s2) = splitAt 1 s
         in case toList s1 of -- TODO use index instead of ToList
                [c] -> Just (c, s2)
                _   -> internalError "uncons"

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

{-
-- | Convert a Byte Array to a string and check UTF8 validity
fromBytes :: Encoding -> UArray Word8 -> Maybe String
fromBytes UTF8 bytes =
    case validate bytes 0 (C.length bytes) of
        (_, Nothing) -> Just $ fromBytesUnsafe bytes
        (_, Just _)  -> Nothing
        -}

data Encoding
    = ASCII7
    | UTF8
    | UTF16
    | UTF32
    | ISO_8859_1
  deriving (Typeable, Data, Eq, Ord, Show, Enum, Bounded)

fromEncoderBytes :: ( Encoder.Encoding encoding
                    , Exception (Encoder.Error encoding)
                    , PrimType (Encoder.Unit encoding)
                    )
                 => encoding
                 -> UArray Word8
                 -> (String, Maybe ValidationFailure, UArray Word8)
fromEncoderBytes enc bytes =
    ( String $ runST $ Encoder.convertFromTo enc EncoderUTF8 (Vec.recast bytes)
    , Nothing
    , mempty
    )

-- | Convert a ByteArray to a string assuming a specific encoding.
--
-- It returns a 3-tuple of:
--
-- * The string that has been succesfully converted without any error
-- * An optional validation error
-- * The remaining buffer that hasn't been processed (either as a result of an error, or because the encoded sequence is not fully available)
--
-- Considering a stream of data that is fetched chunk by chunk, it's valid to assume
-- that some sequence might fall in a chunk boundary. When converting chunks,
-- if the error is Nothing and the remaining buffer is not empty, then this buffer
-- need to be prepended to the next chunk
fromBytes :: Encoding -> UArray Word8 -> (String, Maybe ValidationFailure, UArray Word8)
fromBytes ASCII7     bytes = fromEncoderBytes Encoder.ASCII7     bytes
fromBytes ISO_8859_1 bytes = fromEncoderBytes Encoder.ISO_8859_1 bytes
fromBytes UTF16      bytes = fromEncoderBytes Encoder.UTF16      bytes
fromBytes UTF32      bytes = fromEncoderBytes Encoder.UTF32      bytes
fromBytes UTF8       bytes
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

-- | Convert a UTF8 array of bytes to a String.
--
-- If there's any error in the stream, it will automatically
-- insert replacement bytes to replace invalid sequences.
--
-- In the case of sequence that fall in the middle of 2 chunks,
-- the remaining buffer is supposed to be preprended to the
-- next chunk, and resume the parsing.
fromBytesLenient :: UArray Word8 -> (String, UArray Word8)
fromBytesLenient bytes
    | C.null bytes = (mempty, mempty)
    | otherwise    =
        case validate bytes (Offset 0) (Size $ C.length bytes) of
            (_, Nothing)                   -> (fromBytesUnsafe bytes, mempty)
            (Offset pos, Just MissingByte) ->
                let (b1,b2) = C.splitAt pos bytes
                 in (fromBytesUnsafe b1, b2)
            (Offset pos, Just InvalidHeader) ->
                let (b1,b2) = C.splitAt pos bytes
                    (_,b3)  = C.splitAt 1 b2
                    (s3, r) = fromBytesLenient b3
                 in (mconcat [fromBytesUnsafe b1,replacement, s3], r)
            (Offset pos, Just InvalidContinuation) ->
                let (b1,b2) = C.splitAt pos bytes
                    (_,b3)  = C.splitAt 1 b2
                    (s3, r) = fromBytesLenient b3
                 in (mconcat [fromBytesUnsafe b1,replacement, s3], r)
  where
    -- This is the replacement character U+FFFD used for any invalid header or continuation
    replacement :: String
    !replacement = fromBytesUnsafe $ fromList [0xef,0xbf,0xbd]

fromChunkBytes :: [UArray Word8] -> [String]
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

-- | Convert a Byte Array representing UTF8 data directly to a string without checking for UTF8 validity
--
-- If the input contains invalid sequences, it will trigger runtime async errors when processing data.
--
-- In doubt, use 'fromBytes'
fromBytesUnsafe :: UArray Word8 -> String
fromBytesUnsafe = String

toEncoderBytes :: ( Encoder.Encoding encoding
                  , PrimType (Encoder.Unit encoding)
                  , Exception (Encoder.Error encoding)
                  )
               => encoding
               -> UArray Word8
               -> UArray Word8
toEncoderBytes enc bytes = Vec.recast (runST $ Encoder.convertFromTo EncoderUTF8 enc bytes)

-- | Convert a String to a bytearray in a specific encoding
--
-- if the encoding is UTF8, the underlying buffer is returned without extra allocation or any processing
--
-- In any other encoding, some allocation and processing are done to convert.
toBytes :: Encoding -> String -> UArray Word8
toBytes UTF8       (String bytes) = bytes
toBytes ASCII7     (String bytes) = toEncoderBytes Encoder.ASCII7     bytes
toBytes ISO_8859_1 (String bytes) = toEncoderBytes Encoder.ISO_8859_1 bytes
toBytes UTF16      (String bytes) = toEncoderBytes Encoder.UTF16      bytes
toBytes UTF32      (String bytes) = toEncoderBytes Encoder.UTF32      bytes

lines :: String -> [String]
lines = fmap fromList . Prelude.lines . toList

words :: String -> [String]
words = fmap fromList . Prelude.words . toList
