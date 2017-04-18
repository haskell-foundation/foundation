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
{-# LANGUAGE CPP                        #-}
module Foundation.String.UTF8
    ( String(..)
    , MutableString(..)
    , create
    , replicate
    , length
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
    , index
    , sToList
    , null
    , drop
    , take
    , splitAt
    , revDrop
    , revTake
    , revSplitAt
    , splitOn
    , sub
    , elem
    , intersperse
    , span
    , break
    , breakElem
    , singleton
    , charMap
    , snoc
    , cons
    , unsnoc
    , uncons
    , find
    , findIndex
    , sortBy
    , filter
    , reverse
    , builderAppend
    , builderBuild
    , readInteger
    , readIntegral
    , readNatural
    , readDouble
    , readFloatingExact
    -- * Legacy utility
    , lines
    , words
    ) where

import           Foundation.Array.Unboxed           (UArray)
import qualified Foundation.Array.Unboxed           as Vec
import qualified Foundation.Array.Unboxed           as C
import           Foundation.Array.Unboxed.ByteArray (MutableByteArray)
import qualified Foundation.Array.Unboxed.Mutable   as MVec
import           Foundation.Internal.Base
import           Foundation.Bits
import           Foundation.Internal.Natural
import           Foundation.Internal.MonadTrans
import           Foundation.Internal.Primitive
import           Foundation.Primitive.Types.OffsetSize
import           Foundation.Numerical
import           Foundation.Primitive.Monad
import           Foundation.Primitive.Types
import           Foundation.Primitive.IntegralConv
import           Foundation.Primitive.Floating
import           Foundation.Boot.Builder
import qualified Foundation.Boot.List as List
import           Foundation.String.UTF8Table
import           GHC.Prim
import           GHC.ST
import           GHC.Types
import           GHC.Word
#if MIN_VERSION_base(4,9,0)
import           GHC.Char
#endif

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

instance Data String where
    toConstr s   = mkConstr stringType (show s) [] Prefix
    dataTypeOf _ = stringType
    gunfold _ _  = error "gunfold"

stringType :: DataType
stringType = mkNoRepType "Foundation.String"

-- | Mutable String Buffer.
--
-- Note that it's hard to use properly since the UTF8 encoding
-- is variable, and thus you can't mutable previously filled
-- data without potentially have to move all the data around.
--
-- See 'charMap' for an idea of the scale of the problem
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

-- | Possible failure related to validating bytes of UTF8 sequences.
data ValidationFailure = InvalidHeader
                       | InvalidContinuation
                       | MissingByte
                       deriving (Show,Eq,Typeable)

instance Exception ValidationFailure

-- | UTF8 Encoder
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
                0    -> (pos + 1, Nothing)
                0xff -> (pos, Just InvalidHeader)
                _ | (pos + 1) `offsetPlusE` nbContsE > end -> (pos, Just MissingByte)
                1    ->
                    let c1 = getIdx (pos + 1)
                     in if isContinuation c1
                            then (pos + 2, Nothing)
                            else (pos, Just InvalidContinuation)
                2 ->
                    let c1 = getIdx (pos + 1)
                        c2 = getIdx (pos + 2)
                     in if isContinuation c1 && isContinuation c2
                            then (pos + 3, Nothing)
                            else (pos, Just InvalidContinuation)
                3 ->
                    let c1 = getIdx (pos + 1)
                        c2 = getIdx (pos + 2)
                        c3 = getIdx (pos + 3)
                     in if isContinuation c1 && isContinuation c2 && isContinuation c3
                            then (pos + 4, Nothing)
                            else (pos, Just InvalidContinuation)
                _ -> error "internal error"
          where
            !h = getIdx pos
            !nbContsE@(Size nbConts) = Size $ getNbBytes h
    {-# INLINE go #-}

-- | Similar to 'validate' but works on a 'MutableByteArray'
mutableValidate :: PrimMonad prim
                => MutableByteArray (PrimState prim)
                -> Offset Word8
                -> Size Word8
                -> prim (Offset Word8, Maybe ValidationFailure)
mutableValidate mba ofsStart sz = do
    loop ofsStart
  where
    end = ofsStart `offsetPlusE` sz

    loop ofs
        | ofs > end  = error "mutableValidate: internal error: went pass offset"
        | ofs == end = return (end, Nothing)
        | otherwise  = do
            r <- one ofs
            case r of
                (nextOfs, Nothing)  -> loop nextOfs
                (pos, Just failure) -> return (pos, Just failure)

    one pos = do
        h <- Vec.unsafeRead mba pos
        let nbConts = getNbBytes h
        if nbConts == 0xff
            then return (pos, Just InvalidHeader)
            else if pos + 1 + Offset nbConts > end
                then return (pos, Just MissingByte)
                else do
                    case nbConts of
                        0 -> return (pos + 1, Nothing)
                        1 -> do
                            c1 <- Vec.unsafeRead mba (pos + 1)
                            if isContinuation c1
                                then return (pos + 2, Nothing)
                                else return (pos, Just InvalidContinuation)
                        2 -> do
                            c1 <- Vec.unsafeRead mba (pos + 1)
                            c2 <- Vec.unsafeRead mba (pos + 2)
                            if isContinuation c1 && isContinuation c2
                                then return (pos + 3, Nothing)
                                else return (pos, Just InvalidContinuation)
                        3 -> do
                            c1 <- Vec.unsafeRead mba (pos + 1)
                            c2 <- Vec.unsafeRead mba (pos + 2)
                            c3 <- Vec.unsafeRead mba (pos + 3)
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
                 -> Builder (UArray Word8) (MVec.MUArray Word8) Word8 st ()
writeWithBuilder c =
    if      bool# (ltWord# x 0x80##   ) then encode1
    else if bool# (ltWord# x 0x800##  ) then encode2
    else if bool# (ltWord# x 0x10000##) then encode3
    else                                     encode4
  where
    !(I# xi) = fromEnum c
    !x       = int2Word# xi

    encode1 = Vec.builderAppend (W8# x)

    encode2 = do
        let x1  = or# (uncheckedShiftRL# x 6#) 0xc0##
            x2  = toContinuation x
        Vec.builderAppend (W8# x1) >> Vec.builderAppend (W8# x2)

    encode3 = do
        let x1  = or# (uncheckedShiftRL# x 12#) 0xe0##
            x2  = toContinuation (uncheckedShiftRL# x 6#)
            x3  = toContinuation x
        Vec.builderAppend (W8# x1) >> Vec.builderAppend (W8# x2) >> Vec.builderAppend (W8# x3)

    encode4 = do
        let x1  = or# (uncheckedShiftRL# x 18#) 0xf0##
            x2  = toContinuation (uncheckedShiftRL# x 12#)
            x3  = toContinuation (uncheckedShiftRL# x 6#)
            x4  = toContinuation x
        Vec.builderAppend (W8# x1) >> Vec.builderAppend (W8# x2) >> Vec.builderAppend (W8# x3) >> Vec.builderAppend (W8# x4)

    toContinuation :: Word# -> Word#
    toContinuation w = or# (and# w 0x3f##) 0x80##

-- A variant of 'next' when you want the next character
-- to be ASCII only. if Bool is False, then it's not ascii,
-- otherwise it is and the return Word8 is valid.
nextAscii :: String -> Offset8 -> (# Word8, Bool #)
nextAscii (String ba) n = (# w, not (testBit w 7) #)
  where
    !w = Vec.unsafeIndex ba n

-- same as nextAscii but with a ByteArray#
nextAsciiBA :: ByteArray# -> Offset8 -> (# Word8, Bool #)
nextAsciiBA ba n = (# w, not (testBit w 7) #)
  where
    !w = primBaIndex ba n
{-# INLINE nextAsciiBA #-}

-- same as nextAscii but with a ByteArray#
nextAsciiPtr :: Ptr Word8 -> Offset8 -> (# Word8, Bool #)
nextAsciiPtr (Ptr addr) n = (# w, not (testBit w 7) #)
  where !w = primAddrIndex addr n
{-# INLINE nextAsciiPtr #-}

-- | nextAsciiBa specialized to get a digit between 0 and 9 (included)
nextAsciiDigitBA :: ByteArray# -> Offset8 -> (# Word8, Bool #)
nextAsciiDigitBA ba n = (# d, d < 0xa #)
  where !d = primBaIndex ba n - 0x30
{-# INLINE nextAsciiDigitBA #-}

nextAsciiDigitPtr :: Ptr Word8 -> Offset8 -> (# Word8, Bool #)
nextAsciiDigitPtr (Ptr addr) n = (# d, d < 0xa #)
  where !d = primAddrIndex addr n - 0x30
{-# INLINE nextAsciiDigitPtr #-}

expectAscii :: String -> Offset8 -> Word8 -> Bool
expectAscii (String ba) n v = Vec.unsafeIndex ba n == v
{-# INLINE expectAscii #-}

expectAsciiBA :: ByteArray# -> Offset8 -> Word8 -> Bool
expectAsciiBA ba n v = primBaIndex ba n == v
{-# INLINE expectAsciiBA #-}

expectAsciiPtr :: Ptr Word8 -> Offset8 -> Word8 -> Bool
expectAsciiPtr (Ptr ptr) n v = primAddrIndex ptr n == v
{-# INLINE expectAsciiPtr #-}

next :: String -> Offset8 -> (# Char, Offset8 #)
next (String ba) n =
    case getNbBytes# h of
        0# -> (# toChar h, n + 1 #)
        1# -> (# toChar (decode2 (Vec.unsafeIndex ba (n + 1))) , n + 2 #)
        2# -> (# toChar (decode3 (Vec.unsafeIndex ba (n + 1))
                                 (Vec.unsafeIndex ba (n + 2))) , n + 3 #)
        3# -> (# toChar (decode4 (Vec.unsafeIndex ba (n + 1))
                                 (Vec.unsafeIndex ba (n + 2))
                                 (Vec.unsafeIndex ba (n + 3))) , n + 4 #)
        r -> error ("next: internal error: invalid input: offset=" <> show n <> " table=" <> show (I# r) <> " h=" <> show (W# h))
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

asUTF8Char :: Char -> UTF8Char
asUTF8Char !c
  | bool# (ltWord# x 0x80##   ) = encode1
  | bool# (ltWord# x 0x800##  ) = encode2
  | bool# (ltWord# x 0x10000##) = encode3
  | otherwise                   = encode4
    where
      !(I# xi) = fromEnum c
      !x       = int2Word# xi

      encode1 = UTF8_1 (W8# x)
      encode2 =
          let !x1 = W8# (or# (uncheckedShiftRL# x 6#) 0xc0##)
              !x2 = toContinuation x
           in UTF8_2 x1 x2
      encode3 =
          let !x1 = W8# (or# (uncheckedShiftRL# x 12#) 0xe0##)
              !x2 = toContinuation (uncheckedShiftRL# x 6#)
              !x3 = toContinuation x
           in UTF8_3 x1 x2 x3
      encode4 =
          let !x1 = W8# (or# (uncheckedShiftRL# x 18#) 0xf0##)
              !x2 = toContinuation (uncheckedShiftRL# x 12#)
              !x3 = toContinuation (uncheckedShiftRL# x 6#)
              !x4 = toContinuation x
           in UTF8_4 x1 x2 x3 x4

      toContinuation :: Word# -> Word8
      toContinuation w = W8# (or# (and# w 0x3f##) 0x80##)
      {-# INLINE toContinuation #-}

numBytes :: UTF8Char -> Size8
numBytes UTF8_1{} = Size 1
numBytes UTF8_2{} = Size 2
numBytes UTF8_3{} = Size 3
numBytes UTF8_4{} = Size 4

writeUTF8Char :: PrimMonad prim => MutableString (PrimState prim) -> Offset8 -> UTF8Char -> prim ()
writeUTF8Char (MutableString mba) i (UTF8_1 x1) =
    Vec.unsafeWrite mba i     x1
writeUTF8Char (MutableString mba) i (UTF8_2 x1 x2) = do
    Vec.unsafeWrite mba i     x1
    Vec.unsafeWrite mba (i+1) x2
writeUTF8Char (MutableString mba) i (UTF8_3 x1 x2 x3) = do
    Vec.unsafeWrite mba i     x1
    Vec.unsafeWrite mba (i+1) x2
    Vec.unsafeWrite mba (i+2) x3
writeUTF8Char (MutableString mba) i (UTF8_4 x1 x2 x3 x4) = do
    Vec.unsafeWrite mba i     x1
    Vec.unsafeWrite mba (i+1) x2
    Vec.unsafeWrite mba (i+2) x3
    Vec.unsafeWrite mba (i+3) x4
{-# INLINE writeUTF8Char #-}

write :: PrimMonad prim => MutableString (PrimState prim) -> Offset8 -> Char -> prim Offset8
write (MutableString mba) i c =
    if      bool# (ltWord# x 0x80##   ) then encode1
    else if bool# (ltWord# x 0x800##  ) then encode2
    else if bool# (ltWord# x 0x10000##) then encode3
    else                                     encode4
  where
    !(I# xi) = fromEnum c
    !x       = int2Word# xi

    encode1 = Vec.unsafeWrite mba i (W8# x) >> return (i + 1)

    encode2 = do
        let x1  = or# (uncheckedShiftRL# x 6#) 0xc0##
            x2  = toContinuation x
        Vec.unsafeWrite mba i     (W8# x1)
        Vec.unsafeWrite mba (i+1) (W8# x2)
        return (i + 2)

    encode3 = do
        let x1  = or# (uncheckedShiftRL# x 12#) 0xe0##
            x2  = toContinuation (uncheckedShiftRL# x 6#)
            x3  = toContinuation x
        Vec.unsafeWrite mba i     (W8# x1)
        Vec.unsafeWrite mba (i+1) (W8# x2)
        Vec.unsafeWrite mba (i+2) (W8# x3)
        return (i + 3)

    encode4 = do
        let x1  = or# (uncheckedShiftRL# x 18#) 0xf0##
            x2  = toContinuation (uncheckedShiftRL# x 12#)
            x3  = toContinuation (uncheckedShiftRL# x 6#)
            x4  = toContinuation x
        Vec.unsafeWrite mba i     (W8# x1)
        Vec.unsafeWrite mba (i+1) (W8# x2)
        Vec.unsafeWrite mba (i+2) (W8# x3)
        Vec.unsafeWrite mba (i+3) (W8# x4)
        return (i + 4)

    toContinuation :: Word# -> Word#
    toContinuation w = or# (and# w 0x3f##) 0x80##
{-# INLINE write #-}

freeze :: PrimMonad prim => MutableString (PrimState prim) -> prim String
freeze (MutableString mba) = String `fmap` C.unsafeFreeze mba
{-# INLINE freeze #-}

unsafeFreezeShrink :: PrimMonad prim => MutableString (PrimState prim) -> Size Word8 -> prim String
unsafeFreezeShrink (MutableString mba) s = String <$> Vec.unsafeFreezeShrink mba s
{-# INLINE unsafeFreezeShrink #-}

------------------------------------------------------------------------
-- real functions

-- | Convert a String to a list of characters
--
-- The list is lazily created as evaluation needed
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

-- | Create a new String from a list of characters
--
-- The list is strictly and fully evaluated before
-- creating the new String, as the size need to be
-- computed before filling.
sFromList :: [Char] -> String
sFromList l = runST (new bytes >>= startCopy)
  where
    -- count how many bytes
    !bytes = List.sum $ fmap (charToBytes . fromEnum) l

    startCopy :: MutableString (PrimState (ST st)) -> ST st String
    startCopy ms = loop azero l
      where
        loop _   []     = freeze ms
        loop idx (c:xs) = write ms idx c >>= \idx' -> loop idx' xs
{-# INLINE [0] sFromList #-}

-- | Check if a String is null
null :: String -> Bool
null (String ba) = C.length ba == 0

-- | Create a string composed of a number @n of Chars (Unicode code points).
--
-- if the input @s contains less characters than required, then the input string is returned.
take :: Int -> String -> String
take n s@(String ba)
    | n <= 0           = mempty
    | n >= C.length ba = s
    | otherwise        = let (Offset o) = indexN (Offset n) s in String $ Vec.take o ba

-- | Create a string with the remaining Chars after dropping @n Chars from the beginning
drop :: Int -> String -> String
drop n s@(String ba)
    | n <= 0           = s
    | n >= C.length ba = mempty
    | otherwise        = let (Offset o) = indexN (Offset n) s in String $ Vec.drop o ba

-- | Split a string at the Offset specified (in Char) returning both
-- the leading part and the remaining part.
splitAt :: Int -> String -> (String, String)
splitAt nI s@(String ba)
    | nI <= 0           = (mempty, s)
    | nI >= C.length ba = (s, mempty)
    | otherwise =
        let (Offset k) = indexN (Offset nI) s
            (v1,v2)    = C.splitAt k ba
         in (String v1, String v2)

-- | Return the offset (in bytes) of the N'th sequence in an UTF8 String
indexN :: Offset Char -> String -> Offset Word8
indexN !n (String ba) = Vec.unsafeDewrap goVec goAddr ba
  where
    goVec :: ByteArray# -> Offset Word8 -> Offset Word8
    goVec !ma !start = loop start (Offset 0)
      where
        !len = start `offsetPlusE` Vec.lengthSize ba
        loop :: Offset Word8 -> Offset Char -> Offset Word8
        loop !idx !i
            | idx >= len || i >= n = sizeAsOffset (idx - start)
            | otherwise              = loop (idx `offsetPlusE` d) (i + Offset 1)
          where d = skipNextHeaderValue (primBaIndex ma idx)
    {-# INLINE goVec #-}

    goAddr :: Ptr Word8 -> Offset Word8 -> ST s (Offset Word8)
    goAddr !(Ptr ptr) !start = return $ loop start (Offset 0)
      where
        !len = start `offsetPlusE` Vec.lengthSize ba
        loop :: Offset Word8 -> Offset Char -> Offset Word8
        loop !idx !i
            | idx >= len || i >= n = sizeAsOffset (idx - start)
            | otherwise            = loop (idx `offsetPlusE` d) (i + Offset 1)
          where d = skipNextHeaderValue (primAddrIndex ptr idx)
    {-# INLINE goAddr #-}
{-# INLINE indexN #-}

-- rev{Take,Drop,SplitAt} TODO optimise:
-- we can process the string from the end using a skipPrev instead of getting the length

-- | Similar to 'take' but from the end
revTake :: Int -> String -> String
revTake nbElems v = drop (length v - nbElems) v

-- | Similar to 'drop' but from the end
revDrop :: Int -> String -> String
revDrop nbElems v = take (length v - nbElems) v

-- | Similar to 'splitAt' but from the end
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
    | sz == Size 0 = [mempty]
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

-- | Internal call to make a substring given offset in bytes.
--
-- This is unsafe considering that one can create a substring
-- starting and/or ending on the middle of a UTF8 sequence.
sub :: String -> Offset8 -> Offset8 -> String
sub (String ba) start end = String $ Vec.sub ba start end

-- | Internal call to split at a given index in offset of bytes.
--
-- This is unsafe considering that one can split in the middle of a
-- UTF8 sequence, so use with care.
splitIndex :: Offset8 -> String -> (String, String)
splitIndex (Offset idx) (String ba) = (String v1, String v2)
  where (v1,v2) = C.splitAt idx ba

-- | Break a string into 2 strings at the location where the predicate return True
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

#if MIN_VERSION_base(4,9,0)
{-# RULES "break (== 'c')" [3] forall c . break (eqChar c) = breakElem c #-}
#else
{-# RULES "break (== 'c')" [3] forall c . break (== c) = breakElem c #-}
#endif

-- | Break a string into 2 strings at the first occurence of the character
breakElem :: Char -> String -> (String, String)
breakElem !el s@(String ba) =
    case asUTF8Char el of
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

-- | Apply a @predicate@ to the string to return the longest prefix that satisfy the predicate and
-- the remaining
span :: (Char -> Bool) -> String -> (String, String)
span predicate s = break (not . predicate) s

-- | Return whereas the string contains a specific character or not
elem :: Char -> String -> Bool
elem !el s@(String ba) =
    case asUTF8Char el of
        UTF8_1 w -> Vec.elem w ba
        _        -> runST $ Vec.unsafeIndexer ba go
  where
    sz = size s
    end = azero `offsetPlusE` sz

    go :: (Offset Word8 -> Word8) -> ST st Bool
    go getIdx = loop (Offset 0)
      where
        !nextI = nextWithIndexer getIdx
        loop !idx
            | idx == end = return False
            | otherwise  = do
                let (c, idx') = nextI idx
                case el == c of
                    True  -> return True
                    False -> loop idx'

-- | Intersperse the character @sep@ between each character in the string
--
-- > intersperse ' ' "Hello Foundation"
-- "H e l l o   F o u n d a t i o n"
intersperse :: Char -> String -> String
intersperse sep src
    | srcLen <= 1 = src
    | otherwise   = runST $ unsafeCopyFrom src dstBytes (go sep)
  where
    !srcBytes = size src
    !srcLen   = lengthSize src
    dstBytes = (srcBytes :: Size8)
             + ((srcLen - 1) `scale` charToBytes (fromEnum sep))

    lastSrcI :: Offset Char
    lastSrcI = 0 `offsetPlusE` (srcLen - 1)

    go :: Char -> String -> Offset Char -> Offset8 -> MutableString s -> Offset8 -> ST s (Offset8, Offset8)
    go sep' src' srcI srcIdx dst dstIdx
        | srcI == lastSrcI = do
            nextDstIdx <- write dst dstIdx c
            return (nextSrcIdx, nextDstIdx)
        | otherwise        = do
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

-- | size in bytes.
--
-- this size is available in o(1)
size :: String -> Size8
size (String ba) = Size $ C.length ba

-- | Length of a String using Size
--
-- this size is available in o(n)
lengthSize :: String -> Size Char
lengthSize (String ba)
    | C.null ba = Size 0
    | otherwise = Vec.unsafeDewrap goVec goAddr ba
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

-- | Length of a string in number of characters
length :: String -> Int
length s = let (Size sz) = lengthSize s in sz

-- | Replicate a character @c@ @n@ times to create a string of length @n@
replicate :: Word -> Char -> String
replicate n c = runST (new nbBytes >>= fill)
  where
    --end       = azero `offsetPlusE` nbBytes
    nbBytes   = scale n sz
    sz = charToBytes (fromEnum c)
    fill :: PrimMonad prim => MutableString (PrimState prim) -> prim String
    fill ms = loop (Offset 0)
      where
        loop idx
            | idx .==# nbBytes = freeze ms
            | otherwise        = write ms idx c >>= loop

-- | Copy the String
--
-- The slice of memory is copied to a new slice, making the new string
-- independent from the original string..
copy :: String -> String
copy (String s) = String (Vec.copy s)

-- | Create a single element String
singleton :: Char -> String
singleton c = runST $ do
    ms <- new nbBytes
    _  <- write ms (Offset 0) c
    freeze ms
  where
    !nbBytes = charToBytes (fromEnum c)

-- | Allocate a MutableString of a specific size in bytes.
new :: PrimMonad prim
    => Size8 -- ^ in number of bytes, not of elements.
    -> prim (MutableString (PrimState prim))
new n = MutableString `fmap` MVec.new n

-- | Unsafely create a string of up to @sz@ bytes.
--
-- The callback @f@ needs to return the number of bytes filled in the underlaying
-- bytes buffer. No check is made on the callback return values, and if it's not
-- contained without the bounds, bad things will happen.
create :: PrimMonad prim => Int -> (MutableString (PrimState prim) -> prim Int) -> prim String
create sz f = do
    ms     <- new (Size sz)
    filled <- f ms
    if filled == sz
        then freeze ms
        else take filled `fmap` freeze ms

charToBytes :: Int -> Size8
charToBytes c
    | c < 0x80     = Size 1
    | c < 0x800    = Size 2
    | c < 0x10000  = Size 3
    | c < 0x110000 = Size 4
    | otherwise    = error ("invalid code point: " `mappend` show c)

-- | Monomorphically map the character in a string and return the transformed one
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

-- | Append a Char to the end of the String and return this new String
snoc :: String -> Char -> String
snoc s@(String ba) c
    | len == Size 0 = singleton c
    | otherwise     = runST $ do
        ms@(MutableString mba) <- new (len + nbBytes)
        Vec.unsafeCopyAtRO mba (Offset 0) ba (Offset 0) len
        _ <- write ms (azero `offsetPlusE` len) c
        freeze ms
  where
    !len     = size s
    !nbBytes = charToBytes (fromEnum c)

-- | Prepend a Char to the beginning of the String and return this new String
cons :: Char -> String -> String
cons c s@(String ba)
  | len == Size 0 = singleton c
  | otherwise     = runST $ do
      ms@(MutableString mba) <- new (len + nbBytes)
      idx <- write ms (Offset 0) c
      Vec.unsafeCopyAtRO mba idx ba (Offset 0) len
      freeze ms
  where
    !len     = size s
    !nbBytes = charToBytes (fromEnum c)

-- | Extract the String stripped of the last character and the last character if not empty
--
-- If empty, Nothing is returned
unsnoc :: String -> Maybe (String, Char)
unsnoc s
    | null s    = Nothing
    | otherwise = case index s (sizeLastOffset $ lengthSize s) of
        Nothing -> Nothing
        Just c  -> Just (revDrop 1 s, c)

-- | Extract the First character of a string, and the String stripped of the first character.
--
-- If empty, Nothing is returned
uncons :: String -> Maybe (Char, String)
uncons s
    | null s    = Nothing
    | otherwise = case index s 0 of
          Nothing -> Nothing
          Just c  -> Just (c, drop 1 s)

-- | Look for a predicate in the String and return the matched character, if any.
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

-- | Sort the character in a String using a specific sort function
sortBy :: (Char -> Char -> Ordering) -> String -> String
sortBy sortF s = fromList $ Data.List.sortBy sortF $ toList s -- FIXME for tests

-- | Filter characters of a string using the predicate
filter :: (Char -> Bool) -> String -> String
filter p s = fromList $ Data.List.filter p $ toList s

-- | Reverse a string
reverse :: String -> String
reverse s@(String ba) = runST $ do
    ms <- new len
    loop ms (Offset 0) (Offset 0 `offsetPlusE` len)
  where
    !len = size s
    -- write those bytes
    loop :: PrimMonad prim => MutableString (PrimState prim) -> Offset8 -> Offset8 -> prim String
    loop ms@(MutableString mba) si didx
        | didx == Offset 0 = freeze ms
        | otherwise = do
            let !h = Vec.unsafeIndex ba si
                !nb = Size (getNbBytes h + 1)
                d  = didx `offsetMinusE` nb
            case nb of
                Size 1 -> Vec.unsafeWrite mba d h
                Size 2 -> do
                    Vec.unsafeWrite mba d       h
                    Vec.unsafeWrite mba (d + 1) (Vec.unsafeIndex ba (si + 1))
                Size 3 -> do
                    Vec.unsafeWrite mba d       h
                    Vec.unsafeWrite mba (d + 1) (Vec.unsafeIndex ba (si + 1))
                    Vec.unsafeWrite mba (d + 2) (Vec.unsafeIndex ba (si + 2))
                Size 4 -> do
                    Vec.unsafeWrite mba d       h
                    Vec.unsafeWrite mba (d + 1) (Vec.unsafeIndex  ba (si + 1))
                    Vec.unsafeWrite mba (d + 2) (Vec.unsafeIndex ba (si + 2))
                    Vec.unsafeWrite mba (d + 3) (Vec.unsafeIndex ba (si + 3))
                _  -> return () -- impossible
            loop ms (si `offsetPlusE` nb) d

-- | Return the nth character in a String
--
-- Compared to an array, the string need to be scanned from the beginning
-- since the UTF8 encoding is variable.
index :: String -> Offset Char -> Maybe Char
index s n
    | ofs >= end = Nothing
    | otherwise  =
        let (# c, _ #) = next s ofs
         in Just c
  where
    !nbBytes = size s
    end = 0 `offsetPlusE` nbBytes
    ofs = indexN n s

-- | Return the index in unit of Char of the first occurence of the predicate returning True
--
-- If not found, Nothing is returned
findIndex :: (Char -> Bool) -> String -> Maybe (Offset Char)
findIndex predicate s = loop 0 0
  where
    !sz = size s
    loop ofs idx
        | idx .==# sz = Nothing
        | otherwise   =
            let (# c, idx' #) = next s idx
             in case predicate c of
                    True  -> Just ofs
                    False -> loop (ofs+1) idx'

-- | Various String Encoding that can be use to convert to and from bytes
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

-- | Decode a stream of binary chunks containing UTF8 encoding in a list of valid String
--
-- Chunk not necessarily contains a valid string, as
-- a UTF8 sequence could be split over 2 chunks.
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

-- | Split lines in a string using newline as separation
lines :: String -> [String]
lines = fmap fromList . Prelude.lines . toList

-- | Split words in a string using spaces as separation
--
-- > words "Hello Foundation"
-- [ "Hello", "Foundation" ]
words :: String -> [String]
words = fmap fromList . Prelude.words . toList

-- | Append a character to a String builder
builderAppend :: PrimMonad state => Char -> Builder String MutableString Word8 state ()
builderAppend c = Builder $ State $ \(i, st) ->
    if offsetAsSize i + nbBytes >= chunkSize st
        then do
            cur      <- unsafeFreezeShrink (curChunk st) (offsetAsSize i)
            newChunk <- new (chunkSize st)
            writeUTF8Char newChunk (Offset 0) utf8Char
            return ((), (sizeAsOffset nbBytes, st { prevChunks     = cur : prevChunks st
                                                  , prevChunksSize = offsetAsSize i + prevChunksSize st
                                                  , curChunk       = newChunk
                                                  }))
        else do
            writeUTF8Char (curChunk st) i utf8Char
            return ((), (i + sizeAsOffset nbBytes, st))
  where
    utf8Char = asUTF8Char c
    nbBytes  = numBytes utf8Char

-- | Create a new String builder using chunks of @sizeChunksI@
builderBuild :: PrimMonad m => Int -> Builder String MutableString Word8 m () -> m String
builderBuild sizeChunksI sb
    | sizeChunksI <= 3 = builderBuild 64 sb
    | otherwise        = do
        first         <- new sizeChunks
        ((), (i, st)) <- runState (runBuilder sb) (Offset 0, BuildingState [] (Size 0) first sizeChunks)
        cur           <- unsafeFreezeShrink (curChunk st) (offsetAsSize i)
        -- Build final array
        let totalSize = prevChunksSize st + offsetAsSize i
        final <- Vec.new totalSize >>= fillFromEnd totalSize (cur : prevChunks st) >>= Vec.unsafeFreeze
        return $ String final
  where
    sizeChunks = Size sizeChunksI

    fillFromEnd _   []            mba = return mba
    fillFromEnd !end (String x:xs) mba = do
        let sz = Vec.lengthSize x
        Vec.unsafeCopyAtRO mba (sizeAsOffset (end - sz)) x (Offset 0) sz
        fillFromEnd (end - sz) xs mba

stringDewrap :: (ByteArray# -> Offset Word8 -> a)
             -> (Ptr Word8 -> Offset Word8 -> ST s a)
             -> String
             -> a
stringDewrap withBa withPtr (String ba) = C.unsafeDewrap withBa withPtr ba
{-# INLINE stringDewrap #-}

-- | Read an Integer from a String
--
-- Consume an optional minus sign and many digits until end of string.
readIntegral :: (HasNegation i, IntegralUpsize Word8 i, Additive i, Multiplicative i, IsIntegral i) => String -> Maybe i
readIntegral str
    | sz == 0   = Nothing
    | otherwise = stringDewrap withBa withPtr str
  where
    !sz = size str
    withBa ba ofs =
        let negativeSign = expectAsciiBA ba ofs 0x2d
            startOfs     = if negativeSign then succ ofs else ofs
         in case decimalDigitsBA 0 ba endOfs startOfs of
                (# acc, True, endOfs' #) | endOfs' > startOfs -> Just $! if negativeSign then negate acc else acc
                _                                             -> Nothing
      where !endOfs = ofs `offsetPlusE` sz
    withPtr ptr ofs = return $
        let negativeSign = expectAsciiPtr ptr ofs 0x2d
            startOfs     = if negativeSign then succ ofs else ofs
         in case decimalDigitsPtr 0 ptr endOfs startOfs of
                (# acc, True, endOfs' #) | endOfs' > startOfs -> Just $! if negativeSign then negate acc else acc
                _                                             -> Nothing
      where !endOfs = ofs `offsetPlusE` sz
{-# SPECIALISE readIntegral :: String -> Maybe Integer #-}
{-# SPECIALISE readIntegral :: String -> Maybe Int #-}

readInteger :: String -> Maybe Integer
readInteger = readIntegral

-- | Read a Natural from a String
--
-- Consume many digits until end of string.
readNatural :: String -> Maybe Natural
readNatural str
    | sz == 0  = Nothing
    | otherwise =
        case decimalDigits 0 str 0 of
            (# acc, True, endOfs #) | endOfs > 0 -> Just $ acc
            _                                    -> Nothing
  where
    !sz = size str

-- | Try to read a Double
readDouble :: String -> Maybe Double
readDouble s =
    readFloatingExact s $ \isNegative integral floatingDigits mExponant ->
        Just $ applySign isNegative $ case (floatingDigits, mExponant) of
            (0, Nothing)              ->                         naturalToDouble integral
            (0, Just exponant)        -> withExponant exponant $ naturalToDouble integral
            (floating, Nothing)       ->                         applyFloating floating $ naturalToDouble integral
            (floating, Just exponant) -> withExponant exponant $ applyFloating floating $ naturalToDouble integral
  where
    applySign True = negate
    applySign False = id
    withExponant e v = v * doubleExponant 10 e
    applyFloating digits n = n / (10 Prelude.^ digits)

type ReadFloatingCallback a = Bool      -- sign
                           -> Natural   -- integral part
                           -> Word      -- number of digits in floating section
                           -> Maybe Int -- optional integer representing exponent in base 10
                           -> Maybe a

-- | Read an Floating like number of the form:
--
--   [ '-' ] <numbers> [ '.' <numbers> ] [ ( 'e' | 'E' ) [ '-' ] <number> ]
--
-- Call a function with:
--
-- * A boolean representing if the number is negative
-- * The digits part represented as a single natural number (123.456 is represented as 123456)
-- * The number of digits in the fractional part (e.g. 123.456 => 3)
-- * The exponant if any
--
-- The code is structured as a simple state machine that:
--
-- * Optionally Consume a '-' sign
-- * Consume number for the integral part
-- * Optionally
--   * Consume '.'
--   * Consume remaining digits if not already end of string
-- * Optionally Consume a 'e' or 'E' follow by an optional '-' and a number
--
readFloatingExact :: String -> ReadFloatingCallback a -> Maybe a
readFloatingExact str f
    | sz == 0   = Nothing
    | otherwise = stringDewrap withBa withPtr str
  where
    !sz = size str

    withBa ba stringStart =
        let !isNegative = expectAsciiBA ba stringStart 0x2d
         in consumeIntegral isNegative (if isNegative then stringStart+1 else stringStart)
      where
        eofs = stringStart `offsetPlusE` sz
        consumeIntegral !isNegative startOfs =
            case decimalDigitsBA 0 ba eofs startOfs of
                (# acc, True , endOfs #) | endOfs > startOfs -> f isNegative acc 0 Nothing -- end of stream and no '.'
                (# acc, False, endOfs #) | endOfs > startOfs ->
                    if expectAsciiBA ba endOfs 0x2e
                        then consumeFloat isNegative acc (endOfs + 1)
                        else consumeExponant isNegative acc 0 endOfs
                _                                            -> Nothing

        consumeFloat isNegative integral startOfs =
            case decimalDigitsBA integral ba eofs startOfs of
                (# acc, True, endOfs #) | endOfs > startOfs -> let (Size !diff) = endOfs - startOfs
                                                                in f isNegative acc (integralCast diff) Nothing
                (# acc, False, endOfs #) | endOfs > startOfs -> let (Size !diff) = endOfs - startOfs
                                                                in consumeExponant isNegative acc (integralCast diff) endOfs
                _                                           -> Nothing

        consumeExponant !isNegative !integral !floatingDigits !startOfs
            | startOfs == eofs = f isNegative integral floatingDigits Nothing
            | otherwise        =
                -- consume 'E' or 'e'
                case nextAsciiBA ba startOfs of
                    (# 0x45, True #) -> consumeExponantSign (startOfs+1)
                    (# 0x65, True #) -> consumeExponantSign (startOfs+1)
                    (# _   , _    #) -> Nothing
          where
            consumeExponantSign ofs
                | ofs == eofs = Nothing
                | otherwise   = let exponantNegative = expectAsciiBA ba ofs 0x2d
                                 in consumeExponantNumber exponantNegative (if exponantNegative then ofs + 1 else ofs)

            consumeExponantNumber exponantNegative ofs =
                case decimalDigitsBA 0 ba eofs ofs of
                    (# acc, True, endOfs #) | endOfs > ofs -> f isNegative integral floatingDigits (Just $! if exponantNegative then negate acc else acc)
                    _                                      -> Nothing
    withPtr ptr stringStart = return $
        let !isNegative = expectAsciiPtr ptr stringStart 0x2d
         in consumeIntegral isNegative (if isNegative then stringStart+1 else stringStart)
      where
        eofs = stringStart `offsetPlusE` sz
        consumeIntegral !isNegative startOfs =
            case decimalDigitsPtr 0 ptr eofs startOfs of
                (# acc, True , endOfs #) | endOfs > startOfs -> f isNegative acc 0 Nothing -- end of stream and no '.'
                (# acc, False, endOfs #) | endOfs > startOfs ->
                    if expectAsciiPtr ptr endOfs 0x2e
                        then consumeFloat isNegative acc (endOfs + 1)
                        else consumeExponant isNegative acc 0 endOfs
                _                                            -> Nothing

        consumeFloat isNegative integral startOfs =
            case decimalDigitsPtr integral ptr eofs startOfs of
                (# acc, True, endOfs #) | endOfs > startOfs -> let (Size !diff) = endOfs - startOfs
                                                                in f isNegative acc (integralCast diff) Nothing
                (# acc, False, endOfs #) | endOfs > startOfs -> let (Size !diff) = endOfs - startOfs
                                                                in consumeExponant isNegative acc (integralCast diff) endOfs
                _                                           -> Nothing

        consumeExponant !isNegative !integral !floatingDigits !startOfs
            | startOfs == eofs = f isNegative integral floatingDigits Nothing
            | otherwise        =
                -- consume 'E' or 'e'
                case nextAsciiPtr ptr startOfs of
                    (# 0x45, True #) -> consumeExponantSign (startOfs+1)
                    (# 0x65, True #) -> consumeExponantSign (startOfs+1)
                    (# _   , _    #) -> Nothing
          where
            consumeExponantSign ofs
                | ofs == eofs = Nothing
                | otherwise   = let exponantNegative = expectAsciiPtr ptr ofs 0x2d
                                 in consumeExponantNumber exponantNegative (if exponantNegative then ofs + 1 else ofs)

            consumeExponantNumber exponantNegative ofs =
                case decimalDigitsPtr 0 ptr eofs ofs of
                    (# acc, True, endOfs #) | endOfs > ofs -> f isNegative integral floatingDigits (Just $! if exponantNegative then negate acc else acc)
                    _                                      -> Nothing

-- | Take decimal digits and accumulate it in `acc`
--
-- The loop starts at the offset specified and finish either when:
--
-- * It reach the end of the string
-- * It reach a non-ASCII character
-- * It reach an ASCII character that is not a digit (0 to 9)
--
-- Otherwise each iterations:
--
-- * Transform the ASCII digits into a number
-- * scale the accumulator by 10
-- * Add the number (between 0 and 9) to the accumulator
--
-- It then returns:
--
-- * The new accumulated value
-- * Whether it stop by end of string or not
-- * The end offset when the loop stopped
--
-- If end offset == start offset then no digits have been consumed by
-- this function
decimalDigits :: (IntegralUpsize Word8 acc, Additive acc)
              => acc
              -> String
              -> Offset Word8
              -> (# acc, Bool, Offset Word8 #)
decimalDigits startAcc str startOfs = loop startAcc startOfs
  where
    !sz = size str
    loop acc ofs
        | ofs .==# sz = (# acc, True, ofs #)
        | otherwise   =
            case nextAscii str ofs of
                (# d, True #) | isDigit d -> loop (scale (10::Word) acc + fromDigit d) (ofs+1)
                (# _, _ #)                -> (# acc, False, ofs #)
    ascii0 = 0x30 -- use pattern synonym when we support >= 8.0
    ascii9 = 0x39
    isDigit c = c >= ascii0 && c <= ascii9
    fromDigit c = integralUpsize (c - ascii0)

-- | same as decimalDigitsBA for a bytearray#
decimalDigitsBA :: (IntegralUpsize Word8 acc, Additive acc, Multiplicative acc, Integral acc)
                => acc
                -> ByteArray#
                -> Offset Word8 -- end offset
                -> Offset Word8 -- start offset
                -> (# acc, Bool, Offset Word8 #)
decimalDigitsBA startAcc ba !endOfs !startOfs = loop startAcc startOfs
  where
    loop !acc !ofs
        | ofs == endOfs = (# acc, True, ofs #)
        | otherwise     =
            case nextAsciiDigitBA ba ofs of
                (# d, True #) -> loop (10 * acc + integralUpsize d) (succ ofs)
                (# _, _ #)    -> (# acc, False, ofs #)
{-# SPECIALIZE decimalDigitsBA :: Integer -> ByteArray# -> Offset Word8 -> Offset Word8 -> (# Integer, Bool, Offset Word8 #) #-}
{-# SPECIALIZE decimalDigitsBA :: Natural -> ByteArray# -> Offset Word8 -> Offset Word8 -> (# Natural, Bool, Offset Word8 #) #-}
{-# SPECIALIZE decimalDigitsBA :: Int -> ByteArray# -> Offset Word8 -> Offset Word8 -> (# Int, Bool, Offset Word8 #) #-}
{-# SPECIALIZE decimalDigitsBA :: Word -> ByteArray# -> Offset Word8 -> Offset Word8 -> (# Word, Bool, Offset Word8 #) #-}

-- | same as decimalDigitsBA specialized for ptr #
decimalDigitsPtr :: (IntegralUpsize Word8 acc, Additive acc, Multiplicative acc, Integral acc)
                 => acc
                 -> Ptr Word8
                 -> Offset Word8 -- end offset
                 -> Offset Word8 -- start offset
                 -> (# acc, Bool, Offset Word8 #)
decimalDigitsPtr startAcc ptr !endOfs !startOfs = loop startAcc startOfs
  where
    loop !acc !ofs
        | ofs == endOfs = (# acc, True, ofs #)
        | otherwise     =
            case nextAsciiDigitPtr ptr ofs of
                (# d, True #) -> loop (10 * acc + integralUpsize d) (succ ofs)
                (# _, _ #)    -> (# acc, False, ofs #)
{-# SPECIALIZE decimalDigitsPtr :: Integer -> Ptr Word8 -> Offset Word8 -> Offset Word8 -> (# Integer, Bool, Offset Word8 #) #-}
{-# SPECIALIZE decimalDigitsPtr :: Natural -> Ptr Word8 -> Offset Word8 -> Offset Word8 -> (# Natural, Bool, Offset Word8 #) #-}
{-# SPECIALIZE decimalDigitsPtr :: Int -> Ptr Word8 -> Offset Word8 -> Offset Word8 -> (# Int, Bool, Offset Word8 #) #-}
{-# SPECIALIZE decimalDigitsPtr :: Word -> Ptr Word8 -> Offset Word8 -> Offset Word8 -> (# Word, Bool, Offset Word8 #) #-}
