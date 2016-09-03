-- |
-- Module      : Foundation.String.ASCII
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
-- Stability   : experimental
-- Portability : portable
--
-- A AsciiString type backed by a `ASCII` encoded byte array and all the necessary
-- functions to manipulate the string.
--
-- The recommended type is `AsciiString` from `Foundation.AsciiString.UTF8`
--
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE FlexibleContexts           #-}
module Foundation.String.ASCII
    ( AsciiString(..)
    --, Buffer
    , create
    , replicate
    -- * Binary conversion
    , fromBytesUnsafe
    , toBytes
    , copy

    , validate
    , ASCII7_Invalid(..)
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
import           Foundation.Internal.Types
import           Foundation.Number
import           Foundation.Primitive.Monad
import           GHC.Prim
import           GHC.Types
import           GHC.Word

 -- temporary
import qualified Data.List
import qualified Prelude
import Foundation.Class.Bifunctor

import           Foundation.String.ModifiedUTF8     (fromModified)
import           GHC.CString                  (unpackCString#,
                                               unpackCStringUtf8#)

import qualified Foundation.String.Encoding.ASCII7     as Encoder
import Foundation.String.Encoding.ASCII7 (ASCII7_Invalid)

-- | Opaque packed array of characters in the ASCII encoding
newtype AsciiString = AsciiString (UArray Word8)
    deriving (Typeable, Monoid, Eq, Ord)

toBytes :: AsciiString -> UArray Word8
toBytes (AsciiString bs) = bs

newtype MutableAsciiString st = MutableAsciiString (MutableByteArray st)
    deriving (Typeable)

instance Show AsciiString where
    show = show . sToList
instance IsString AsciiString where
    fromString = sFromList
instance IsList AsciiString where
    type Item AsciiString = Char
    fromList = sFromList
    toList = sToList

type instance C.Element AsciiString = Char

instance C.InnerFunctor AsciiString where
    imap = charMap
instance C.Collection AsciiString where
    null = null
    length = length
instance C.Sequential AsciiString where
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

instance C.Zippable AsciiString where
  -- TODO Use a string builder once available
  zipWith f a b = sFromList (C.zipWith f a b)

-- | Validate a bytearray for ASCIIness
--
-- On success Nothing is returned
-- On Failure the position along with the failure reason
validate :: AsciiString -> Maybe ASCII7_Invalid
validate = Encoder.validate . toBytes
{-# INLINE validate #-}

next :: AsciiString -> Offset Char -> (# Char, Offset Char #)
next (AsciiString ba) (Offset n) = (# toChar h, Offset (n + 1) #)
  where
    !h = Vec.unsafeIndex ba n

toChar :: Word8 -> Char
toChar (W8# w) = C# (chr# (word2Int# w))
{-# INLINE toChar #-}

toWord8 :: Char -> Word8
toWord8 (C# i) = W8# (int2Word# (ord# i))
{-# INLINE toWord8 #-}

freeze :: PrimMonad prim => MutableAsciiString (PrimState prim) -> prim AsciiString
freeze (MutableAsciiString mba) = AsciiString `fmap` C.unsafeFreeze mba
{-# INLINE freeze #-}

------------------------------------------------------------------------
-- real functions

sToList :: AsciiString -> [Char]
sToList s = loop azero
  where
    !nbBytes = size s
    !end = azero `offsetPlusE` nbBytes
    loop idx
        | idx == end = []
        | otherwise  =
            let (# c , idx' #) = next s idx in c : loop idx'


{-# RULES
"AsciiString sFromList" forall s .
  sFromList (unpackCString# s) = AsciiString $ fromModified s
  #-}
{-# RULES
"AsciiString sFromList" forall s .
  sFromList (unpackCStringUtf8# s) = AsciiString $ fromModified s
  #-}

sFromList :: [Char] -> AsciiString
sFromList = AsciiString . fromList . fmap toWord8
{-# INLINE [0] sFromList #-}

null :: AsciiString -> Bool
null  = Vec.null . toBytes
{-# INLINE null #-}

-- | Create a string composed of a number @n of Chars (Unicode code points).
--
-- if the input @s contains less characters than required, then
take :: Int -> AsciiString -> AsciiString
take n s = fst $ splitAt n s -- TODO specialize
{-# INLINE take #-}

-- | Create a string with the remaining Chars after dropping @n Chars from the beginning
drop :: Int -> AsciiString -> AsciiString
drop n = AsciiString . Vec.drop n . toBytes
{-# INLINE drop #-}

splitAt :: Int -> AsciiString -> (AsciiString, AsciiString)
splitAt n = bimap AsciiString AsciiString . Vec.splitAt n . toBytes
{-# INLINE splitAt #-}

-- rev{Take,Drop,SplitAt} TODO optimise:
-- we can process the string from the end using a skipPrev instead of getting the length

revTake :: Int -> AsciiString -> AsciiString
revTake nbElems v = drop (length v - nbElems) v

revDrop :: Int -> AsciiString -> AsciiString
revDrop nbElems v = take (length v - nbElems) v

revSplitAt :: Int -> AsciiString -> (AsciiString, AsciiString)
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
splitOn :: (Char -> Bool) -> AsciiString -> [AsciiString]
splitOn predicate = fmap AsciiString . Vec.splitOn f . toBytes
  where
    f :: Word8 -> Bool
    f = predicate . toChar

break :: (Char -> Bool) -> AsciiString -> (AsciiString, AsciiString)
break predicate = bimap AsciiString AsciiString . Vec.break (predicate . toChar) . toBytes
{-# INLINE[0] break #-}

{-# RULES "break (== 'c')" [3] forall c . break (== c) = breakElem c #-}

breakElem :: Char -> AsciiString -> (AsciiString, AsciiString)
breakElem !el (AsciiString ba) =
    let (# v1,v2 #) = Vec.splitElem (w8 el) ba in (AsciiString v1, AsciiString v2)
  where
    w8 (C# ch) = W8# (int2Word# (ord# ch))
{-# INLINE breakElem #-}

intersperse :: Char -> AsciiString -> AsciiString
intersperse sep = AsciiString . Vec.intersperse (toWord8 sep) . toBytes

span :: (Char -> Bool) -> AsciiString -> (AsciiString, AsciiString)
span predicate = break (not . predicate)

-- | size in bytes
size :: AsciiString -> Size Char
size = Size . C.length . toBytes

length :: AsciiString -> Int
length s = let (Size l) = size s in l

replicate :: Int -> Char -> AsciiString
replicate n c = AsciiString $ Vec.create n (const w)
  where
    !w = toWord8 c

-- | Copy the AsciiString
copy :: AsciiString -> AsciiString
copy (AsciiString s) = AsciiString (Vec.copy s)

-- | Allocate a MutableAsciiString of a specific size in bytes.
new :: PrimMonad prim
    => Size8 -- ^ in number of bytes, not of elements.
    -> prim (MutableAsciiString (PrimState prim))
new n = MutableAsciiString `fmap` MVec.new n

create :: PrimMonad prim => Int -> (MutableAsciiString (PrimState prim) -> prim Int) -> prim AsciiString
create sz f = do
    ms     <- new (Size sz)
    filled <- f ms
    if filled == sz
        then freeze ms
        else C.take filled `fmap` freeze ms

charMap :: (Char -> Char) -> AsciiString -> AsciiString
charMap f = AsciiString . Vec.map (toWord8 . f . toChar) . toBytes

snoc :: AsciiString -> Char -> AsciiString
snoc (AsciiString ba) = AsciiString . Vec.snoc ba . toWord8

cons :: Char -> AsciiString -> AsciiString
cons c = AsciiString . Vec.cons (toWord8 c) . toBytes

unsnoc :: AsciiString -> Maybe (AsciiString, Char)
unsnoc str = bimap AsciiString toChar <$> Vec.unsnoc (toBytes str)

uncons :: AsciiString -> Maybe (Char, AsciiString)
uncons str = bimap toChar AsciiString <$> Vec.uncons (toBytes str)

find :: (Char -> Bool) -> AsciiString -> Maybe Char
find predicate (AsciiString ba) = toChar <$> Vec.find (predicate . toChar) ba

sortBy :: (Char -> Char -> Ordering) -> AsciiString -> AsciiString
sortBy sortF = AsciiString . Vec.sortBy f . toBytes
  where
    f a b = sortF (toChar a) (toChar b)

filter :: (Char -> Bool) -> AsciiString -> AsciiString
filter p s = fromList $ Data.List.filter p $ toList s

reverse :: AsciiString -> AsciiString
reverse (AsciiString ba) = AsciiString $ Vec.reverse ba

-- | Convert a Byte Array representing UTF8 data directly to a string without checking for UTF8 validity
--
-- If the input contains invalid sequences, it will trigger runtime async errors when processing data.
--
-- In doubt, use 'fromBytes'
fromBytesUnsafe :: UArray Word8 -> AsciiString
fromBytesUnsafe = AsciiString

lines :: AsciiString -> [AsciiString]
lines = fmap fromList . Prelude.lines . toList

words :: AsciiString -> [AsciiString]
words = fmap fromList . Prelude.words . toList
