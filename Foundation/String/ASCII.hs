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
    ( AsciiString
    --, Buffer
    , create
    , replicate
    -- * Binary conversion
    , fromBytesUnsafe
    , toBytes
    , copy

    -- * Legacy utility
    , lines
    , words
    ) where

import           Foundation.Array.Unboxed           (UArray)
import qualified Foundation.Array.Unboxed           as Vec
import qualified Foundation.Array.Unboxed.Mutable   as MVec
import qualified Foundation.Collection              as C
import           Foundation.Internal.Base
import           Foundation.Internal.Types
import           Foundation.Number
import           Foundation.Primitive.Monad
import           Foundation.Foreign

import GHC.Int
import GHC.Types
import GHC.Prim

 -- temporary
import qualified Data.List
import qualified Prelude
import Foundation.Class.Bifunctor

ccharToChar :: CChar -> Char
ccharToChar (CChar (I8# i)) = C# (chr# i)
charToCChar :: Char -> CChar
charToCChar (C# i) = CChar (I8# (ord# i))

-- | Opaque packed array of characters in the ASCII encoding
newtype AsciiString = AsciiString { toBytes :: UArray CChar }
    deriving (Typeable, Monoid, Eq, Ord)

newtype MutableAsciiString st = MutableAsciiString (MVec.MUArray CChar st)
    deriving (Typeable)

instance Show AsciiString where
    show = fmap ccharToChar . toList
instance IsString AsciiString where
    fromString = fromList . fmap charToCChar
instance IsList AsciiString where
    type Item AsciiString = CChar
    fromList = sFromList
    toList = sToList

type instance C.Element AsciiString = CChar

instance C.InnerFunctor AsciiString where
    imap = ccharMap
instance C.Collection AsciiString where
    null = null
    length = length
    minimum = Data.List.minimum . toList . C.getNonEmpty -- TODO faster implementation
    maximum = Data.List.maximum . toList . C.getNonEmpty -- TODO faster implementation
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

next :: AsciiString -> Offset CChar -> (# CChar, Offset CChar #)
next (AsciiString ba) (Offset n) = (# h, Offset (n + 1) #)
  where
    !h = Vec.unsafeIndex ba n

freeze :: PrimMonad prim => MutableAsciiString (PrimState prim) -> prim AsciiString
freeze (MutableAsciiString mba) = AsciiString `fmap` C.unsafeFreeze mba
{-# INLINE freeze #-}

------------------------------------------------------------------------
-- real functions

sToList :: AsciiString -> [CChar]
sToList s = loop azero
  where
    nbBytes :: Size CChar
    !nbBytes = size s
    !end = azero `offsetPlusE` nbBytes
    loop idx
        | idx == end = []
        | otherwise  =
            let (# c , idx' #) = next s idx in c : loop idx'

sFromList :: [CChar] -> AsciiString
sFromList = AsciiString . fromList
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
splitOn :: (CChar -> Bool) -> AsciiString -> [AsciiString]
splitOn predicate = fmap AsciiString . Vec.splitOn predicate . toBytes

break :: (CChar -> Bool) -> AsciiString -> (AsciiString, AsciiString)
break predicate = bimap AsciiString AsciiString . Vec.break predicate . toBytes
{-# INLINE[0] break #-}

{-# RULES "break (== 'c')" [3] forall c . break (== c) = breakElem c #-}

breakElem :: CChar -> AsciiString -> (AsciiString, AsciiString)
breakElem !el (AsciiString ba) =
    let (# v1,v2 #) = Vec.splitElem el ba in (AsciiString v1, AsciiString v2)
{-# INLINE breakElem #-}

intersperse :: CChar -> AsciiString -> AsciiString
intersperse sep = AsciiString . Vec.intersperse sep . toBytes

span :: (CChar -> Bool) -> AsciiString -> (AsciiString, AsciiString)
span predicate = break (not . predicate)

-- | size in bytes
size :: AsciiString -> Size CChar
size = Size . C.length . toBytes

length :: AsciiString -> Int
length s = let (Size l) = size s in l

replicate :: Int -> CChar -> AsciiString
replicate n c = AsciiString $ Vec.create n (const c)

-- | Copy the AsciiString
copy :: AsciiString -> AsciiString
copy (AsciiString s) = AsciiString (Vec.copy s)

-- | Allocate a MutableAsciiString of a specific size in bytes.
new :: PrimMonad prim
    => Size CChar -- ^ in number of bytes, not of elements.
    -> prim (MutableAsciiString (PrimState prim))
new n = MutableAsciiString `fmap` MVec.new n

create :: PrimMonad prim => Int -> (MutableAsciiString (PrimState prim) -> prim Int) -> prim AsciiString
create sz f = do
    ms     <- new (Size sz)
    filled <- f ms
    if filled == sz
        then freeze ms
        else C.take filled `fmap` freeze ms

ccharMap :: (CChar -> CChar) -> AsciiString -> AsciiString
ccharMap f = AsciiString . Vec.map f . toBytes

snoc :: AsciiString -> CChar -> AsciiString
snoc (AsciiString ba) = AsciiString . Vec.snoc ba

cons :: CChar -> AsciiString -> AsciiString
cons c = AsciiString . Vec.cons c . toBytes

unsnoc :: AsciiString -> Maybe (AsciiString, CChar)
unsnoc str = first AsciiString <$> Vec.unsnoc (toBytes str)

uncons :: AsciiString -> Maybe (CChar, AsciiString)
uncons str = second AsciiString <$> Vec.uncons (toBytes str)

find :: (CChar -> Bool) -> AsciiString -> Maybe CChar
find predicate = Vec.find predicate . toBytes

sortBy :: (CChar -> CChar -> Ordering) -> AsciiString -> AsciiString
sortBy sortF = AsciiString . Vec.sortBy sortF . toBytes

filter :: (CChar -> Bool) -> AsciiString -> AsciiString
filter p s = fromList $ Data.List.filter p $ toList s

reverse :: AsciiString -> AsciiString
reverse (AsciiString ba) = AsciiString $ Vec.reverse ba

-- | Convert a Byte Array representing UTF8 data directly to a string without checking for UTF8 validity
--
-- If the input contains invalid sequences, it will trigger runtime async errors when processing data.
--
-- In doubt, use 'fromBytes'
fromBytesUnsafe :: UArray CChar -> AsciiString
fromBytesUnsafe = AsciiString

lines :: AsciiString -> [AsciiString]
lines = fmap fromString . Prelude.lines . show

words :: AsciiString -> [AsciiString]
words = fmap fromString . Prelude.words . show
