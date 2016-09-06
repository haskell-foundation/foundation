-- |
-- Module      : Foundation.Array.Chunked.Unboxed
-- License     : BSD-style
-- Maintainer  : Alfredo Di Napoli <alfredo.dinapoli@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Simple array-of-arrays abstraction
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
module Foundation.Array.Chunked.Unboxed
    ( ChunkedUArray
    ) where

import           Data.Typeable
import           Foundation.Array.Boxed (Array)
import qualified Foundation.Array.Boxed as A
import           Foundation.Array.Unboxed (UArray, PrimType)
import qualified Foundation.Collection as C
import           Foundation.Internal.Base
import           Foundation.Internal.Types
import           Foundation.Number
import           GHC.ST


data ChunkedUArray ty = ChunkedUArray (Array (UArray ty))
                      deriving (Show, Eq, Ord, Typeable)

instance Monoid (ChunkedUArray a) where
    mempty  = empty
    mappend = append
    mconcat = concat

type instance C.Element (ChunkedUArray ty) = ty

instance PrimType ty => IsList (ChunkedUArray ty) where
    type Item (ChunkedUArray ty) = ty
    fromList = vFromList
    toList = vToList

instance PrimType ty => C.Collection (ChunkedUArray ty) where
    null = null
    length = length

--instance C.Sequential (ChunkedUArray ty) where
--    take = take
--    drop = drop
--    splitAt = splitAt
--    revTake = revTake
--    revDrop = revDrop
--    revSplitAt = revSplitAt
--    splitOn = splitOn
--    break = break
--    intersperse = intersperse
--    span = span
--    reverse = reverse
--    filter = filter
--    unsnoc = unsnoc
--    uncons = uncons
--    snoc = snoc
--    cons = cons
--    find = find
--    sortBy = sortBy
--    singleton = fromList . (:[])

--instance C.IndexedCollection (ChunkedUArray ty) where
--    (!) l n
--        | n < 0 || n >= length l = Nothing
--        | otherwise              = Just $ index l n
--    findIndex predicate c = loop 0
--      where
--        !len = length c
--        loop i
--            | i == len  = Nothing
--            | otherwise =
--                if predicate (unsafeIndex c i) then Just i else Nothing

empty :: ChunkedUArray ty
empty = ChunkedUArray (A.empty)

append :: ChunkedUArray ty -> ChunkedUArray ty -> ChunkedUArray ty
append (ChunkedUArray a1) (ChunkedUArray a2) = ChunkedUArray $ runST $ do
  let a1Size@(Size a1len) = Size $ C.length a1
  let a2Size              = Size $ C.length a2
  a <- A.new (a1Size + a2Size)
  A.thaw a1 >>= \a1' -> A.copyAt a (Offset 0) a1' (Offset 0) a1Size
  A.thaw a2 >>= \a2' -> A.copyAt a (Offset a1len) a2' (Offset 0) a2Size
  A.unsafeFreeze a

concat :: [ChunkedUArray ty] -> ChunkedUArray ty
concat x = C.foldl' append mempty x

vFromList :: PrimType ty => [ty] -> ChunkedUArray ty
vFromList l = ChunkedUArray array
  where
    array = runST $ do
      a <- A.new (Size 1)
      A.unsafeWrite a 0 (fromList l)
      A.unsafeFreeze a

vToList :: PrimType ty => ChunkedUArray ty -> [ty]
vToList (ChunkedUArray a) = loop (C.length a) 0 mempty
  where
    -- TODO: Rewrite this to use something like a `DList`
    -- to avoid the expensive `mappend`.
    loop len !acc l = case acc >= len of
      True  -> l
      False -> loop len (acc+1) ((toList (A.unsafeIndex a acc)) <> l)

null :: PrimType ty => ChunkedUArray ty -> Bool
null (ChunkedUArray array) =
  let len = C.length array
  in C.null array || allNulls 0 len
  where
    allNulls !idx len
      | idx == len = True
      | otherwise  = C.null (array `A.unsafeIndex` idx) && allNulls (idx + 1) len

-- | Returns the length of this `ChunkedUArray`, by summing each inner length.
-- Complexity: O(n*m) where `n` is the number of chunks and m the size of
-- each chunk.
length :: PrimType ty => ChunkedUArray ty -> Int
length (ChunkedUArray array) = C.foldl' (\acc l -> acc + C.length l) 0 array

--drop = _
--splitAt = _
--revTake = _
--revDrop = _
--revSplitAt = _
--splitOn = _
--break = _
--intersperse = _
--span = _
--reverse = _
--filter = _
--unsnoc = _
--uncons = _
--snoc = _
--cons = _
--find = _
--sortBy = _

--index :: ChunkedUArray ty -> Int -> ty
--index array n = _
--
--unsafeIndex :: ChunkedUArray ty -> Int -> ty
--unsafeIndex array n = _
