-- |
-- Module      : Foundation.Array.MultiArray
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
module Foundation.Array.MultiArray
    ( MultiArray
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


data MultiArray ty = MultiArray (Array (UArray ty))
                      deriving (Show, Eq, Ord, Typeable)

instance Monoid (MultiArray a) where
    mempty  = empty
    mappend = append
    mconcat = concat

type instance C.Element (MultiArray ty) = ty

instance PrimType ty => IsList (MultiArray ty) where
    type Item (MultiArray ty) = ty
    fromList = vFromList
    toList = vToList

instance PrimType ty => C.Collection (MultiArray ty) where
    null = null
    length = length

--instance C.Sequential (MultiArray ty) where
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

--instance C.IndexedCollection (MultiArray ty) where
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

empty :: MultiArray ty
empty = MultiArray (A.empty)

append :: MultiArray ty -> MultiArray ty -> MultiArray ty
append (MultiArray a1) (MultiArray a2) = MultiArray $ runST $ do
  let a1Size@(Size a1len) = Size $ C.length a1
  let a2Size              = Size $ C.length a2
  a <- A.new (a1Size + a2Size)
  A.thaw a1 >>= \a1' -> A.copyAt a (Offset 0) a1' (Offset 0) a1Size
  A.thaw a2 >>= \a2' -> A.copyAt a (Offset a1len) a2' (Offset 0) a2Size
  A.unsafeFreeze a

concat :: [MultiArray ty] -> MultiArray ty
concat x = C.foldl' append mempty x

vFromList :: PrimType ty => [ty] -> MultiArray ty
vFromList l = MultiArray array
  where
    array = runST $ do
      a <- A.new (Size 1)
      A.unsafeWrite a 0 (fromList l)
      A.unsafeFreeze a

vToList :: PrimType ty => MultiArray ty -> [ty]
vToList (MultiArray a) = loop (C.length a) 0 mempty
  where
    -- TODO: Rewrite this to use something like a `DList`
    -- to avoid the expensive `mappend`.
    loop len !acc l = case acc >= len of
      True  -> l
      False -> loop len (acc+1) ((toList (A.unsafeIndex a acc)) <> l)

null :: PrimType ty => MultiArray ty -> Bool
null (MultiArray array) =
  let len = C.length array
  in C.null array || allNulls 0 len
  where
    allNulls !idx len
      | idx == len = True
      | otherwise  = C.null (array `A.unsafeIndex` idx) && allNulls (idx + 1) len

-- | Returns the length of this `MultiArray`, by summing each inner length.
-- Complexity: O(n*m) where `n` is the number of chunks and m the size of
-- each chunk.
length :: PrimType ty => MultiArray ty -> Int
length (MultiArray array) = C.foldl' (\acc l -> acc + C.length l) 0 array

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

--index :: MultiArray ty -> Int -> ty
--index array n = _
--
--unsafeIndex :: MultiArray ty -> Int -> ty
--unsafeIndex array n = _
