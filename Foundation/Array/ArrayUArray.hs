-- |
-- Module      : Foundation.Array.ArrayUArray
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- Simple array-of-arrays abstraction
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
module Foundation.Array.ArrayUArray
    ( ArrayUArray
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


data ArrayUArray ty = ArrayUArray (Array (UArray ty))
                      deriving (Show, Eq, Ord, Typeable)

instance Monoid (ArrayUArray a) where
    mempty  = empty
    mappend = append
    mconcat = concat

type instance C.Element (ArrayUArray ty) = ty

instance PrimType ty => IsList (ArrayUArray ty) where
    type Item (ArrayUArray ty) = ty
    fromList = vFromList
    toList = vToList

--instance C.Sequential (ArrayUArray ty) where
--    null = null
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
--    length = length
--    singleton = fromList . (:[])

--instance C.IndexedCollection (ArrayUArray ty) where
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

empty :: ArrayUArray ty
empty = ArrayUArray (A.empty)

append :: ArrayUArray ty -> ArrayUArray ty -> ArrayUArray ty
append (ArrayUArray a1) (ArrayUArray a2) = ArrayUArray $ runST $ do
  let a1Size@(Size a1len) = Size $ C.length a1
  let a2Size              = Size $ C.length a2
  a <- A.new (a1Size + a2Size)
  A.thaw a1 >>= \a1' -> A.copyAt a (Offset 0) a1' (Offset 0) a1Size
  A.thaw a2 >>= \a2' -> A.copyAt a (Offset a1len) a2' (Offset 0) a2Size
  A.unsafeFreeze a

concat :: [ArrayUArray ty] -> ArrayUArray ty
concat x = C.foldl' append mempty x

vFromList :: PrimType ty => [ty] -> ArrayUArray ty
vFromList l = ArrayUArray array
  where
    array = runST $ do
      a <- A.new (Size 1)
      A.unsafeWrite a 0 (fromList l)
      A.unsafeFreeze a

vToList :: PrimType ty => ArrayUArray ty -> [ty]
vToList (ArrayUArray a) = loop (C.length a) 0 mempty
  where
    -- TODO: Rewrite this to use something like a `DList`
    -- to avoid the expensive `mappend`.
    loop len !acc l = case acc >= len of
      True  -> l
      False -> loop len (acc+1) ((toList (A.unsafeIndex a acc)) <> l)

--null = _
--take = _
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
--length = _

--index :: ArrayUArray ty -> Int -> ty
--index array n = _
--
--unsafeIndex :: ArrayUArray ty -> Int -> ty
--unsafeIndex array n = _
