{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE CPP                        #-}
module Basement.UArray.Addr
    ( findIndexElem
    , revFindIndexElem
    , findIndexPredicate
    , revFindIndexPredicate
    , foldl
    , foldr
    , foldl1
    , all
    , any
    , filter
    , primIndex
    , inplaceSortBy
    ) where

import           GHC.Types
import           GHC.Prim
import           Basement.Compat.Base
import           Basement.Numerical.Additive
import           Basement.Numerical.Multiplicative
import           Basement.Types.OffsetSize
import           Basement.PrimType
import           Basement.Monad

import           Basement.Alg.Foreign.Prim

findIndexElem :: PrimType ty => ty -> Immutable -> Offset ty -> Offset ty -> Offset ty
findIndexElem ty ba startIndex endIndex = loop startIndex
  where
    loop !i
        | i < endIndex && t /= ty = loop (i+1)
        | otherwise               = i
      where t = primIndex ba i
{-# INLINE findIndexElem #-}

revFindIndexElem :: PrimType ty => ty -> Immutable -> Offset ty -> Offset ty -> Offset ty
revFindIndexElem ty ba startIndex endIndex
    | endIndex > startIndex = loop (endIndex `offsetMinusE` 1)
    | otherwise             = endIndex
  where
    loop !i
        | t == ty        = i
        | i > startIndex = loop (i `offsetMinusE` 1)
        | otherwise      = endIndex
      where t = primIndex ba i
{-# INLINE revFindIndexElem #-}

findIndexPredicate :: PrimType ty => (ty -> Bool) -> Immutable -> Offset ty -> Offset ty -> Offset ty
findIndexPredicate predicate ba !startIndex !endIndex = loop startIndex
  where
    loop !i
        | i < endIndex && not found = loop (i+1)
        | otherwise                 = i
      where found = predicate (primIndex ba i)
{-# INLINE findIndexPredicate #-}

revFindIndexPredicate :: PrimType ty => (ty -> Bool) -> Immutable -> Offset ty -> Offset ty -> Offset ty
revFindIndexPredicate predicate ba startIndex endIndex
    | endIndex > startIndex = loop (endIndex `offsetMinusE` 1)
    | otherwise             = endIndex
  where
    loop !i
        | found          = i
        | i > startIndex = loop (i `offsetMinusE` 1)
        | otherwise      = endIndex
      where found = predicate (primIndex ba i)
{-# INLINE revFindIndexPredicate #-}

foldl :: PrimType ty => (a -> ty -> a) -> a -> Immutable -> Offset ty -> Offset ty -> a
foldl f !initialAcc ba !startIndex !endIndex = loop startIndex initialAcc
  where
    loop !i !acc
        | i == endIndex = acc
        | otherwise     = loop (i+1) (f acc (primIndex ba i))
{-# INLINE foldl #-}

foldr :: PrimType ty => (ty -> a -> a) -> a -> Immutable -> Offset ty -> Offset ty -> a
foldr f !initialAcc ba startIndex endIndex = loop startIndex
  where
    loop !i
        | i == endIndex = initialAcc
        | otherwise     = primIndex ba i `f` loop (i+1)
{-# INLINE foldr #-}

foldl1 :: PrimType ty => (ty -> ty -> ty) -> Immutable -> Offset ty -> Offset ty -> ty
foldl1 f ba startIndex endIndex = loop (startIndex+1) (primIndex ba startIndex)
  where
    loop !i !acc
        | i == endIndex = acc
        | otherwise     = loop (i+1) (f acc (primIndex ba i))
{-# INLINE foldl1 #-}

filter :: (PrimMonad prim, PrimType ty)
       => (ty -> Bool) -> MutableByteArray# (PrimState prim) -> Immutable -> Offset ty -> Offset ty -> prim (CountOf ty)
filter predicate dst src start end = loop azero start
  where
    loop !d !s
        | s == end    = pure (offsetAsSize d)
        | predicate v = primMbaWrite dst d v >> loop (d+Offset 1) (s+Offset 1)
        | otherwise   = loop d (s+Offset 1)
      where
        v = primIndex src s

all :: PrimType ty => (ty -> Bool) -> Immutable -> Offset ty -> Offset ty -> Bool
all predicate ba start end = loop start
  where
    loop !i
        | i == end                   = True
        | predicate (primIndex ba i) = loop (i+1)
        | otherwise                  = False
{-# INLINE all #-}

any :: PrimType ty => (ty -> Bool) -> Immutable -> Offset ty -> Offset ty -> Bool
any predicate ba start end = loop start
  where
    loop !i
        | i == end                   = False
        | predicate (primIndex ba i) = True
        | otherwise                  = loop (i+1)
{-# INLINE any #-}

inplaceSortBy :: (PrimType ty, PrimMonad prim)
              => (ty -> ty -> Ordering)
              -> Mutable (PrimState prim)
              -> Offset ty
              -> Offset ty
              -> prim ()
inplaceSortBy ford ma start end = qsort start (end `offsetSub` 1)
  where
    qsort lo hi
        | lo >= hi  = pure ()
        | otherwise = do
            p <- partition lo hi
            qsort lo (pred p)
            qsort (p+1) hi
    pivotStrategy (Offset low) hi@(Offset high) = do
        let mid = Offset $ (low + high) `div` 2
        pivot <- primRead ma mid
        primRead ma hi >>= primWrite ma mid
        primWrite ma hi pivot -- move pivot @ pivotpos := hi
        pure pivot
    partition lo hi = do
        pivot <- pivotStrategy lo hi
        -- RETURN: index of pivot with [<pivot | pivot | >=pivot]
        -- INVARIANT: i & j are valid array indices; pivotpos==hi
        let go i j = do
                -- INVARIANT: k <= pivotpos
                let fw k = do ak <- primRead ma k
                              if ford ak pivot == LT
                                then fw (k+1)
                                else pure (k, ak)
                (i, ai) <- fw i -- POST: ai >= pivot
                -- INVARIANT: k >= i
                let bw k | k==i = pure (i, ai)
                         | otherwise = do ak <- primRead ma k
                                          if ford ak pivot /= LT
                                            then bw (pred k)
                                            else pure (k, ak)
                (j, aj) <- bw j -- POST: i==j OR (aj<pivot AND j<pivotpos)
                -- POST: ai>=pivot AND (i==j OR aj<pivot AND (j<pivotpos))
                if i < j
                    then do -- (ai>=p AND aj<p) AND (i<j<pivotpos)
                        -- swap two non-pivot elements and proceed
                        primWrite ma i aj
                        primWrite ma j ai
                        -- POST: (ai < pivot <= aj)
                        go (i+1) (pred j)
                    else do -- ai >= pivot
                        -- complete partitioning by swapping pivot to the center
                        primWrite ma hi ai
                        primWrite ma i pivot
                        pure i
        go lo hi
{-# INLINE inplaceSortBy #-}
