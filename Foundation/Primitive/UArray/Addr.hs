{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE CPP                        #-}
module Foundation.Primitive.UArray.Addr
    ( findIndexElem
    , revFindIndexElem
    , findIndexPredicate
    , foldl
    , foldr
    , foldl1
    , all
    , any
    , filter
    , primIndex
    ) where

import           GHC.Types
import           GHC.Prim
import           Foundation.Internal.Base
import           Foundation.Primitive.Numerical.Additive
import           Foundation.Primitive.Types.OffsetSize
import           Foundation.Primitive.Types
import           Foundation.Primitive.Monad

type Immutable = Addr#

primIndex :: PrimType ty => Immutable -> Offset ty -> ty
primIndex = primAddrIndex

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
