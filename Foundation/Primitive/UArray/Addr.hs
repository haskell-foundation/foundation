{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE CPP                        #-}
module Foundation.Primitive.UArray.Addr
    ( findIndexElem
    , findIndexPredicate
    , filter
    ) where

import           GHC.Types
import           GHC.Prim
import           Foundation.Internal.Base
import           Foundation.Numerical
import           Foundation.Primitive.Types.OffsetSize
import           Foundation.Primitive.Types
import           Foundation.Primitive.Monad

type Immutable = Addr#

primIndex :: PrimType ty => Immutable -> Offset ty -> ty
primIndex = primAddrIndex

findIndexElem :: PrimType ty => ty -> Immutable -> Offset ty -> Offset ty -> Offset ty
findIndexElem ty ba startIndex endIndex = loop startIndex
  where
    loop i
        | i < endIndex && t /= ty = loop (i+Offset 1)
        | otherwise               = i
      where t = primIndex ba i
{-# INLINE findIndexElem #-}

findIndexPredicate :: PrimType ty => (ty -> Bool) -> Immutable -> Offset ty -> Offset ty -> Offset ty
findIndexPredicate predicate ba startIndex endIndex = loop startIndex
  where
    loop i
        | i < endIndex && not found = loop (i+Offset 1)
        | otherwise                 = i
      where found = predicate (primIndex ba i)
{-# INLINE findIndexPredicate #-}

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
