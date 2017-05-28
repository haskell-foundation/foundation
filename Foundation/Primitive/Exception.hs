-- |
-- Module      : Foundation.Primitive.Exception
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- Common part for vectors
--
{-# LANGUAGE DeriveDataTypeable #-}
module Foundation.Primitive.Exception
    ( OutOfBound(..)
    , OutOfBoundOperation(..)
    , isOutOfBound
    , outOfBound
    , primOutOfBound
    , InvalidRecast(..)
    , RecastSourceSize(..)
    , RecastDestinationSize(..)
    ) where

import           Foundation.Internal.Base
import           Foundation.Primitive.Types.OffsetSize
import           Foundation.Primitive.Monad

-- | The type of operation that triggers an OutOfBound exception.
--
-- * OOB_Index: reading an immutable vector
-- * OOB_Read: reading a mutable vector
-- * OOB_Write: write a mutable vector
data OutOfBoundOperation = OOB_Read | OOB_Write | OOB_MemSet | OOB_MemCopy | OOB_Index
    deriving (Show,Eq,Typeable)

-- | Exception during an operation accessing the vector out of bound
--
-- Represent the type of operation, the index accessed, and the total length of the vector.
data OutOfBound = OutOfBound OutOfBoundOperation Int Int
    deriving (Show,Typeable)

instance Exception OutOfBound

outOfBound :: OutOfBoundOperation -> Offset ty -> Size ty -> a
outOfBound oobop (Offset ofs) (Size sz) = throw (OutOfBound oobop ofs sz)
{-# INLINE outOfBound #-}

primOutOfBound :: PrimMonad prim => OutOfBoundOperation -> Offset ty -> Size ty -> prim a
primOutOfBound oobop (Offset ofs) (Size sz) = primThrow (OutOfBound oobop ofs sz)
{-# INLINE primOutOfBound #-}

isOutOfBound :: Offset ty -> Size ty -> Bool
isOutOfBound (Offset ty) (Size sz) = ty < 0 || ty >= sz
{-# INLINE isOutOfBound #-}

newtype RecastSourceSize      = RecastSourceSize Int
    deriving (Show,Eq,Typeable)
newtype RecastDestinationSize = RecastDestinationSize Int
    deriving (Show,Eq,Typeable)

data InvalidRecast = InvalidRecast RecastSourceSize RecastDestinationSize
    deriving (Show,Typeable)

instance Exception InvalidRecast
