-- |
-- Module      : Core.Array.Common
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- Common part for vectors
--
{-# LANGUAGE DeriveDataTypeable #-}
module Core.Array.Common
    ( OutOfBound(..)
    , OutOfBoundOperation(..)

    , InvalidRecast(..)
    , RecastSourceSize(..)
    , RecastDestinationSize(..)
    ) where

import           Core.Internal.Base

-- | The type of operation that triggers an OutOfBound exception.
--
-- * OOB_Index: reading an immutable vector
-- * OOB_Read: reading a mutable vector
-- * OOB_Write: write a mutable vector
data OutOfBoundOperation = OOB_Read | OOB_Write | OOB_MemSet | OOB_Index
    deriving (Show,Eq,Typeable)

-- | Exception during an operation accessing the vector out of bound
--
-- Represent the type of operation, the index accessed, and the total length of the vector.
data OutOfBound = OutOfBound OutOfBoundOperation Int Int
    deriving (Show,Typeable)

instance Exception OutOfBound

newtype RecastSourceSize      = RecastSourceSize Int
    deriving (Show,Eq,Typeable)
newtype RecastDestinationSize = RecastDestinationSize Int
    deriving (Show,Eq,Typeable)

data InvalidRecast = InvalidRecast RecastSourceSize RecastDestinationSize
    deriving (Show,Typeable)

instance Exception InvalidRecast
