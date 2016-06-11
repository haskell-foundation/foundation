-- |
-- Module      : Core.Internal.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
module Core.Internal.Types
    ( FileSize(..)
    , Offset(..)
    , Size(..)
    ) where

import GHC.Types
import GHC.Word
import Core.Internal.Base

-- | File size in bytes
newtype FileSize = FileSize Word64
    deriving (Show,Eq,Ord)

-- | Offset in bytes used for memory addressing (e.g. in a vector, string, ..)
--
-- Int a terrible backing type which is hard to get away
-- considering that GHC/haskell are mostly using this for offset.
-- try to bring some sanity by a lightweight wrapping
newtype Offset = Offset Int
    deriving (Show,Eq,Ord)

-- | Size in bytes
--
-- Same caveat as Offset apply here
newtype Size = Size Int
    deriving (Show,Eq,Ord)
