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
    , Offset8
    , offsetOfE
    , offsetPlusE
    , offsetMinusE
    , offsetRecast
    , (+.)
    , Size(..)
    , Size8
    , sizeOfE
    ) where

import GHC.Types
import GHC.Word
import Core.Internal.Base
import Core.Number

-- | File size in bytes
newtype FileSize = FileSize Word64
    deriving (Show,Eq,Ord)

-- | Offset in bytes used for memory addressing (e.g. in a vector, string, ..)
--
-- Int a terrible backing type which is hard to get away
-- considering that GHC/haskell are mostly using this for offset.
-- try to bring some sanity by a lightweight wrapping
--newtype Offset = Offset Int
--    deriving (Show,Eq,Ord)
type Offset8 = Offset Word8

-- | Offset in element of a certain type
newtype Offset ty = Offset Int
    deriving (Show,Eq,Ord)

instance Additive (Offset ty) where
    azero = Offset 0
    (+) (Offset a) (Offset b) = Offset (a+b)

instance Subtractive (Offset ty) where
    type Difference (Offset ty) = Size ty
    (Offset a) - (Offset b) = Size (a-b)

(+.) :: Offset ty -> Int -> Offset ty
(+.) (Offset a) b = Offset (a + b)

offsetOfE :: Size8 -> Offset ty -> Offset8
offsetOfE (Size sz) (Offset ty) = Offset (ty * sz)

offsetPlusE :: Offset ty -> Size ty -> Offset ty
offsetPlusE (Offset ofs) (Size sz) = Offset (ofs + sz)

offsetMinusE :: Offset ty -> Size ty -> Offset ty
offsetMinusE (Offset ofs) (Size sz) = Offset (ofs - sz)

offsetRecast :: Size8 -> Size8 -> Offset ty -> Offset ty2
offsetRecast szTy (Size szTy2) ofs =
    let (Offset bytes) = offsetOfE szTy ofs
     in Offset (bytes `div` szTy2)

-- | Size in bytes
--
-- Same caveat as Offset apply here
type Size8 = Size Word8

instance Additive (Size ty) where
    azero = Size 0
    (+) (Size a) (Size b) = Size (a+b)

instance Subtractive (Size ty) where
    type Difference (Size ty) = Size ty
    (Size a) - (Size b) = Size (a-b)

-- | Size in element
newtype Size ty = Size Int
    deriving (Show,Eq,Ord)

sizeOfE :: Size8 -> Size ty -> Size8
sizeOfE (Size sz) (Size ty) = Size (ty * sz)
