-- |
-- Module      : Foundation.Internal.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
module Foundation.Internal.Types
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
import Foundation.Internal.Base
import Foundation.Number

-- $setup
-- >>> import Foundation.Array.Unboxed

-- | File size in bytes
newtype FileSize = FileSize Word64
    deriving (Show,Eq,Ord)

-- | Offset in bytes used for memory addressing (e.g. in a vector, string, ..)
type Offset8 = Offset Word8

-- | Offset in a data structure consisting of elements of type 'ty'.
--
-- Int is a terrible backing type which is hard to get away from,
-- considering that GHC/Haskell are mostly using this for offset.
-- Trying to bring some sanity by a lightweight wrapping.
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

-- | Size of a data structure in bytes.
type Size8 = Size Word8

instance Additive (Size ty) where
    azero = Size 0
    (+) (Size a) (Size b) = Size (a+b)

instance Subtractive (Size ty) where
    type Difference (Size ty) = Size ty
    (Size a) - (Size b) = Size (a-b)

-- | Size of a data structure.
--
-- More specifically, it represents the number of elements of type `ty` that fit
-- into the data structure.
--
-- >>> lengthSize (fromList ['a', 'b', 'c', '🌟']) :: Size Char
-- Size 4
--
-- Same caveats as 'Offset' apply here.
newtype Size ty = Size Int
    deriving (Show,Eq,Ord)

sizeOfE :: Size8 -> Size ty -> Size8
sizeOfE (Size sz) (Size ty) = Size (ty * sz)
