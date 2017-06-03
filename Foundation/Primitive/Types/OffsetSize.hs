-- |
-- Module      : Foundation.Primitive.Types.OffsetSize
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE CPP                        #-}
module Foundation.Primitive.Types.OffsetSize
    ( FileSize(..)
    , Offset(..)
    , Offset8
    , offsetOfE
    , offsetPlusE
    , offsetMinusE
    , offsetRecast
    , offsetCast
    , sizeCast
    , sizeLastOffset
    , sizeAsOffset
    , offsetAsSize
    , (+.)
    , (.==#)
    , Size(..)
    , Size8
    , sizeOfE
    , csizeOfOffset
    , csizeOfSize
    , sizeOfCSSize
    , sizeOfCSize
    ) where

#include "MachDeps.h"

import GHC.Types
import GHC.Word
import GHC.Int
import GHC.Prim
import Foreign.C.Types
import System.Posix.Types (CSsize (..))
import Foundation.Internal.Base
import Foundation.Internal.Proxy
import Foundation.Numerical.Primitives
import Foundation.Numerical.Number
import Foundation.Numerical.Additive
import Foundation.Numerical.Subtractive
import Foundation.Numerical.Multiplicative
import Foundation.Primitive.IntegralConv

#if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64
#endif

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
    deriving (Show,Eq,Ord,Enum,Additive)

instance Integral (Offset ty) where
    fromInteger n
        | n < 0     = error "Size: fromInteger: negative"
        | otherwise = Offset . fromInteger $ n
instance IsIntegral (Offset ty) where
    toInteger (Offset i) = toInteger i
instance IsNatural (Offset ty) where
    toNatural (Offset i) = toNatural (intToWord i)
instance Subtractive (Offset ty) where
    type Difference (Offset ty) = Size ty
    (Offset a) - (Offset b) = Size (a-b)
instance IntegralCast Int (Offset ty) where
    integralCast i = Offset i
instance IntegralCast Word (Offset ty) where
    integralCast (W# w) = Offset (I# (word2Int# w))


(+.) :: Offset ty -> Int -> Offset ty
(+.) (Offset a) b = Offset (a + b)
{-# INLINE (+.) #-}

-- . is offset (as a pointer from a beginning), and # is the size (amount of data)
(.==#) :: Offset ty -> Size ty -> Bool
(.==#) (Offset ofs) (Size sz) = ofs == sz
{-# INLINE (.==#) #-}

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

offsetCast :: Proxy (a -> b) -> Offset a -> Offset b
offsetCast _ (Offset o) = Offset o
{-# INLINE offsetCast #-}

sizeCast :: Proxy (a -> b) -> Size a -> Size b
sizeCast _ (Size sz) = Size sz
{-# INLINE sizeCast #-}

-- TODO add a callstack, or a construction to prevent size == 0 error
sizeLastOffset :: Size a -> Offset a
sizeLastOffset (Size s)
    | s > 0     = Offset (pred s)
    | otherwise = error "last offset on size 0"

sizeAsOffset :: Size a -> Offset a
sizeAsOffset (Size a) = Offset a
{-# INLINE sizeAsOffset #-}

offsetAsSize :: Offset a -> Size a
offsetAsSize (Offset a) = Size a
{-# INLINE offsetAsSize #-}


-- | Size of a data structure in bytes.
type Size8 = Size Word8

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
    deriving (Show,Eq,Ord,Enum)

instance Integral (Size ty) where
    fromInteger n
        | n < 0     = error "Size: fromInteger: negative"
        | otherwise = Size . fromInteger $ n
instance IsIntegral (Size ty) where
    toInteger (Size i) = toInteger i
instance IsNatural (Size ty) where
    toNatural (Size i) = toNatural (intToWord i)

instance Additive (Size ty) where
    azero = Size 0
    (+) (Size a) (Size b) = Size (a+b)

instance Subtractive (Size ty) where
    type Difference (Size ty) = Size ty
    (Size a) - (Size b) = Size (a-b)

instance IntegralCast Int (Size ty) where
    integralCast i = Size i
instance IntegralCast Word (Size ty) where
    integralCast (W# w) = Size (I# (word2Int# w))

sizeOfE :: Size8 -> Size ty -> Size8
sizeOfE (Size sz) (Size ty) = Size (ty * sz)

-- when #if WORD_SIZE_IN_BITS < 64 the 2 following are wrong
-- instead of using FromIntegral and being silently wrong
-- explicit pattern match to sort it out.

csizeOfSize :: Size8 -> CSize
#if WORD_SIZE_IN_BITS < 64
csizeOfSize (Size (I# sz)) = CSize (W32# (int2Word# sz))
#else
csizeOfSize (Size (I# sz)) = CSize (W64# (int2Word# sz))
#endif

csizeOfOffset :: Offset8 -> CSize
#if WORD_SIZE_IN_BITS < 64
csizeOfOffset (Offset (I# sz)) = CSize (W32# (int2Word# sz))
#else
csizeOfOffset (Offset (I# sz)) = CSize (W64# (int2Word# sz))
#endif

sizeOfCSSize :: CSsize -> Size8
sizeOfCSSize (CSsize (-1))      = error "invalid size: CSSize is -1"
#if WORD_SIZE_IN_BITS < 64
sizeOfCSSize (CSsize (I32# sz)) = Size (I# sz)
#else
sizeOfCSSize (CSsize (I64# sz)) = Size (I# sz)
#endif

sizeOfCSize :: CSize -> Size8
#if WORD_SIZE_IN_BITS < 64
sizeOfCSize (CSize (W32# sz)) = Size (I# (word2Int# sz))
#else
sizeOfCSize (CSize (W64# sz)) = Size (I# (word2Int# sz))
#endif
