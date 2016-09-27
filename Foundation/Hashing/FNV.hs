-- |
-- Module      : Foundation.Hashing.FNV.FNV
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : good
--
-- Fowler Noll Vo Hash (1 and 1a / 32 / 64 bits versions)
-- <http://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function>
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE BangPatterns               #-}
module Foundation.Hashing.FNV
    (
    -- * types
      FnvHash32(..)
    , FnvHash64(..)
    -- * methods
    , fnv1
    , fnv1a
    , fnv1_64
    , fnv1a_64
    ) where

import           Foundation.Internal.Base
import qualified Foundation.Array.Unboxed as A
import           Foundation.Internal.Types
import           Foundation.Primitive.Types
import           Foundation.Number
import           Data.Bits
import           GHC.Prim
import           GHC.ST
import qualified Prelude

-- | FNV1(a) hash (32 bit variants)
newtype FnvHash32 = FnvHash32 Word32
    deriving (Show,Eq,Ord)

-- | FNV1(a) hash (64 bit variants)
newtype FnvHash64 = FnvHash64 Word64
    deriving (Show,Eq,Ord)

xor32 :: Word -> Word8 -> Word
xor32 !a !b = a `xor` Prelude.fromIntegral b
{-# INLINE xor32 #-}

xor64 :: Word64 -> Word8 -> Word64
xor64 !a !b = a `xor` Prelude.fromIntegral b
{-# INLINE xor64 #-}

-- | compute FNV1 (32 bit variant) of a raw piece of memory
fnv1 :: PrimType a => A.UArray a -> FnvHash32
fnv1 baA = FnvHash32 $ Prelude.fromIntegral $ A.unsafeDewrap goVec goAddr ba
  where
    ba :: A.UArray Word8
    ba = A.unsafeRecast baA

    goVec :: ByteArray# -> Offset Word8 -> Word
    goVec !ma !start = loop start 0
      where
        !len = start `offsetPlusE` A.lengthSize ba
        loop !idx !acc
            | idx >= len = acc
            | otherwise  = loop (idx + Offset 1) ((0x01000193 * acc) `xor32` primBaIndex ma idx)
    {-# INLINE goVec #-}

    goAddr :: Ptr Word8 -> Offset Word8 -> ST s Word
    goAddr !(Ptr ptr) !start = return $ loop start 0
      where
        !len = start `offsetPlusE` A.lengthSize ba
        loop !idx !acc
            | idx >= len = acc
            | otherwise  = loop (idx + Offset 1) ((0x01000193 * acc) `xor32` primAddrIndex ptr idx)
    {-# INLINE goAddr #-}

-- | compute FNV1a (32 bit variant) of a raw piece of memory
fnv1a :: PrimType a => A.UArray a -> FnvHash32
fnv1a baA = FnvHash32 $ Prelude.fromIntegral $ A.unsafeDewrap goVec goAddr ba
  where
    ba :: A.UArray Word8
    ba = A.unsafeRecast baA

    goVec :: ByteArray# -> Offset Word8 -> Word
    goVec !ma !start = loop start 0
      where
        !len = start `offsetPlusE` A.lengthSize ba
        loop !idx !acc
            | idx >= len = acc
            | otherwise  = loop (idx + Offset 1) (0x01000193 * (acc `xor32` primBaIndex ma idx))
    {-# INLINE goVec #-}

    goAddr :: Ptr Word8 -> Offset Word8 -> ST s Word
    goAddr !(Ptr ptr) !start = return $ loop start 0
      where
        !len = start `offsetPlusE` A.lengthSize ba
        loop !idx !acc
            | idx >= len = acc
            | otherwise  = loop (idx + Offset 1) (0x01000193 * (acc `xor32` primAddrIndex ptr idx))
    {-# INLINE goAddr #-}

-- | compute FNV1 (64 bit variant) of a raw piece of memory
fnv1_64 :: PrimType a => A.UArray a -> FnvHash64
fnv1_64 baA = FnvHash64 $ Prelude.fromIntegral $ A.unsafeDewrap goVec goAddr ba
  where
    ba :: A.UArray Word8
    ba = A.unsafeRecast baA

    goVec :: ByteArray# -> Offset Word8 -> Word64
    goVec !ma !start = loop start 0xcbf29ce484222325
      where
        !len = start `offsetPlusE` A.lengthSize ba
        loop !idx !acc
            | idx >= len = acc
            | otherwise  = loop (idx + Offset 1) ((0x100000001b3 * acc) `xor64` primBaIndex ma idx)
    {-# INLINE goVec #-}

    goAddr :: Ptr Word8 -> Offset Word8 -> ST s Word64
    goAddr !(Ptr ptr) !start = return $ loop start 0xcbf29ce484222325
      where
        !len = start `offsetPlusE` A.lengthSize ba
        loop !idx !acc
            | idx >= len = acc
            | otherwise  = loop (idx + Offset 1) ((0x100000001b3 * acc) `xor64` primAddrIndex ptr idx)
    {-# INLINE goAddr #-}

-- | compute FNV1a (64 bit variant) of a raw piece of memory
fnv1a_64 :: PrimType a => A.UArray a -> FnvHash64
fnv1a_64 baA = FnvHash64 $ Prelude.fromIntegral $ A.unsafeDewrap goVec goAddr ba
  where
    ba :: A.UArray Word8
    ba = A.unsafeRecast baA

    goVec :: ByteArray# -> Offset Word8 -> Word64
    goVec !ma !start = loop start 0xcbf29ce484222325
      where
        !len = start `offsetPlusE` A.lengthSize ba
        loop !idx !acc
            | idx >= len = acc
            | otherwise  = loop (idx + Offset 1) (0x100000001b3 * (acc `xor64` primBaIndex ma idx))
    {-# INLINE goVec #-}

    goAddr :: Ptr Word8 -> Offset Word8 -> ST s Word64
    goAddr !(Ptr ptr) !start = return $ loop start 0xcbf29ce484222325
      where
        !len = start `offsetPlusE` A.lengthSize ba
        loop !idx !acc
            | idx >= len = acc
            | otherwise  = loop (idx + Offset 1) (0x100000001b3 * (acc `xor64` primAddrIndex ptr idx))
    {-# INLINE goAddr #-}
