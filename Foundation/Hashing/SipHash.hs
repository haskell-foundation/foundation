-- |
-- Module      : Foundation.Hashing.SipHash
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : good
--
-- provide the SipHash algorithm.
-- reference: <http://131002.net/siphash/siphash.pdf>
--
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
module Foundation.Hashing.SipHash
    ( SipKey(..)
    , SipHash(..)
    , hash
    , hashWith
    ) where

import           Foundation.Internal.Base
import           Foundation.Internal.Types
import           Foundation.Primitive.Types
import qualified Foundation.Array.Unboxed as A
import           Foundation.Array
import           Foundation.Number
import           Data.Bits
import qualified Prelude
import           GHC.ST
import           GHC.Prim

-- | SigHash Key
data SipKey = SipKey {-# UNPACK #-} !Word64
                     {-# UNPACK #-} !Word64

-- | Siphash Hash value
newtype SipHash = SipHash Word64
    deriving (Show,Eq,Ord)

data InternalState = InternalState {-# UNPACK #-} !Word64
                                   {-# UNPACK #-} !Word64
                                   {-# UNPACK #-} !Word64
                                   {-# UNPACK #-} !Word64

-- | produce a siphash with a key and an array
hash :: PrimType a => SipKey -> UArray a -> SipHash
hash key ba = hashWith8 2 4 key (A.unsafeRecast ba)

-- | same as 'hash', except also specifies the number of sipround iterations for compression (C) and digest (D).
hashWith :: PrimType a
         => Int        -- ^ SipHash C (e.g. 2)
         -> Int        -- ^ SipHash D (e.g. 4)
         -> SipKey     -- ^ SipHash Key used for hashing
         -> UArray a   -- ^ Data to hash
         -> SipHash
hashWith c d key ba = hashWith8 c d key (A.unsafeRecast ba)

hashWith8 :: Int -> Int -> SipKey -> UArray Word8 -> SipHash
hashWith8 c d key byteArray = A.unsafeDewrap goVec goAddr byteArray
  where
    totalLen = A.length byteArray

    goVec :: ByteArray# -> Offset Word8 -> SipHash
    goVec ba start = finish $ loop (initSip key) start totalLen
      where
        loop !st !ofs !l
            | l < 8     = end st ofs l
            | otherwise =
                let v =     to64 56 (primBaIndex ba ofs)
                        .|. to64 48 (primBaIndex ba (ofs + Offset 1))
                        .|. to64 40 (primBaIndex ba (ofs + Offset 2))
                        .|. to64 32 (primBaIndex ba (ofs + Offset 3))
                        .|. to64 24 (primBaIndex ba (ofs + Offset 4))
                        .|. to64 16 (primBaIndex ba (ofs + Offset 5))
                        .|. to64 8  (primBaIndex ba (ofs + Offset 6))
                        .|. to64 0  (primBaIndex ba (ofs + Offset 7))
                in loop (process st v) (start + Offset 8) (l - 8)
        end !st !ofs l =
            let !lengthBlock = to64 56 (Prelude.fromIntegral totalLen)
                get64 s o    = to64 s (primBaIndex ba (ofs + Offset o))
                !v = case l of
                        0 -> lengthBlock
                        1 -> lengthBlock .|. get64 0 0
                        2 -> lengthBlock .|. get64 8 0  .|. get64 0 1
                        3 -> lengthBlock .|. get64 16 0 .|. get64 8  1 .|. get64 0  2
                        4 -> lengthBlock .|. get64 24 0 .|. get64 16 1 .|. get64 8  2 .|. get64 0  3
                        5 -> lengthBlock .|. get64 32 0 .|. get64 24 1 .|. get64 16 2 .|. get64 8  3 .|. get64 0  4
                        6 -> lengthBlock .|. get64 40 0 .|. get64 32 1 .|. get64 24 2 .|. get64 16 3 .|. get64 8  4 .|. get64 0 5
                        _ -> lengthBlock .|. get64 48 0 .|. get64 40 1 .|. get64 32 2 .|. get64 24 3 .|. get64 16 4 .|. get64 8 5 .|. get64 0 6
             in process st v

    goAddr :: Ptr Word8 -> Offset Word8 -> ST s SipHash
    goAddr (Ptr ptr) start = return $ finish $ loop (initSip key) start totalLen
      where
        loop !st !ofs !l
            | l < 8     = end st ofs l
            | otherwise =
                let v =     to64 56 (primAddrIndex ptr ofs)
                        .|. to64 48 (primAddrIndex ptr (ofs + Offset 1))
                        .|. to64 40 (primAddrIndex ptr (ofs + Offset 2))
                        .|. to64 32 (primAddrIndex ptr (ofs + Offset 3))
                        .|. to64 24 (primAddrIndex ptr (ofs + Offset 4))
                        .|. to64 16 (primAddrIndex ptr (ofs + Offset 5))
                        .|. to64 8  (primAddrIndex ptr (ofs + Offset 6))
                        .|. to64 0  (primAddrIndex ptr (ofs + Offset 7))
                in loop (process st v) (start + Offset 8) (l - 8)
        end !st !ofs l =
            let !lengthBlock = to64 56 (Prelude.fromIntegral totalLen)
                get64 s 0    = to64 s (primAddrIndex ptr ofs)
                get64 s o    = to64 s (primAddrIndex ptr (ofs + Offset o))
                !v = case l of
                        0 -> lengthBlock
                        1 -> lengthBlock .|. get64 0 0
                        2 -> lengthBlock .|. get64 8 0  .|. get64 0 1
                        3 -> lengthBlock .|. get64 16 0 .|. get64 8  1 .|. get64 0  2
                        4 -> lengthBlock .|. get64 24 0 .|. get64 16 1 .|. get64 8  2 .|. get64 0  3
                        5 -> lengthBlock .|. get64 32 0 .|. get64 24 1 .|. get64 16 2 .|. get64 8  3 .|. get64 0  4
                        6 -> lengthBlock .|. get64 40 0 .|. get64 32 1 .|. get64 24 2 .|. get64 16 3 .|. get64 8  4 .|. get64 0 5
                        _ -> lengthBlock .|. get64 48 0 .|. get64 40 1 .|. get64 32 2 .|. get64 24 3 .|. get64 16 4 .|. get64 8 5 .|. get64 0 6
             in process st v

    to64 :: Int -> Word8 -> Word64
    to64 0  !v = Prelude.fromIntegral v
    to64 !s !v = Prelude.fromIntegral v `unsafeShiftL` s

    {-# INLINE process #-}
    process istate m = postInject $! runRoundsCompression $! preInject istate
      where
              preInject  (InternalState v0 v1 v2 v3) = InternalState v0 v1 v2 (v3 `xor` m)
              postInject (InternalState v0 v1 v2 v3) = InternalState (v0 `xor` m) v1 v2 v3

    {-# INLINE finish #-}
    finish istate = getDigest $! runRoundsDigest $! preInject istate
        where getDigest (InternalState v0 v1 v2 v3) = SipHash (v0 `xor` v1 `xor` v2 `xor` v3)
              preInject (InternalState v0 v1 v2 v3) = InternalState v0 v1 (v2 `xor` 0xff) v3

    {-# INLINE doRound #-}
    doRound (InternalState v0 v1 v2 v3) =
          let !v0'    = v0 + v1
              !v2'    = v2 + v3
              !v1'    = v1 `rotateL` 13
              !v3'    = v3 `rotateL` 16
              !v1''   = v1' `xor` v0'
              !v3''   = v3' `xor` v2'
              !v0''   = v0' `rotateL` 32
              !v2''   = v2' + v1''
              !v0'''  = v0'' + v3''
              !v1'''  = v1'' `rotateL` 17
              !v3'''  = v3'' `rotateL` 21
              !v1'''' = v1''' `xor` v2''
              !v3'''' = v3''' `xor` v0'''
              !v2'''  = v2'' `rotateL` 32
           in InternalState v0''' v1'''' v2''' v3''''

    {-# INLINE runRoundsCompression #-}
    runRoundsCompression st
        | c == 2    = doRound $! doRound st
        | otherwise = loopRounds c st

    {-# INLINE runRoundsDigest #-}
    runRoundsDigest st
        | d == 4    = doRound $! doRound $! doRound $! doRound st
        | otherwise = loopRounds d st

    {-# INLINE loopRounds #-}
    loopRounds 1 !v = doRound v
    loopRounds n !v = loopRounds (n-1) (doRound v)

    {-# INLINE initSip #-}
    initSip (SipKey k0 k1) = InternalState (k0 `xor` 0x736f6d6570736575)
                                           (k1 `xor` 0x646f72616e646f6d)
                                           (k0 `xor` 0x6c7967656e657261)
                                           (k1 `xor` 0x7465646279746573)
