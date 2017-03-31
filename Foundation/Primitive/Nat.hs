{-# LANGUAGE CPP                       #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}
#if __GLASGOW_HASKELL__ < 710
# error "IMPORT ERROR: cannot include this file with GHC version below 7.10
#else
module Foundation.Primitive.Nat
    ( Nat
    , KnownNat
    , natVal
    , type (<=), type (<=?), type (+), type (*), type (^), type (-)
    , CmpNat
    -- * Nat convertion
    , natValInt
    , natValInt8
    , natValInt16
    , natValInt32
    , natValInt64
    , natValWord
    , natValWord8
    , natValWord16
    , natValWord32
    , natValWord64
    -- * Maximum bounds
    , NatMaxBoundInt
    , NatMaxBoundInt8
    , NatMaxBoundInt16
    , NatMaxBoundInt32
    , NatMaxBoundInt64
    , NatMaxBoundWord
    , NatMaxBoundWord8
    , NatMaxBoundWord16
    , NatMaxBoundWord32
    , NatMaxBoundWord64
    -- * Constraint
    , NatWithinIntBound
    , NatWithinInt64Bound
    , NatWithinInt32Bound
    , NatWithinInt16Bound
    , NatWithinInt8Bound
    , NatWithinWordBound
    , NatWithinWord64Bound
    , NatWithinWord32Bound
    , NatWithinWord16Bound
    , NatWithinWord8Bound
    ) where

#include "MachDeps.h"

import           GHC.TypeLits
import           Foundation.Internal.Base
import           Data.Type.Bool
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.Word (Word8, Word16, Word32, Word64)
import qualified Prelude (fromIntegral)

--natValSize :: forall n proxy. KnownNat n => proxy n -> Size a
--natValSize

natValInt :: forall n proxy . (KnownNat n, NatWithinIntBound n) => proxy n -> Int
natValInt n = Prelude.fromIntegral (natVal n)

natValInt64 :: forall n proxy . (KnownNat n, NatWithinInt64Bound n) => proxy n -> Int64
natValInt64 n = Prelude.fromIntegral (natVal n)

natValInt32 :: forall n proxy . (KnownNat n, NatWithinInt32Bound n) => proxy n -> Int32
natValInt32 n = Prelude.fromIntegral (natVal n)

natValInt16 :: forall n proxy . (KnownNat n, NatWithinInt16Bound n) => proxy n -> Int16
natValInt16 n = Prelude.fromIntegral (natVal n)

natValInt8 :: forall n proxy . (KnownNat n, NatWithinInt8Bound n) => proxy n -> Int8
natValInt8 n = Prelude.fromIntegral (natVal n)

natValWord :: forall n proxy . (KnownNat n, NatWithinWordBound n) => proxy n -> Word
natValWord n = Prelude.fromIntegral (natVal n)

natValWord64 :: forall n proxy . (KnownNat n, NatWithinWord64Bound n) => proxy n -> Word64
natValWord64 n = Prelude.fromIntegral (natVal n)

natValWord32 :: forall n proxy . (KnownNat n, NatWithinWord32Bound n) => proxy n -> Word32
natValWord32 n = Prelude.fromIntegral (natVal n)

natValWord16 :: forall n proxy . (KnownNat n, NatWithinWord16Bound n) => proxy n -> Word16
natValWord16 n = Prelude.fromIntegral (natVal n)

natValWord8 :: forall n proxy . (KnownNat n, NatWithinWord8Bound n) => proxy n -> Word8
natValWord8 n = Prelude.fromIntegral (natVal n)

#if WORD_SIZE_IN_BITS == 64
type NatMaxBoundInt = NatMaxBoundInt64
#else
type NatMaxBoundInt = NatMaxBoundInt32
#endif

type NatMaxBoundInt64 = 0x7fffffffffffffff
type NatMaxBoundInt32 = 0x7fffffff
type NatMaxBoundInt16 = 0x7fff
type NatMaxBoundInt8 = 0x7f

#if WORD_SIZE_IN_BITS == 64
type NatMaxBoundWord = NatMaxBoundWord64
#else
type NatMaxBoundWord = NatMaxBoundWord32
#endif

type NatMaxBoundWord64 = 0xffffffffffffffff
type NatMaxBoundWord32 = 0xffffffff
type NatMaxBoundWord16 = 0xffff
type NatMaxBoundWord8  = 0xff

type family NatWithinIntBound (n :: Nat) where
    NatWithinIntBound n = If (n <=? NatMaxBoundInt)
        (() ~ ())
        (TypeError ('Text "Natural " ':<>: 'ShowType n ':<>: 'Text " is out of bounds for Int"))

type family NatWithinInt64Bound (n :: Nat) where
    NatWithinInt64Bound n = If (n <=? NatMaxBoundInt64)
        (() ~ ())
        (TypeError ('Text "Natural " ':<>: 'ShowType n ':<>: 'Text " is out of bounds for Int64"))

type family NatWithinInt32Bound (n :: Nat) where
    NatWithinInt32Bound n = If (n <=? NatMaxBoundInt32)
        (() ~ ())
        (TypeError ('Text "Natural " ':<>: 'ShowType n ':<>: 'Text " is out of bounds for Int32"))

type family NatWithinInt16Bound (n :: Nat) where
    NatWithinInt16Bound n = If (n <=? NatMaxBoundInt16)
        (() ~ ())
        (TypeError ('Text "Natural " ':<>: 'ShowType n ':<>: 'Text " is out of bounds for Int16"))

type family NatWithinInt8Bound (n :: Nat) where
    NatWithinInt8Bound n = If (n <=? NatMaxBoundInt8)
        (() ~ ())
        (TypeError ('Text "Natural " ':<>: 'ShowType n ':<>: 'Text " is out of bounds for Int8"))

type family NatWithinWordBound (n :: Nat) where
    NatWithinWordBound n = If (n <=? NatMaxBoundWord)
        (() ~ ())
        (TypeError ('Text "Natural " ':<>: 'ShowType n ':<>: 'Text " is out of bounds for Word"))

type family NatWithinWord64Bound (n :: Nat) where
    NatWithinWord64Bound n = If (n <=? NatMaxBoundWord64)
        (() ~ ())
        (TypeError ('Text "Natural " ':<>: 'ShowType n ':<>: 'Text " is out of bounds for Word64"))

type family NatWithinWord32Bound (n :: Nat) where
    NatWithinWord32Bound n = If (n <=? NatMaxBoundWord32)
        (() ~ ())
        (TypeError ('Text "Natural " ':<>: 'ShowType n ':<>: 'Text " is out of bounds for Word32"))

type family NatWithinWord16Bound (n :: Nat) where
    NatWithinWord16Bound n = If (n <=? NatMaxBoundWord16)
        (() ~ ())
        (TypeError ('Text "Natural " ':<>: 'ShowType n ':<>: 'Text " is out of bounds for Word16"))

type family NatWithinWord8Bound (n :: Nat) where
    NatWithinWord8Bound n = If (n <=? NatMaxBoundWord8)
        (() ~ ())
        (TypeError ('Text "Natural " ':<>: 'ShowType n ':<>: 'Text " is out of bounds for Word8"))

#endif
