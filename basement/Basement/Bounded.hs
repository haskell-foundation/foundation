-- |
-- Module      : Basement.Block
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
--
-- Types to represent ℤ/nℤ.
--
-- ℤ/nℤ is a finite field and is defined as the set of natural number:
-- {0, 1, ..., n − 1}.
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Basement.Bounded
    ( Zn64
    , unZn64
    , Zn
    , unZn
    , zn64
    , zn
    , zn64Nat
    , znNat
    ) where

import           GHC.TypeLits
import           Data.Word
import           Basement.Compat.Base
import           Basement.Compat.Natural
import           Data.Proxy
import           Basement.Nat
import qualified Prelude

-- | A type level bounded natural backed by a Word64
newtype Zn64 (n :: Nat) = Zn64 { unZn64 :: Word64 }
    deriving (Show,Eq,Ord)

-- | Create an element of ℤ/nℤ from a Word64
--
-- If the value is greater than n, then the value is normalized by using the
-- integer modulus n
zn64 :: forall n . (KnownNat n, NatWithinBound Word64 n) => Word64 -> Zn64 n
zn64 v = Zn64 (v `Prelude.mod` natValWord64 (Proxy :: Proxy n))

-- | Create an element of ℤ/nℤ from a type level Nat
zn64Nat :: forall m n . (KnownNat m, KnownNat n, NatWithinBound Word64 m, NatWithinBound Word64 n, CmpNat m n ~ 'LT)
        => Proxy m
        -> Zn64 n
zn64Nat p = Zn64 (natValWord64 p)

-- | A type level bounded natural
newtype Zn (n :: Nat) = Zn { unZn :: Natural }
    deriving (Show,Eq,Ord)

-- | Create an element of ℤ/nℤ from a Natural.
--
-- If the value is greater than n, then the value is normalized by using the
-- integer modulus n
zn :: forall n . KnownNat n => Natural -> Zn n
zn v = Zn (v `Prelude.mod` natValNatural (Proxy :: Proxy n))

-- | Create an element of ℤ/nℤ from a type level Nat
znNat :: forall m n . (KnownNat m, KnownNat n, CmpNat m n ~ 'LT) => Proxy m -> Zn n
znNat m = Zn (natValNatural m)
