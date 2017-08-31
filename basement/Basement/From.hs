{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MagicHash             #-}
-- |
-- Module      : Basement.From
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
--
-- Flexible Type convertion
--
-- From is multi parameter type class that allow converting
-- from a to b.
--
-- Only type that are valid to convert to another type
-- should be From instance; otherwise TryFrom should be used.
--
-- Into (resp TryInto) allows the contrary instances to be able
-- to specify the destination type before the source. This is
-- practical with TypeApplication
module Basement.From
    ( From(..)
    , Into
    , TryFrom(..)
    , TryInto
    , into
    , tryInto
    ) where

import Basement.Compat.Base

-- basic instances
import           GHC.Types
import           GHC.Prim
import           GHC.Int
import           GHC.Word
import qualified Basement.Block as Block
import qualified Basement.BoxedArray as BoxArray
import qualified Basement.UArray as UArray
import qualified Basement.String as String
import qualified Basement.Types.AsciiString as AsciiString
import           Basement.These
import           Basement.PrimType (PrimType)
import           Basement.Types.OffsetSize
import           Basement.Compat.Natural
import qualified Prelude (fromIntegral)

-- nat instances
#if __GLASGOW_HASKELL__ >= 800
import           Basement.Nat
import qualified Basement.BlockN as BlockN
import           Basement.Bounded
#endif

-- | Class of things that can be converted from a to b.
--
-- In a valid instance, the source should be always representable by the destination,
-- otherwise the instance should be using 'TryFrom'
class From a b where
    from :: a -> b

type Into b a = From a b

-- | Same as from but reverse the type variable so that the destination type can be specified first
--
-- e.g. converting:
--
-- from @_ @Word (10 :: Int)
--
-- into @Word (10 :: Int)
--
into :: Into b a => a -> b
into = from

-- | Class of things that can mostly be converted from a to b, but with possible error cases.
class TryFrom a b where
    tryFrom :: a -> Maybe b

type TryInto b a = TryFrom a b

-- | same as tryFrom but reversed
tryInto :: TryInto b a => a -> Maybe b
tryInto = tryFrom

instance From a a where
    from = id

-- Simple numerical instances
instance From Int Word where
    from (I# i) = W# (int2Word# i)
instance From Word Int where
    from (W# w) = I# (word2Int# w)

instance From Word8 Natural where
    from = Prelude.fromIntegral
instance From Word16 Natural where
    from = Prelude.fromIntegral
instance From Word32 Natural where
    from = Prelude.fromIntegral
instance From Word64 Natural where
    from = Prelude.fromIntegral
instance From Word Natural where
    from = Prelude.fromIntegral

-- Simple prelude types
instance From (Maybe a) (Either () a) where
    from (Just x) = Right x
    from Nothing  = Left ()

-- basic basement types
instance From (CountOf ty) Int where
    from (CountOf n) = n
instance From (CountOf ty) Word where
    from (CountOf n) = from n

instance From (Either a b) (These a b) where
    from (Left a) = This a
    from (Right b) = That b

-- basement instances

-- uarrays
instance PrimType ty => From (Block.Block ty) (UArray.UArray ty) where
    from = UArray.fromBlock
instance PrimType ty => From (BoxArray.Array ty) (UArray.UArray ty) where
    from = BoxArray.mapToUnboxed id

-- blocks
instance PrimType ty => From (UArray.UArray ty) (Block.Block ty) where
    from = UArray.toBlock
instance PrimType ty => From (BoxArray.Array ty) (Block.Block ty) where
    from = UArray.toBlock . BoxArray.mapToUnboxed id

-- boxed array
instance PrimType ty => From (UArray.UArray ty) (BoxArray.Array ty) where
    from = BoxArray.mapFromUnboxed id


instance From String.String (UArray.UArray Word8) where
    from = String.toBytes String.UTF8

instance From AsciiString.AsciiString String.String where
    from = String.fromBytesUnsafe . UArray.unsafeRecast . AsciiString.toBytes
instance From AsciiString.AsciiString (UArray.UArray Word8) where
    from = UArray.unsafeRecast . AsciiString.toBytes

instance TryFrom (UArray.UArray Word8) String.String where
    tryFrom arr = case String.fromBytes String.UTF8 arr of
                    (s, Nothing, _) -> Just s
                    (_, Just _, _)  -> Nothing

#if __GLASGOW_HASKELL__ >= 800
instance From (BlockN.BlockN n ty) (Block.Block ty) where
    from = BlockN.toBlock
instance (NatWithinBound Int n, PrimType ty) => From (BlockN.BlockN n ty) (UArray.UArray ty) where
    from = UArray.fromBlock . BlockN.toBlock
instance (NatWithinBound Int n, PrimType ty) => From (BlockN.BlockN n ty) (BoxArray.Array ty) where
    from = BoxArray.mapFromUnboxed id . UArray.fromBlock . BlockN.toBlock

instance (NatWithinBound (CountOf ty) n, KnownNat n, PrimType ty)
      => TryFrom (Block.Block ty) (BlockN.BlockN n ty) where
    tryFrom = BlockN.toBlockN
instance (NatWithinBound (CountOf ty) n, KnownNat n, PrimType ty)
      => TryFrom (UArray.UArray ty) (BlockN.BlockN n ty) where
    tryFrom = BlockN.toBlockN . UArray.toBlock
instance (NatWithinBound (CountOf ty) n, KnownNat n, PrimType ty)
      => TryFrom (BoxArray.Array ty) (BlockN.BlockN n ty) where
    tryFrom = BlockN.toBlockN . UArray.toBlock . BoxArray.mapToUnboxed id

instance From (Zn64 n) Word64 where
    from = unZn64
instance (KnownNat n, NatWithinBound Word64 n) => From (Zn n) (Zn64 n) where
    from = zn64 . Prelude.fromIntegral . unZn
instance KnownNat n => From (Zn64 n) (Zn n) where
    from = zn . from . unZn64
#endif
