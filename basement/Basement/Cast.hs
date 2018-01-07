{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Basement.Cast
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
--
module Basement.Cast
    ( Cast(..)
    ) where

import           Basement.Compat.Base

import           GHC.Types
import           GHC.Prim
import           GHC.Int
import           GHC.Word
import           Basement.Numerical.Number
import           Basement.Numerical.Conversion
import qualified Basement.Block.Base as Block
import qualified Basement.BoxedArray as BoxArray
import qualified Basement.UArray as UArray
import qualified Basement.String as String
import qualified Basement.Types.AsciiString as AsciiString
import           Basement.Types.Word128 (Word128(..))
import           Basement.Types.Word256 (Word256(..))
import qualified Basement.Types.Word128 as Word128
import qualified Basement.Types.Word256 as Word256
import           Basement.These
import           Basement.PrimType (PrimType)
import           Basement.Types.OffsetSize
import           Basement.Compat.Natural
import qualified Prelude (fromIntegral)

-- | `Cast` an object of type a to b.
--
-- Do not add instance of this class if the source type is not of the same
-- size of the destination type. Also keep in mind this is casting a value
-- of a given type into a destination type. The value won't be changed to
-- fit the destination represention.
--
-- If you wish to convert a value of a given type into another type, look at
-- `From` and `TryFrom`.
--
-- @
-- cast (-10 :: Int) :: Word === 18446744073709551606
-- @
--
class Cast source destination where
    cast :: source -> destination

instance Cast Int Word where
    cast (I# i) = W# (int2Word# i)
instance Cast Word Int where
    cast (W# w) = I# (word2Int# w)

-- Int casts

instance Cast Int8 Int16 where
    cast (I8# i) = I16# i
instance Cast Int8 Int32 where
    cast (I8# i) = I32# i
instance Cast Int8 Int64 where
    cast (I8# i) = intToInt64 (I# i)
instance Cast Int8 Int where
    cast (I8# i) = I# i

instance Cast Int16 Int32 where
    cast (I16# i) = I32# i
instance Cast Int16 Int64 where
    cast (I16# i) = intToInt64 (I# i)
instance Cast Int16 Int where
    cast (I16# i) = I# i

instance Cast Int32 Int64 where
    cast (I32# i) = intToInt64 (I# i)
instance Cast Int32 Int where
    cast (I32# i) = I# i

instance Cast Int Int64 where
    cast = intToInt64

-- Word casts

instance Cast Word8 Word16 where
    cast (W8# i) = W16# i
instance Cast Word8 Word32 where
    cast (W8# i) = W32# i
instance Cast Word8 Word64 where
    cast (W8# i) = wordToWord64 (W# i)
instance Cast Word8 Word128 where
    cast (W8# i) = Word128 0 (wordToWord64 $ W# i)
instance Cast Word8 Word256 where
    cast (W8# i) = Word256 0 0 0 (wordToWord64 $ W# i)
instance Cast Word8 Word where
    cast (W8# i) = W# i
instance Cast Word8 Int8 where
    cast (W8# w) = I8# (word2Int# w)
instance Cast Word8 Int16 where
    cast (W8# w) = I16# (word2Int# w)
instance Cast Word8 Int32 where
    cast (W8# w) = I32# (word2Int# w)
instance Cast Word8 Int64 where
    cast (W8# w) = intToInt64 (I# (word2Int# w))
instance Cast Word8 Int where
    cast (W8# w) = I# (word2Int# w)

instance Cast Word16 Word32 where
    cast (W16# i) = W32# i
instance Cast Word16 Word64 where
    cast (W16# i) = wordToWord64 (W# i)
instance Cast Word16 Word128 where
    cast (W16# i) = Word128 0 (wordToWord64 $ W# i)
instance Cast Word16 Word256 where
    cast (W16# i) = Word256 0 0 0 (wordToWord64 $ W# i)
instance Cast Word16 Word where
    cast (W16# i) = W# i
instance Cast Word16 Int16 where
    cast (W16# w) = I16# (word2Int# w)
instance Cast Word16 Int32 where
    cast (W16# w) = I32# (word2Int# w)
instance Cast Word16 Int64 where
    cast (W16# w) = intToInt64 (I# (word2Int# w))
instance Cast Word16 Int where
    cast (W16# w) = I# (word2Int# w)

instance Cast Word32 Word64 where
    cast (W32# i) = wordToWord64 (W# i)
instance Cast Word32 Word128 where
    cast (W32# i) = Word128 0 (wordToWord64 $ W# i)
instance Cast Word32 Word256 where
    cast (W32# i) = Word256 0 0 0 (wordToWord64 $ W# i)
instance Cast Word32 Word where
    cast (W32# i) = W# i

instance Cast Word64 Word128 where
    cast w = Word128 0 w
instance Cast Word64 Word256 where
    cast w = Word256 0 0 0 w

instance Cast Word Word64 where
    cast = wordToWord64


instance Cast (Block.Block a) (Block.Block Word8) where
    cast (Block.Block ba) = Block.Block ba
