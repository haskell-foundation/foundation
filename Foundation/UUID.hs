module Foundation.UUID
    ( UUID(..)
    , nil
    ) where

import           Foundation.Internal.Base
import           Foundation.Class.Storable
import           Foundation.Hashing.Hashable
import           Foundation.Bits
import           Foundation.Primitive
import           Foundation.Collection.Sequential
import qualified Prelude

data UUID = UUID {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
    deriving (Eq,Ord,Typeable)
instance Show UUID where
    show = toLString
instance Hashable UUID where
    hashMix (UUID a b) = hashMix a . hashMix b
instance Storable UUID where
    peek p = UUID <$> (fromBE <$> peekOff ptr 0)
                  <*> (fromBE <$> peekOff ptr 1)
      where ptr = castPtr p :: Ptr (BE Word64)
    poke p (UUID a b) = do
        pokeOff ptr 0 (toBE a)
        pokeOff ptr 1 (toBE b)
      where ptr = castPtr p :: Ptr (BE Word64)
instance StorableFixed UUID where
    size      _ = 16
    alignment _ = 8

withComponent :: UUID -> (Word -> Word -> Word -> Word -> Word64 -> a) -> a
withComponent (UUID a b) f = f x1 x2 x3 x4 x5
  where
    !x1 = un64 (a .>>. 32)
    !x2 = un64 ((a .>>. 16) .&. 0xffff)
    !x3 = un64 (a .&. 0xffff)
    !x4 = un64 (b .>>. 48)
    !x5 = (b .&. 0x0000ffffffffffff)

    un64 :: Word64 -> Word
    un64 = Prelude.fromIntegral
{-# INLINE withComponent #-}

toLString :: UUID -> [Char]
toLString uuid = withComponent uuid $ \x1 x2 x3 x4 x5 ->
    intercalate "-" [hexWord_4 x1,hexWord_2 x2,hexWord_2 x3,hexWord_2 x4,hexWord64_6 x5]
  where
    hexWord_2 w = "0000"
    hexWord_4 w = "00000000"
    hexWord64_6 w = "000000000000"

nil :: UUID
nil = UUID 0 0
