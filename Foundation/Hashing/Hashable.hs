-- |
-- Module      : Foundation.Hashing.Hashable
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : good
--
-- provide the SipHash algorithm.
-- reference: <http://131002.net/siphash/siphash.pdf>
--
module Foundation.Hashing.Hashable
    ( Hashable(..)
    ) where

import Foundation.Internal.Base
import Foundation.Array
import Foundation.Tuple
import Foundation.String
import Foundation.Collection
import Foundation.Hashing.Hasher

-- | Type with the ability to be hashed
class Hashable a where
    hashMix :: Hasher st => a -> st -> st

-- specific type instances
instance Hashable Word8 where
    hashMix w = hashMix8 w
instance Hashable Word16 where
    hashMix w = hashMix16 w
instance Hashable Word32 where
    hashMix w = hashMix32 w
instance Hashable Word64 where
    hashMix w = hashMix64 w
-- TODO : add Int{8,16,24,32}, Int, Word, Integer

instance Hashable String where
    hashMix s = hashMixBytes (toBytes UTF8 s)

-- collection type instances
instance PrimType a => Hashable (UArray a) where
    hashMix ba = hashMixBytes ba
--instance Hashable a => Hashable (Array a) where
--    hashMix arr st = foldl' (flip hashMix) st arr

-- combined instances
instance Hashable a => Hashable [a] where
    hashMix ba st = foldl' (flip hashMix) st ba

instance (Hashable a, Hashable b) => Hashable (a,b) where
    hashMix (a,b) = hashMix b . hashMix a
instance (Hashable a, Hashable b, Hashable c) => Hashable (a,b,c) where
    hashMix (a,b,c) = hashMix c . hashMix b . hashMix a
instance (Hashable a, Hashable b, Hashable c, Hashable d) => Hashable (a,b,c,d) where
    hashMix (a,b,c,d) = hashMix d . hashMix c . hashMix b . hashMix a
instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e) => Hashable (a,b,c,d,e) where
    hashMix (a,b,c,d,e) = hashMix e . hashMix d . hashMix c . hashMix b . hashMix a
instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f) => Hashable (a,b,c,d,e,f) where
    hashMix (a,b,c,d,e,f) = hashMix f . hashMix e . hashMix d . hashMix c . hashMix b . hashMix a
instance (Hashable a, Hashable b) => Hashable (Tuple2 a b) where
    hashMix (Tuple2 a b) = hashMix b . hashMix a
instance (Hashable a, Hashable b, Hashable c) => Hashable (Tuple3 a b c) where
    hashMix (Tuple3 a b c) = hashMix c . hashMix b . hashMix a
instance (Hashable a, Hashable b, Hashable c, Hashable d) => Hashable (Tuple4 a b c d) where
    hashMix (Tuple4 a b c d) = hashMix d . hashMix c . hashMix b . hashMix a
{-
instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e) => Hashable (Tuple5 a b c d e) where
    hashMix (Tuple5 a b c d e) = hashMix e . hashMix d . hashMix c . hashMix b . hashMix a
instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f) => Hashable (Tuple6 a b c d e f) where
    hashMix (Tuple6 a b c d e f) = hashMix f . hashMix e . hashMix d . hashMix c . hashMix b . hashMix a
-}
