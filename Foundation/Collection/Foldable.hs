-- |
-- Module      : Foundation.Primitive.Foldable
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- A mono-morphic re-thinking of the Foldable class
--
module Foundation.Collection.Foldable
    ( Foldable(..)
    ) where

import           Foundation.Internal.Base
import           Foundation.Collection.Element
import qualified Data.List
import qualified Foundation.Array.Unboxed as UV
import qualified Foundation.Primitive.Block as BLK
import qualified Foundation.Array.Boxed as BA

-- | Give the ability to fold a collection on itself
class Foldable collection where
    -- | Left-associative fold of a structure.
    --
    -- In the case of lists, foldl, when applied to a binary operator, a starting value (typically the left-identity of the operator), and a list, reduces the list using the binary operator, from left to right:
    --
    -- > foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
    -- Note that to produce the outermost application of the operator the entire input list must be traversed. This means that foldl' will diverge if given an infinite list.
    --
    -- Note that Foundation only provides `foldl'`, a strict version of `foldl` because
    -- the lazy version is seldom useful.

    -- | Left-associative fold of a structure with strict application of the operator.
    foldl' :: (a -> Element collection -> a) -> a -> collection -> a

    -- | Right-associative fold of a structure.
    --
    -- > foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)
    foldr :: (Element collection -> a -> a) -> a -> collection -> a

    -- | Right-associative fold of a structure, but with strict application of the operator.
    foldr' :: (Element collection -> a -> a) -> a -> collection -> a
    foldr' f z0 xs = foldl' f' id xs z0 where f' k x z = k $! f x z

----------------------------
-- Foldable instances
----------------------------

instance Foldable [a] where
    foldr = Data.List.foldr
    foldl' = Data.List.foldl'

instance UV.PrimType ty => Foldable (UV.UArray ty) where
    foldr = UV.foldr
    foldl' = UV.foldl'
instance Foldable (BA.Array ty) where
    foldr = BA.foldr
    foldl' = BA.foldl'
instance UV.PrimType ty => Foldable (BLK.Block ty) where
    foldr = BLK.foldr
    foldl' = BLK.foldl'
