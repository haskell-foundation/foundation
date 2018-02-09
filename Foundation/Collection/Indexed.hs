-- |
-- Module      : Foundation.Array.Indexed
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
{-# LANGUAGE FlexibleInstances #-}
module Foundation.Collection.Indexed
    ( IndexedCollection(..)
    ) where

import           Basement.Compat.Base
import           Basement.Numerical.Additive ((+))
import           Basement.Types.OffsetSize
import           Foundation.Collection.Element
import qualified Data.List
import qualified Basement.Block as BLK
import qualified Basement.UArray as UV
import qualified Basement.BoxedArray as BA
import qualified Basement.Exception as A
import qualified Basement.String as S

-- | Collection of elements that can indexed by int
class IndexedCollection c where
    (!) :: c -> Offset (Element c) -> Maybe (Element c)
    findIndex :: (Element c -> Bool) -> c -> Maybe (Offset (Element c))

instance IndexedCollection [a] where
    (!) l (Offset n)
        | n < 0     = Nothing
        | otherwise = case Data.List.drop n l of
                        []  -> Nothing
                        x:_ -> Just x
    findIndex predicate = fmap Offset . Data.List.findIndex predicate

instance UV.PrimType ty => IndexedCollection (BLK.Block ty) where
    (!) l n
        | A.isOutOfBound n (BLK.length l) = Nothing
        | otherwise                       = Just $ BLK.index l n
    findIndex predicate c = loop 0
      where
        !len = BLK.length c
        loop i
            | i .==# len                      = Nothing
            | predicate (BLK.unsafeIndex c i) = Just i
            | otherwise                       = loop (i + 1)

instance UV.PrimType ty => IndexedCollection (UV.UArray ty) where
    (!) l n
        | A.isOutOfBound n (UV.length l) = Nothing
        | otherwise                          = Just $ UV.index l n
    findIndex predicate c = loop 0
      where
        !len = UV.length c
        loop i
            | i .==# len                     = Nothing
            | predicate (UV.unsafeIndex c i) = Just i
            | otherwise                      = Nothing

instance IndexedCollection (BA.Array ty) where
    (!) l n
        | A.isOutOfBound n (BA.length l) = Nothing
        | otherwise                          = Just $ BA.index l n
    findIndex predicate c = loop 0
      where
        !len = BA.length c
        loop i
            | i .==# len = Nothing
            | otherwise  =
                if predicate (BA.unsafeIndex c i) then Just i else Nothing

instance IndexedCollection S.String where
    (!) = S.index
    findIndex = S.findIndex
