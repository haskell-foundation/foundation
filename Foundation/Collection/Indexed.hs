-- |
-- Module      : Foundation.Array.Indexed
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
module Foundation.Collection.Indexed
    ( IndexedCollection(..)
    ) where

import           Foundation.Internal.Base
import           Foundation.Collection.Element
import qualified Data.List
import qualified Foundation.Array.Unboxed as UV
import qualified Foundation.Array.Boxed as BA
import qualified Foundation.String.UTF8 as S

-- | Collection of elements that can indexed by int
class IndexedCollection c where
    (!) :: c -> Int -> Maybe (Element c)
    findIndex :: (Element c -> Bool) -> c -> Maybe Int

instance IndexedCollection [a] where
    (!) l n
        | n < 0     = Nothing
        | otherwise = case Data.List.drop n l of
                        []  -> Nothing
                        x:_ -> Just x
    findIndex = Data.List.findIndex

instance UV.PrimType ty => IndexedCollection (UV.UArray ty) where
    (!) l n
        | n < 0 || n >= UV.length l = Nothing
        | otherwise                 = Just $ UV.index l n
    findIndex predicate c = loop 0
      where
        !len = UV.length c
        loop i
            | i == len                       = Nothing
            | predicate (UV.unsafeIndex c i) = Just i
            | otherwise                      = Nothing

instance IndexedCollection (BA.Array ty) where
    (!) l n
        | n < 0 || n >= BA.length l = Nothing
        | otherwise                 = Just $ BA.index l n
    findIndex predicate c = loop 0
      where
        !len = BA.length c
        loop i
            | i == len  = Nothing
            | otherwise =
                if predicate (BA.unsafeIndex c i) then Just i else Nothing

instance IndexedCollection S.String where
    (!) = S.index
    findIndex = S.findIndex
