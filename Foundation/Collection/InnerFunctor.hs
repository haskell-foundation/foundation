{-# LANGUAGE DefaultSignatures #-}
module Foundation.Collection.InnerFunctor
    ( InnerFunctor(..)
    ) where

import Foundation.Internal.Base
import Foundation.Collection.Element
import qualified Foundation.Primitive.String as S
import qualified Foundation.Primitive.UArray as UV
import           Foundation.Primitive.BoxedArray (Array)

-- | A monomorphic functor that maps the inner values to values of the same type
class InnerFunctor c where
    imap :: (Element c -> Element c) -> c -> c
    default imap :: (Functor f, Element (f a) ~ a, f a ~ c) => (Element c -> Element c) -> c -> c
    imap = fmap

instance InnerFunctor [a]

instance UV.PrimType ty => InnerFunctor (UV.UArray ty) where
    imap = UV.map

instance InnerFunctor (Array ty)

instance InnerFunctor S.String where
    imap = S.charMap
