{-# LANGUAGE DefaultSignatures #-}
module Core.Collection.InnerFunctor
    ( InnerFunctor(..)
    ) where

import Core.Internal.Base
import Core.Collection.Element
import qualified Core.Vector.Unboxed as UV

-- | A monomorphic functor that maps the inner values to values of the same type
class InnerFunctor c where
    imap :: (Element c -> Element c) -> c -> c
    default imap :: (Functor f, Element (f a) ~ a, f a ~ c) => (a -> a) -> f a -> f a
    imap = fmap

instance InnerFunctor [a]

instance UV.PrimType ty => InnerFunctor (UV.UVector ty) where
    imap = UV.map
