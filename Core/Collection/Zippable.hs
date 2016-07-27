-- |
-- Module      : Core.Collection.Zippable
-- License     : BSD-style
-- Maintainer  : foundation
-- Stability   : experimental
-- Portability : portable
--
-- Common functions (e. g. 'zip', 'zipWith') that are useful for combining
-- multiple collections.
--
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Core.Collection.Zippable
    ( BoxedZippable(..)
    , Zippable(..)
    ) where

import qualified Core.Array.Unboxed as UV
import qualified Core.Array.Unboxed.Builder as UVB
import           Core.Collection.Element
import           Core.Collection.Sequential
import           Core.Internal.Base
import           Core.Number
import qualified Prelude
import           GHC.ST

class Sequential col => Zippable col where

  -- | 'zipWith' generalises 'zip' by zipping with the function given as the
  --   first argument, instead of a tupling function. For example, @'zipWith' (+)@
  --   is applied to two collections to produce the collection of corresponding
  --   sums.
  zipWith :: (Sequential a, Sequential b)
          => (Element a -> Element b -> Element col)
          -> a -> b -> col
  zipWith f a b = go f (toList a) (toList b)
    where
      go _  []       _        = mempty
      go _  _        []       = mempty
      go f' (a':as') (b':bs') = f' a' b' `cons` go f' as' bs'

  -- | 'unzip' transforms a collection of pairs into a collection of first
  --   components and a collection of second components.
  unzip :: (Sequential a, Sequential b, Element col ~ (Element a, Element b))
        => col -> (a, b)
  unzip = go . toList
    where
      go []          = (mempty, mempty)
      go ((a, b):xs) =
          let (as, bs) = go xs
          in (a `cons` as, b `cons` bs)

instance Zippable [c]

instance UV.PrimType ty => Zippable (UV.UArray ty) where
  zipWith f as bs = runST $
      Prelude.uncurry UVB.build $ go f (toList as) (toList bs)
    where
      go _  []       _        = (0, return ())
      go _  _        []       = (0, return ())
      go f' (a':as') (b':bs') =
          let (i, builder) = go f' as' bs'
          in (i + 1, UVB.appendTy (f' a' b') >> builder)

class Zippable col => BoxedZippable col a b where

  -- | 'zip' takes two collections and returns a collections of corresponding
  --   pairs. If one input collection is short, excess elements of the longer
  --   collection are discarded.
  zip :: ( Sequential a, Sequential b
         , Element col ~ (Element a, Element b) )
        => a -> b -> col
  zip = zipWith (,)

instance ( Sequential a, Sequential b
         , Zippable col, Element col ~ (Element a, Element b))
        => BoxedZippable col a b
