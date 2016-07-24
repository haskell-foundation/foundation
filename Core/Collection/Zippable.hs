module Core.Collection.Zippable
    ( Zippable(..)
    ) where

import           Core.Collection.Element
import           Core.Collection.Sequential
import           Core.Internal.Base

class Sequential col => Zippable col where

  zip :: (Sequential a, Sequential b, Element col ~ (Element a, Element b))
      => a -> b -> col
  zip = zipWith (,)

  zipWith :: (Sequential a, Sequential b, Element col ~ c)
          => (Element a -> Element b -> c) -> a -> b -> col
  zipWith f a b
      | null a || null b = mempty
      | otherwise        =
          let Just (ha, tas) = uncons a
              Just (hb, tbs) = uncons b
          in f ha hb `cons` zipWith f tas tbs

instance Zippable [c] where
