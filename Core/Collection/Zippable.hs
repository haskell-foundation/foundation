module Core.Collection.Zippable
    ( Zippable(..)
    ) where

import qualified Core.Array.Unboxed as UV
import qualified Core.Array.Unboxed.Mutable as MUV
import           Core.Collection.Element
import           Core.Collection.Iterable
import           Core.Collection.Sequential
import           Core.Internal.Base
import           Core.Internal.Types
import           Core.Number
import           GHC.ST

class Sequential col => Zippable col where

  -- | 'zip' takes two collections and returns a collections of corresponding
  --   pairs. If one input collection is short, excess elements of the longer
  --   collection are discarded.
  zip :: ( Iterable a, Iterable b
         , Element col ~ (Element a, Element b) )
      => a -> b -> col
  zip = zipWith (,)

  -- | 'zipWith' generalises 'zip' by zipping with the function given as the
  --   first argument, instead of a tupling function. For example, @'zipWith' (+)@
  --   is applied to two collections to produce the collection of corresponding
  --   sums.
  zipWith :: (Iterable a, Iterable b)
          => (Element a -> Element b -> Element col)
          -> a -> b -> col
  zipWith f a b = go f (initialState a) (initialState b)
    where
      go :: (Iterable a, Iterable b, Sequential col)
         => (Element a -> Element b -> Element col)
         -> State a -> State b -> col
      go f' sa sb
          | hasNext sa && hasNext sb =
              let Just (nextSa, elemA) = next sa
                  Just (nextSb, elemB) = next sb
              in f' elemA elemB `cons` go f' nextSa nextSb
          | otherwise                  = mempty

  -- | @unzip@ transforms a collection of pairs into a collection of first
  --   components and a collection of second components.
  unzip :: (Iterable a, Element a ~ (Element col, Element col2), col ~ col2)
        => a -> (col, col2)
  unzip xs = go (initialState xs) mempty mempty
    where
      go :: (Sequential col2, Iterable a, Element a ~ (Element col, Element col2), col ~ col2)
         => State a -> col -> col2 -> (col, col2)
      go s accA accB
        | hasNext s =
            let Just (nextS, (elemA, elemB)) = next s
                (accA', accB') = go nextS accA accB
            in (elemA `cons` accA', elemB `cons` accB')
        | otherwise = (mempty, mempty)

instance Zippable [c]

instance UV.PrimType ty => Zippable (UV.UArray ty) where
  zipWith f a b = runST $ do
      mv <- MUV.new (Size len)
      go mv 0 f (initialState a) (initialState b)
      UV.unsafeFreeze mv
    where
      !len = min (length a) (length b)
      go mv i f' sa sb
          | hasNext sa && hasNext sb = do
              let Just (nextSa, elemA) = next sa
                  Just (nextSb, elemB) = next sb
              MUV.write mv i (f' elemA elemB)
              go mv (i + 1) f' nextSa nextSb
          | otherwise                    = return ()
