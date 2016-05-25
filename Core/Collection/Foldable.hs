module Core.Collection.Foldable
    ( Foldable(..)
    ) where

import           Core.Internal.Base
import           Core.Collection.Element
import qualified Data.List

class Foldable collection where
    -- | Left-associative fold of a structure.
    --
    -- In the case of lists, foldl, when applied to a binary operator, a starting value (typically the left-identity of the operator), and a list, reduces the list using the binary operator, from left to right:
    --
    -- > foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
    -- Note that to produce the outermost application of the operator the entire input list must be traversed. This means that foldl' will diverge if given an infinite list.
    --
    -- Also note that if you want an efficient left-fold, you probably want to use foldl' instead of foldl. The reason for this is that latter does not force the "inner" results (e.g. z f x1 in the above example) before applying them to the operator (e.g. to (f x2)). This results in a thunk chain O(n) elements long, which then must be evaluated from the outside-in.
    foldl :: (a -> Element collection -> a) -> a -> collection -> a


    -- | Left-associative fold of a structure but with strict application of the operator.
    foldl' :: (a -> Element collection -> a) -> a -> collection -> a

    -- | Right-associative fold of a structure.
    --
    -- > foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)
    foldr :: (Element collection -> a -> a) -> a -> collection -> a

    -- | Right-associative fold of a structure, but with strict application of the operator.
    foldr' :: (Element collection -> a -> a) -> a -> collection -> a
    foldr' f z0 xs = foldl f' id xs z0 where f' k x z = k $! f x z

----------------------------
-- Foldable instances
----------------------------

instance Foldable [a] where
    foldl = Data.List.foldl
    foldr = Data.List.foldr
    foldl' = Data.List.foldl'
