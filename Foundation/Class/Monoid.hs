module Foundation.Class.Monoid
    ( Monoid(..)
    ) where

import           Foundation.Internal.Base hiding ((<>), Monoid(..))
import           Foundation.Class.Semigroup
import qualified Foundation.Boot.List as List

-- | Element with an empty element that is neutral w.r.t
-- to the Semigroup (<>) law
--
-- the following property should hold:
--
-- * mempty <> a = a
-- * a <> mempty = a
--
class Semigroup a => Monoid a where
    -- | Empty element
    mempty  :: a 

    -- | Concat 
    mconcat :: [a] -> a
    mconcat = List.foldr (<>) mempty

instance Semigroup a => Monoid (Maybe a) where
    mempty = Nothing
instance (Monoid a, Monoid b) => Monoid (a,b) where
    mempty = (mempty, mempty)
instance (Monoid a, Monoid b, Monoid c) => Monoid (a,b,c) where
    mempty = (mempty, mempty, mempty)
instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (a,b,c,d) where
    mempty = (mempty, mempty, mempty, mempty)
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) => Monoid (a,b,c,d,e) where
    mempty = (mempty, mempty, mempty, mempty, mempty)
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f) => Monoid (a,b,c,d,e,f) where
    mempty = (mempty, mempty, mempty, mempty, mempty, mempty)
