module Foundation.Class.Semigroup
    ( Semigroup(..)
    ) where

import Foundation.Internal.Base hiding ((<>))

infixr 6 <>

class Semigroup a where
    (<>) :: a -> a -> a

instance Semigroup a => Semigroup (Maybe a) where
    Nothing <> b       = b
    a       <> Nothing = a
    Just a  <> Just b  = Just (a <> b)

instance (Semigroup left, Semigroup right) => Semigroup (Either left right) where
    Left _ <> b = b
    a      <> _ = a

instance Semigroup Ordering where
    LT <> _ = LT
    EQ <> x = x
    GT <> _ = GT

instance (Semigroup a, Semigroup b) => Semigroup (a,b) where
    (<>) (a1,b1) (a2,b2) = (a1 <> a2, b1 <> b2)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (a,b,c) where
    (<>) (a1,b1,c1) (a2,b2,c2) = (a1 <> a2, b1 <> b2, c1 <> c2)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (a,b,c,d) where
    (<>) (a1,b1,c1,d1) (a2,b2,c2,d2) = (a1 <> a2, b1 <> b2, c1 <> c2, d1 <> d2)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e) => Semigroup (a,b,c,d,e) where
    (<>) (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) = (a1 <> a2, b1 <> b2, c1 <> c2, d1 <> d2, e1 <> e2)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e, Semigroup f) => Semigroup (a,b,c,d,e,f) where
    (<>) (a1,b1,c1,d1,e1,f1) (a2,b2,c2,d2,e2,f2) = (a1 <> a2, b1 <> b2, c1 <> c2, d1 <> d2, e1 <> e2, f1 <> f2)
