module Foundation.Check.Property
    ( Property(..)
    , IsProperty
    , property
    -- * Properties
    , forAll
    , (===)
    ) where

import Foundation.Internal.Base
import Foundation.Check.Gen
import Foundation.Check.Arbitrary

class IsProperty p where
    property :: p -> Property

instance IsProperty Bool where
    property b = Prop (pure b)
instance IsProperty Property where
    property p = p -- (Prop result) = Prop . return $ result
instance IsProperty prop => IsProperty (Gen prop) where
    property mp = Prop (mp >>= \p -> unProp (property p))
instance (Arbitrary a, IsProperty prop) => IsProperty (a -> prop) where
    property p = forAll arbitrary p

data Property = Prop { unProp :: Gen Bool }

forAll :: IsProperty prop => Gen a -> (a -> prop) -> Property
forAll generator tst = undefined

(===) :: Eq a => a -> a -> Property
(===) a b = Prop (pure (a == b))
infix 4 ===
