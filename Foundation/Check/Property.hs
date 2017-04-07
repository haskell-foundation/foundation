module Foundation.Check.Property
    ( Property(..)
    , PropertyTestArg(..)
    , PropertyTestCheck(..)
    , PropertyTestResult(..)
    , IsProperty
    , property
    -- * Properties
    , forAll
    , (===)
    ) where

import Foundation.Internal.Base
import Foundation.Check.Gen
import Foundation.Check.Arbitrary
import Foundation.String

-- | The type of check this test did for a property
data PropertyTestCheck = PropertyBoolean
                       | PropertyEquality String String

-- | A linked-list of arguments to this test
data PropertyTestArg = PropertyEOA PropertyTestCheck
                     | PropertyArg String PropertyTestArg

-- | Whether this property test failed or succeed
data PropertyTestResult = PropertyTestFailed | PropertyTestSuccess
    deriving (Show,Eq)

data Property = Prop { unProp :: Gen (PropertyTestResult, PropertyTestArg) }

class IsProperty p where
    property :: p -> Property

boolToTestResult :: Bool -> PropertyTestResult
boolToTestResult True  = PropertyTestSuccess
boolToTestResult False = PropertyTestFailed

instance IsProperty Bool where
    property b = Prop (pure (boolToTestResult b, PropertyEOA PropertyBoolean))
instance IsProperty Property where
    property p = p
instance (Show a, Arbitrary a, IsProperty prop) => IsProperty (a -> prop) where
    property p = forAll arbitrary p

forAll :: (Show a, IsProperty prop) => Gen a -> (a -> prop) -> Property
forAll generator tst = Prop $ do
    a <- generator
    augment a <$> unProp (property (tst a))
  where
    augment a (tr, arg) = (tr, PropertyArg (fromList $ show a) arg)

(===) :: (Show a, Eq a) => a -> a -> Property
(===) a b = -- Prop (pure (a == b, show a `==` show b )
    let sa = fromList (show a)
        sb = fromList (show b)
     in Prop $ pure (boolToTestResult (a == b), PropertyEOA $ PropertyEquality sa sb)
infix 4 ===
