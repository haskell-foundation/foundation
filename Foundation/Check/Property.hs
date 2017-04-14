{-# LANGUAGE OverloadedStrings #-}
module Foundation.Check.Property
    ( Property(..)
    , PropertyTestArg(..)
    , PropertyTestCheck(..)
    , PropertyTestResult(..)
    , IsProperty
    , PropertyCheck
    , property
    -- * Properties
    , forAll
    , (===)
    , propertyCompare
    , propertyAnd
    , propertyFail
    ) where

import Foundation.Internal.Base
import Foundation.Check.Gen
import Foundation.Check.Arbitrary
import Foundation.String

-- | The type of check this test did for a property
data PropertyTestCheck = PropertyBoolean
                       | PropertyBinaryOp String String String
                       | PropertyAnd PropertyTestCheck PropertyTestCheck
                       | PropertyFail String

-- | A linked-list of arguments to this test
data PropertyTestArg = PropertyEOA PropertyTestCheck
                     -- | PropertyAnd PropertyTestArg PropertyTestArg
                     | PropertyArg String PropertyTestArg

-- | Whether this property test failed or succeed
data PropertyTestResult = PropertyTestFailed | PropertyTestSuccess
    deriving (Show,Eq)

data PropertyCheck = PropertyCheck PropertyTestResult PropertyTestCheck

data Property = Prop { unProp :: Gen (PropertyTestResult, PropertyTestArg) }

class IsProperty p where
    property :: p -> Property

boolToTestResult :: Bool -> PropertyTestResult
boolToTestResult True  = PropertyTestSuccess
boolToTestResult False = PropertyTestFailed

testResultAnd :: PropertyTestResult -> PropertyTestResult -> PropertyTestResult
testResultAnd PropertyTestFailed  _ = PropertyTestFailed
testResultAnd PropertyTestSuccess v = v

instance IsProperty Bool where
    property b = Prop $ pure (boolToTestResult b, PropertyEOA PropertyBoolean)
instance IsProperty PropertyCheck where
    property (PropertyCheck r tc) = Prop $ pure (r, PropertyEOA tc)
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

(===) :: (Show a, Eq a) => a -> a -> PropertyCheck
(===) a b = -- Prop (pure (a == b, show a `==` show b )
    let sa = fromList (show a)
        sb = fromList (show b)
     in PropertyCheck (boolToTestResult (a == b)) (PropertyBinaryOp "==" sa sb)
infix 4 ===

propertyCompare :: Show a
                => String  -- ^ name of the function used for comparaison, e.g. (<)
                -> (a -> a -> Bool) -- ^ function used for value comparaison
                -> a                -- ^ value left of the operator
                -> a                -- ^ value right of the operator
                -> PropertyCheck
propertyCompare name op a b =
    let sa = fromList (show a)
        sb = fromList (show b)
     in PropertyCheck (boolToTestResult (a `op` b)) (PropertyBinaryOp name sa sb)

propertyAnd :: PropertyCheck -> PropertyCheck -> PropertyCheck
propertyAnd (PropertyCheck r1 t1) (PropertyCheck r2 t2) =
    PropertyCheck (r1 `testResultAnd` r2) (PropertyAnd t1 t2)

propertyFail :: String -> PropertyCheck
propertyFail = PropertyCheck PropertyTestFailed . PropertyFail
