{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Foundation.Check.Property
    ( Property(..)
    , PropertyTestArg(..)
    , IsProperty
    , PropertyCheck(..)
    , property
    , checkHasSucceed
    , checkHasFailed
    -- * Properties
    , forAll
    , (===)
    , propertyCompare
    , propertyAnd
    , propertyFail
    ) where

import Foundation.Primitive.Imports
import Foundation.Check.Gen
import Foundation.Check.Arbitrary

type PropertyTestResult = Bool

-- | The type of check this test did for a property
data PropertyCheck = PropertyBoolean  PropertyTestResult
                   | PropertyNamed    PropertyTestResult String
                   | PropertyBinaryOp PropertyTestResult String String String
                   | PropertyAnd      PropertyTestResult PropertyCheck PropertyCheck
                   | PropertyFail     PropertyTestResult String

checkHasSucceed :: PropertyCheck -> PropertyTestResult
checkHasSucceed (PropertyBoolean b)        = b
checkHasSucceed (PropertyNamed b _)        = b
checkHasSucceed (PropertyBinaryOp b _ _ _) = b
checkHasSucceed (PropertyAnd b _ _)        = b
checkHasSucceed (PropertyFail b _)         = b

checkHasFailed :: PropertyCheck -> PropertyTestResult
checkHasFailed = not . checkHasSucceed

-- | A linked-list of arguments to this test
data PropertyTestArg = PropertyEOA PropertyCheck
                     | PropertyArg String PropertyTestArg

data Property = Prop { unProp :: Gen PropertyTestArg }

class IsProperty p where
    property :: p -> Property

instance IsProperty Bool where
    property b = Prop $ pure (PropertyEOA $ PropertyBoolean b)
instance IsProperty (String, Bool) where
    property (name, b) = Prop $ pure (PropertyEOA $ PropertyNamed b name)
instance IsProperty PropertyCheck where
    property check = Prop $ pure (PropertyEOA check)
instance IsProperty Property where
    property p = p
instance (Show a, Arbitrary a, IsProperty prop) => IsProperty (a -> prop) where
    property p = forAll arbitrary p

forAll :: (Show a, IsProperty prop) => Gen a -> (a -> prop) -> Property
forAll generator tst = Prop $ do
    a <- generator
    augment a <$> unProp (property (tst a))
  where
    augment a arg = PropertyArg (show a) arg

(===) :: (Show a, Eq a) => a -> a -> PropertyCheck
(===) a b =
    let sa = show a
        sb = show b
     in PropertyBinaryOp (a == b) "==" sa sb
infix 4 ===

propertyCompare :: Show a
                => String           -- ^ name of the function used for comparaison, e.g. (<)
                -> (a -> a -> Bool) -- ^ function used for value comparaison
                -> a                -- ^ value left of the operator
                -> a                -- ^ value right of the operator
                -> PropertyCheck
propertyCompare name op a b =
    let sa = show a
        sb = show b
     in PropertyBinaryOp (a `op` b) name sa sb

propertyAnd :: PropertyCheck -> PropertyCheck -> PropertyCheck
propertyAnd c1 c2 =
    PropertyAnd (checkHasSucceed c1 && checkHasSucceed c2) c1 c2

propertyFail :: String -> PropertyCheck
propertyFail = PropertyFail False
