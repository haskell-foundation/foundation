module Foundation.Check.Arbitrary
    ( Arbitrary(..)
    ) where

import           Foundation.Internal.Base
import           Foundation.Internal.Natural
import           Foundation.Collection.Mappable
import           Foundation.Random
import           Foundation.Numerical
import           Foundation.String
import           Foundation.IO.Terminal
import           Foundation.Check.Gen

-- | How to generate an arbitrary value for 'a'
class Arbitrary a where
    arbitrary :: Gen a

arbitraryBounded :: Bounded b => Gen b
arbitraryBounded = undefined

instance Arbitrary Int where
    arbitrary = return 10
instance Arbitrary Integer where
    arbitrary = undefined
instance Arbitrary Natural where
    arbitrary = undefined
instance Arbitrary Word64 where
    arbitrary = arbitraryBounded
instance Arbitrary Word32 where
    arbitrary = arbitraryBounded
instance Arbitrary Word16 where
    arbitrary = arbitraryBounded
instance Arbitrary Word8 where
    arbitrary = arbitraryBounded
instance Arbitrary Int64 where
    arbitrary = arbitraryBounded
instance Arbitrary Int32 where
    arbitrary = arbitraryBounded
instance Arbitrary Int16 where
    arbitrary = arbitraryBounded
instance Arbitrary Int8 where
    arbitrary = arbitraryBounded
instance Arbitrary Char where
    arbitrary = undefined
instance Arbitrary Bool where
    arbitrary = undefined

--instance Arbitrary a => Arbitrary (Maybe a) where

instance (Arbitrary a, Arbitrary b)
    => Arbitrary (a,b) where
    arbitrary = (,) <$> arbitrary <*> arbitrary
instance (Arbitrary a, Arbitrary b, Arbitrary c)
    => Arbitrary (a,b,c) where
    arbitrary = (,,) <$> arbitrary <*> arbitrary <*> arbitrary
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
    => Arbitrary (a,b,c,d) where
    arbitrary = (,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e)
    => Arbitrary (a,b,c,d,e) where
    arbitrary = (,,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e, Arbitrary f)
    => Arbitrary (a,b,c,d,e,f) where
    arbitrary = (,,,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
