{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
module Foundation.Check
    ( Gen
    , Arbitrary(..)
    -- test
    , Test(..)
    -- * Property
    , Property(..)
    , IsProperty(..)
    , (===)
    -- * As Program
    , defaultMain
    ) where

import           Foundation.Internal.Base
import           Foundation.Internal.Natural
import           Foundation.Random
import           Foundation.String
import           Foundation.IO.Terminal

data GenParams = GenParams
    {
    }

newtype GenRng = GenRng RNGv1

split :: GenRng -> (GenRng, GenRng)
split (GenRng rng) = do
    -- TODO implement actual split
    let (_, rng1) = randomGenerate 32 rng
    let (_, rng2) = randomGenerate 32 rng1
     in (GenRng rng1, GenRng rng2)

-- | Generator monad
newtype Gen a = Gen { unGen :: GenRng -> GenParams -> a }

instance Functor Gen where
    fmap f g = Gen (\rng params -> f (unGen g rng params))

instance Applicative Gen where
    pure a     = Gen (\_ _ -> a)
    fab <*> fa = undefined

instance Monad Gen where
    return a  = Gen (\_ _ -> a)
    ma >>= mb = Gen $ \rng params ->
            let (r1,r2) = split rng
                a       = unGen ma r1 params
             in unGen (mb a) r2 params

-- | How to generate an arbitrary value for 'a'
class Arbitrary a where
    arbitrary :: Gen a

arbitraryBounded :: Bounded b => Gen b
arbitraryBounded = undefined

instance Arbitrary Int where
    arbitrary = return 10
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

------------

-- different type of tests
data Test where
    -- | Unit test
    Unit     :: String -> IO () -> Test
    -- | Property test
    Property :: IsProperty prop => String -> prop -> Test
    -- | Multiples tests grouped together
    Group    :: String -> [Test] -> Test

-- | Name of a test
testName :: Test -> String
testName (Unit s _)     = s
testName (Property s _) = s
testName (Group s _)    = s

type Context = (Word, [String])

-- | Run tests
defaultMain :: Test -> IO ()
defaultMain test = do
    -- parse arguments
    runTest (0, []) test
  where
    runTest :: Context -> Test -> IO ()
    runTest (lvl, stk) (Group s l) = do
        putStrLn s
        mapM_ (runTest (lvl+1, s:stk)) l
        return ()
    runTest _ (Property _ _) = do
        return ()
    runTest _ (Unit _ _) = do
        return ()
