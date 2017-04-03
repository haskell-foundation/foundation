{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
module Foundation.Check.Arbitrary
    ( Arbitrary(..)
    , frequency
    , oneof
    , elements
    , between
    ) where

import           Foundation.Internal.Base
import           Foundation.Internal.Natural
import           Foundation.Primitive
import           Foundation.Check.Gen
import           Foundation.Random
import           Foundation.Collection
import           Foundation.Array
import           Foundation.Numerical
import           Control.Monad (replicateM)

-- | How to generate an arbitrary value for 'a'
class Arbitrary a where
    arbitrary :: Gen a

instance Arbitrary Integer where
    arbitrary = arbitraryInteger
instance Arbitrary Natural where
    arbitrary = arbitraryNatural

-- prim types
instance Arbitrary Int where
    arbitrary = arbitraryPrimtype
instance Arbitrary Word where
    arbitrary = arbitraryPrimtype
instance Arbitrary Word64 where
    arbitrary = arbitraryPrimtype
instance Arbitrary Word32 where
    arbitrary = arbitraryPrimtype
instance Arbitrary Word16 where
    arbitrary = arbitraryPrimtype
instance Arbitrary Word8 where
    arbitrary = arbitraryPrimtype
instance Arbitrary Int64 where
    arbitrary = arbitraryPrimtype
instance Arbitrary Int32 where
    arbitrary = arbitraryPrimtype
instance Arbitrary Int16 where
    arbitrary = arbitraryPrimtype
instance Arbitrary Int8 where
    arbitrary = arbitraryPrimtype
instance Arbitrary Char where
    arbitrary = arbitraryPrimtype

-- other types
instance Arbitrary Bool where
    arbitrary = undefined -- arbitrary

instance Arbitrary String where
    arbitrary = undefined

instance Arbitrary a => Arbitrary (Maybe a) where
    arbitrary = frequency $ nonEmpty_ [ (1, pure Nothing), (4, Just <$> arbitrary) ]

instance (Arbitrary l, Arbitrary r) => Arbitrary (Either l r) where
    arbitrary = oneof $ nonEmpty_ [ Left <$> arbitrary, Right <$> arbitrary ]

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

arbitraryInteger :: Gen Integer
arbitraryInteger =
    -- TODO use the sized parameter
    frequency $ nonEmpty_
        [ (4, integerOfSize True 2)
        , (4, integerOfSize False 2)
        , (4, integerOfSize True 4)
        , (4, integerOfSize False 4)
        , (2, integerOfSize True 8)
        , (2, integerOfSize False 8)
        , (1, integerOfSize True 16)
        , (1, integerOfSize False 16)
        ]
  where
    integerOfSize :: Bool -> Word -> Gen Integer
    integerOfSize toSign n = ((if toSign then (\x -> 0 - x) else id) . foldl (\x y -> x + integralUpsize y) 0 . toList)
                         <$> (arbitraryUArrayOf n :: Gen (UArray Word8))

arbitraryNatural :: Gen Natural
arbitraryNatural = integralDownsize . abs <$> arbitraryInteger

arbitraryPrimtype :: PrimType ty => Gen ty
arbitraryPrimtype = genWithRng getRandomPrimType

arbitraryUArrayOf :: PrimType ty => Word -> Gen (UArray ty)
arbitraryUArrayOf size =
    between (0, size) >>= \sz -> (fromList <$> replicateM (integralCast sz) arbitrary)

-- | Call one of the generator weighted
frequency :: NonEmpty [(Word, Gen a)] -> Gen a
frequency (getNonEmpty -> l) = between (0, sum) >>= pickOne l
  where
    sum :: Word
    !sum = foldl' (+) 0 $ fmap fst l

    pickOne ((k,x):xs) n
        | n <= k    = x
        | otherwise = pickOne xs (n-k)

oneof :: NonEmpty [Gen a] -> Gen a
oneof ne = frequency (nonEmptyFmap (\x -> (1, x)) ne)

elements :: NonEmpty [a] -> Gen a
elements l = frequency (nonEmptyFmap (\x -> (1, pure x)) l)

between :: (Word, Word) -> Gen Word
between (min,max) = (+) min <$> genMax range
  where range = max - min

genMax :: Word -> Gen Word
genMax m = undefined
