{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Foundation.Check.Gen
    ( Gen
    , runGen
    , GenParams(..)
    , genRng
    , genWithRng
    , genWithParams
    ) where

import           Foundation.Primitive.Imports
import           Foundation.Collection
import           Foundation.Random
import           Foundation.String
import           Foundation.Numerical
import           Foundation.Hashing.SipHash
import           Foundation.Hashing.Hasher
import qualified Foundation.Array.Unboxed as A

data GenParams = GenParams
    { genMaxSizeIntegral :: Word -- maximum number of bytes
    , genMaxSizeArray    :: Word -- number of elements, as placeholder
    , genMaxSizeString   :: Word -- maximum number of chars
    }

newtype GenRng = GenRng RNGv1

type GenSeed = Word64

genRng :: GenSeed -> [String] -> (Word64 -> GenRng)
genRng seed groups = \iteration -> genRngNewNoFail $ A.unsafeRecast $ fromList [w1,w2,w3,iteration]
  where
    w1 = rngSeed
    w2 = rngSeed * 2
    w3 = rngSeed * 4

    (SipHash rngSeed) = hashEnd $ hashMixBytes hashData iHashState
    hashData = toBytes UTF8 $ intercalate "::" groups
    iHashState :: Sip1_3
    iHashState = hashNewParam (SipKey seed 0x12345678)

genRngNewNoFail :: A.UArray Word8 -> GenRng
genRngNewNoFail = maybe (error "impossible") GenRng . randomNewFrom

genGenerator :: GenRng -> (GenRng, GenRng)
genGenerator (GenRng rng) =
    let (newSeed, rngNext) = randomGenerate 32 rng
     in (genRngNewNoFail newSeed, GenRng rngNext)

-- | Generator monad
newtype Gen a = Gen { runGen :: GenRng -> GenParams -> a }

instance Functor Gen where
    fmap f g = Gen (\rng params -> f (runGen g rng params))

instance Applicative Gen where
    pure a     = Gen (\_ _ -> a)
    fab <*> fa = Gen $ \rng params ->
        let (r1,r2) = genGenerator rng
            ab      = runGen fab r1 params
            a       = runGen fa r2 params
         in ab a

instance Monad Gen where
    return a  = Gen (\_ _ -> a)
    ma >>= mb = Gen $ \rng params ->
            let (r1,r2) = genGenerator rng
                a       = runGen ma r1 params
             in runGen (mb a) r2 params

genWithRng :: forall a . (forall randomly . MonadRandom randomly => randomly a) -> Gen a
genWithRng f = Gen $ \(GenRng rng) _ ->
    let (a, _) = withRandomGenerator rng f in a

genWithParams :: (GenParams -> Gen a) -> Gen a
genWithParams f = Gen $ \rng params -> runGen (f params) rng params
