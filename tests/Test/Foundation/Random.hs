{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Test.Foundation.Random
    ( testRandom
    ) where

import Imports
import Foundation
import Foundation.Primitive
import Foundation.Array
import Foundation.Collection
import Foundation.System.Entropy
import Foundation.Random
import Control.Monad
import qualified Prelude
import qualified Data.List
import GHC.ST

testRandom :: TestTree
testRandom = testGroup "random"
    [ testProperty "entropy" entropyCheck
    , testProperty "rngv1" rngv1Check
    ]
  where
    entropyCheck = monadicIO $ do
        v <- randomTest <$> run (getEntropy 1024)
        --run (putStrLn . fromList $ show v)

        unless (res_entropy v > 6.5 && res_entropy v <= 8) (failInfo v)
        unless (res_mean v >= 112 && res_mean v <= 144) (failInfo v)
        unless (res_compressionPercent v >= 0 && res_compressionPercent v <= 5.0) (failInfo v)

    rngv1Check = monadicIO $ do
        rng         <- run (randomNew :: IO RNG)
        --nbQueries  <- pick arbitrary
        let (l, _) = withRandomGenerator rng $ do
                mapM getRandomBytes [1,2,4,8,32,80,250,2139]
            v = randomTest (mconcat l)
        unless (res_entropy v > 6.5 && res_entropy v <= 8) (failInfo v)
        unless (res_mean v >= 112 && res_mean v <= 144) (failInfo v)
        unless (res_compressionPercent v >= 0 && res_compressionPercent v <= 5.0) (failInfo v)
        return ()

    failInfo v = do
        fail $ toList
             ("randomness assert failed: entropy=" <> show (res_entropy v)
                                      <> " chi^2=" <> show (res_chi_square v)
                                       <> " mean=" <> show (res_mean v)
                               <> " compression%=" <> show (res_compressionPercent v))

-------- generic random testing

data RandomTestResult = RandomTestResult
    { res_totalChars         :: Word64 -- ^ Total number of characters
    , res_entropy            :: Double -- ^ Entropy per byte
    , res_chi_square         :: Double -- ^ Chi Square
    , res_mean               :: Double -- ^ Arithmetic Mean
    , res_compressionPercent :: Double -- ^ Theorical Compression percent
    , res_probs              :: [Double] -- ^ Probability of every bucket
    } deriving (Show,Eq)

-- | Mutable random test State
newtype RandomTestState s = RandomTestState (MUArray Word64 (PrimState (ST s)))

-- | Initialize new state to run tests
randomTestInitialize :: ST s (RandomTestState s)
randomTestInitialize = do
    m <- mutNew 256
    forM_ [0..255] $ \i -> mutWrite m i 0
    return $ RandomTestState m

-- | Append random data to the test state
randomTestAppend :: RandomTestState s -> UArray Word8 -> ST s ()
randomTestAppend (RandomTestState buckets) = mapM_ (addVec 1 . fromIntegral) . toList
  where
    addVec a i = mutRead buckets i >>= \d -> mutWrite buckets i $! d+a

-- | Finalize random test state into some result
randomTestFinalize :: RandomTestState s -> ST s RandomTestResult
randomTestFinalize (RandomTestState buckets) = (calculate . toList) <$> freeze buckets

randomTest :: UArray Word8 -> RandomTestResult
randomTest a = runST $ do
    st <- randomTestInitialize
    randomTestAppend st a
    randomTestFinalize st

calculate :: [Word64] -> RandomTestResult
calculate buckets = RandomTestResult
    { res_totalChars = totalChars
    , res_entropy    = entropy
    , res_chi_square = chisq
    , res_mean       = fromIntegral datasum Prelude./ fromIntegral totalChars
    , res_compressionPercent = 100.0 * (8 - entropy) Prelude./ 8.0
    , res_probs      = probs
    }
  where totalChars = Prelude.sum buckets
        probs = fmap (\v -> fromIntegral v Prelude./ fromIntegral totalChars :: Double) buckets
        entropy = Data.List.foldl' accEnt 0.0 probs
        cexp    = fromIntegral totalChars Prelude./ 256.0 :: Double
        (datasum, chisq) = foldl' accMeanChi (0, 0.0) $ Prelude.zip [0..255] buckets
        --chip' = abs (sqrt (2.0 * chisq) - sqrt (2.0 * 255.0 - 1.0))

        accEnt ent pr
            | pr > 0.0  = ent + (pr * xlog (1 Prelude./ pr))
            | otherwise = ent
        xlog v = Prelude.logBase 10 v * (Prelude.log 10 Prelude./ Prelude.log 2)

        accMeanChi :: (Word64, Double) -> (Int, Word64) -> (Word64, Double)
        accMeanChi (dataSum, chiSq) (i, ccount) =
            let a      = fromIntegral ccount - cexp
             in (dataSum + fromIntegral i * ccount, chiSq + (a * a Prelude./ cexp))
