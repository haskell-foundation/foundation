module Test.Data.List
    ( generateListOfElement
    , generateListOfElementMaxN
    ) where

import Test.Tasty.QuickCheck
import Control.Monad

-- | convenient function to replicate thegiven Generator of `e` a randomly
-- choosen amount of time.
generateListOfElement :: Gen e -> Gen [e]
generateListOfElement = generateListOfElementMaxN 100

-- | convenient function to generate up to a certain amount of time the given
-- generator.
generateListOfElementMaxN :: Int -> Gen e -> Gen [e]
generateListOfElementMaxN n e = choose (0,n) >>= flip replicateM e
