{-# LANGUAGE NoImplicitPrelude #-}

module Test.Data.List
    ( generateListOfElement
    , generateListOfElementMaxN
    , generateNonEmptyListOfElement
    , RandomList(..)
    ) where

import Foundation
import Foundation.Primitive
import Foundation.Collection (nonEmpty_, NonEmpty)
import Foundation.Check
import Foundation.Monad

-- | convenient function to replicate thegiven Generator of `e` a randomly
-- choosen amount of time.
generateListOfElement :: Gen e -> Gen [e]
generateListOfElement = generateListOfElementMaxN 100

-- | convenient function to generate up to a certain amount of time the given
-- generator.
generateListOfElementMaxN :: CountOf e -> Gen e -> Gen [e]
generateListOfElementMaxN (CountOf n) e = replicateBetween 0 (integralCast n) e

generateNonEmptyListOfElement :: CountOf e -> Gen e -> Gen (NonEmpty [e])
generateNonEmptyListOfElement (CountOf n) e = nonEmpty_ <$> replicateBetween 1 (integralCast n) e

data RandomList = RandomList [Int]
    deriving (Show,Eq)

instance Arbitrary RandomList where
    arbitrary = RandomList <$> replicateBetween 100 400 (integralCast <$> between (0,8))

replicateBetween n1 n2 f =
    between (n1, n2) >>= \n -> replicateM (CountOf (toInt n)) f
  where
    toInt :: Word -> Int
    toInt = integralCast
