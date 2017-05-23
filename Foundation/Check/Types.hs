-- |
-- Module      : Foundation.Check.Types
-- License     : BSD-style
-- Maintainer  : Foundation maintainers
--
-- A implementation of a test framework
-- and property expression & testing
--
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Foundation.Check.Types
    ( Test(..)
    , testName
    , fqTestName
    , groupHasSubGroup
    ) where

import           Foundation.Primitive.Imports
import           Foundation.Collection
import           Foundation.Check.Property

-- | different type of tests supported
data Test where
    -- Unit test
    Unit     :: String -> IO () -> Test
    -- Property test
    Property :: IsProperty prop => String -> prop -> Test
    -- Multiples tests grouped together
    Group    :: String -> [Test] -> Test

-- | Name of a test
testName :: Test -> String
testName (Unit s _)     = s
testName (Property s _) = s
testName (Group s _)    = s

fqTestName :: [String] -> String
fqTestName = intercalate "/" . reverse

groupHasSubGroup :: [Test] -> Bool
groupHasSubGroup [] = False
groupHasSubGroup (Group{}:_) = True
groupHasSubGroup (_:xs) = groupHasSubGroup xs
