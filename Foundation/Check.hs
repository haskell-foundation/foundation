-- |
-- Module      : Foundation.Check
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
module Foundation.Check
    ( Gen
    , Arbitrary(..)
    , oneof
    , elements
    , frequency
    , between
    -- test
    , Test(..)
    , testName
    -- * Property
    , PropertyCheck
    , Property(..)
    , IsProperty(..)
    , (===)
    , propertyCompare
    , propertyAnd
    , propertyFail
    , forAll
    -- * Check Plan
    , Check
    , validate
    , pick
    , iterateProperty
    ) where

import           Foundation.Primitive.Imports
import           Foundation.Check.Gen
import           Foundation.Check.Arbitrary
import           Foundation.Check.Property
import           Foundation.Check.Types
import           Foundation.Check.Print
import           Foundation.Monad
import           Foundation.Monad.State
import           Foundation.Numerical
import           Control.Exception (evaluate, SomeException)

validate :: IsProperty prop => String -> prop -> Check ()
validate propertyName prop = Check $ do
    (genrng, params) <- withState $ \st -> ( (planRng st, planParams st)
                                           , st { planValidations = planValidations st + 1 }
                                           )
    (r,nb) <- liftIO $ iterateProperty 100 params genrng (property prop)
    case r of
        PropertySuccess        -> return ()
        PropertyFailed failMsg -> do
            withState $ \st -> ((), st { planFailures = PropertyResult propertyName nb (PropertyFailed failMsg) : planFailures st })
            return ()

pick :: String -> IO a -> Check a
pick _ io = Check $ do
    -- TODO catch most exception to report failures
    r <- liftIO $ io
    pure r

iterateProperty :: Word64 ->  GenParams -> (Word64 -> GenRng) -> Property -> IO (PropertyResult, Word64)
iterateProperty limit genParams genRngIter prop = iterProp 1
  where
    iterProp !iter
      | iter == limit = return (PropertySuccess, iter)
      | otherwise  = do
          r <- liftIO $ toResult
          case r of
              (PropertyFailed e, _)               -> return (PropertyFailed e, iter)
              (PropertySuccess, cont) | cont      -> iterProp (iter+1)
                                      | otherwise -> return (PropertySuccess, iter)
        where
          -- TODO revisit to let through timeout and other exception like ctrl-c or thread killing.
          toResult :: IO (PropertyResult, Bool)
          toResult = (propertyToResult <$> evaluate (runGen (unProp prop) (genRngIter iter) genParams))
            `catch` (\(e :: SomeException) -> return (PropertyFailed (show e), False))
