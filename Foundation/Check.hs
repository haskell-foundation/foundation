{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Foundation.Check
    ( Gen
    , Arbitrary(..)
    -- test
    , Test(..)
    , testName
    -- * Property
    , Property(..)
    , IsProperty(..)
    , (===)
    -- * As Program
    , defaultMain
    ) where

import           Foundation.Internal.Base
import           Foundation.Internal.Natural
import           Foundation.Collection.Mappable
import           Foundation.Random
import           Foundation.Numerical
import           Foundation.String
import           Foundation.IO.Terminal
import           Foundation.Check.Gen
import           Foundation.Check.Arbitrary
import           Foundation.Check.Property

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

data Context = Context
    { contextLevel  :: !Word
    , contextGroups :: [String]
    , contextSeed   :: !Word64
    }

appendContext :: String -> Context -> Context
appendContext s ctx = ctx
    { contextLevel = 1 + contextLevel ctx
    , contextGroups = s : contextGroups ctx
    }

data TestResult = TestResult

runProp :: Context -> String -> Property -> IO TestResult
runProp ctx s prop = do
    let v = runGen (unProp prop) rng params
    return $ TestResult
  where
    !rng    = genRng (contextSeed ctx) (s : contextGroups ctx)
    !params = GenParams {}

-- | Run tests
defaultMain :: Test -> IO ()
defaultMain test = do
    -- parse arguments
    --let arguments = [ "seed", "j" ]
    let seed = 10

    let context = Context { contextLevel  = 0
                          , contextGroups = []
                          , contextSeed   = seed
                          }
    printHeader context
    runTest context test
  where
    printHeader ctx = do
        putStrLn ("seed: " <> fromList (show (contextSeed ctx))) -- TODO hexadecimal

    runTest :: Context -> Test -> IO ()
    runTest ctx (Group s l) = do
        putStrLn s
        mapM_ (runTest (appendContext s ctx)) l
        return ()
    runTest ctx (Property name prop) = do
        v <- runProp ctx name (property prop)
        return ()

    runTest _ (Unit _ _) = do
        return ()
