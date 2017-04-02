{-# LANGUAGE ScopedTypeVariables #-}
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
import           Foundation.Collection
import           Foundation.Numerical
import           Foundation.String
import           Foundation.IO.Terminal
import           Foundation.Check.Gen
import           Foundation.Check.Arbitrary
import           Foundation.Check.Property
import           Foundation.Monad
import           Control.Exception (evaluate, SomeException)
import           System.Exit

-- different type of tests
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

data PropertyResult =
      PropertySuccess
    | PropertyFailed  String
    deriving (Show,Eq)

data TestResult =
      PropertyResult String Word64      PropertyResult
    | GroupResult    String HasFailures [TestResult]
    deriving (Show)

type HasFailures = Word

nbFail :: TestResult -> HasFailures
nbFail (PropertyResult _ _ (PropertyFailed _)) = 1
nbFail (PropertyResult _ _ PropertySuccess)    = 0
nbFail (GroupResult    _ t _)                  = t

runProp :: Context -> String -> Property -> IO TestResult
runProp ctx s prop = do
    (\(e, i) -> PropertyResult s i e) <$> iterProp 0
  where
    nbTests = 100
    iterProp :: Word64 -> IO (PropertyResult, Word64)
    iterProp i
        | i == nbTests = return (PropertySuccess, nbTests)
        | otherwise    = do
            r <- toResult i
            case r of
                PropertyFailed e -> return (PropertyFailed e, i)
                PropertySuccess  -> iterProp (i+1)
    toResult :: Word64 -> IO PropertyResult
    toResult it =
                (propertyToResult <$> evaluate (runGen (unProp prop) (rngIt it) params))
        `catch` (\(e :: SomeException) -> return $ PropertyFailed (fromList $ show e))

    propertyToResult False = PropertyFailed "property failed"
    propertyToResult True  = PropertySuccess

    !rngIt  = genRng (contextSeed ctx) (s : contextGroups ctx)
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

    tr <- runTest context test
    if nbFail tr > 0
        then putStrLn (fromList (show $ nbFail tr) <> " failure(s)") >> exitFailure
        else putStrLn "Success" >> exitSuccess
  where
    printHeader ctx = do
        putStrLn ("seed: " <> fromList (show (contextSeed ctx))) -- TODO hexadecimal

    runTest :: Context -> Test -> IO TestResult
    runTest ctx (Group s l) = do
        putStrLn s
        results <- mapM (runTest (appendContext s ctx)) l
        return $ GroupResult s (foldl' (+) 0 $ fmap nbFail results) results
    runTest ctx (Property name prop) = do
        v <- runProp ctx name (property prop)
        putStrLn $ fromList (show v)
        return v

    runTest _ (Unit _ _) = do
        error "not implemented"
