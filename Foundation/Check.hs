{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Foundation.Check
    ( Gen
    , Arbitrary(..)
    , oneof
    , elements
    , frequency
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
import           Foundation.Random
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

-- | return the number of tests runned and the result
runProp :: Context -> String -> Property -> IO (PropertyResult, Word64)
runProp ctx s prop = iterProp 1
  where
    nbTests = 100
    iterProp :: Word64 -> IO (PropertyResult, Word64)
    iterProp i
        | i == nbTests = return (PropertySuccess, i)
        | otherwise    = do
            r <- toResult i
            case r of
                (PropertyFailed e, _)               -> return (PropertyFailed e, i)
                (PropertySuccess, cont) | cont      -> iterProp (i+1)
                                        | otherwise -> return (PropertySuccess, i)
    toResult :: Word64 -> IO (PropertyResult, Bool)
    toResult it =
                (propertyToResult <$> evaluate (runGen (unProp prop) (rngIt it) params))
        `catch` (\(e :: SomeException) -> return (PropertyFailed (fromList $ show e), False))

    propertyToResult p =
        let args   = getArgs p
            checks = getChecks p
         in if checkHasFailed checks
                then printError args checks
                else (PropertySuccess, length args > 0)

    printError args checks = (PropertyFailed (mconcat $ loop 1 args), False)
      where
        loop :: Word -> [String] -> [String]
        loop _ []      = printChecks checks
        loop !i (a:as) = "parameter " <> fromList (show i) <> " : " <> a <> "\n" : loop (i+1) as
    printChecks (PropertyBinaryOp True name _ _)  = []
    printChecks (PropertyBinaryOp False name a b) = [name <> " checked fail\n" <> "   left: " <> a <> "\n" <> "  right: " <> b]
    printChecks (PropertyNamed True _)            = []
    printChecks (PropertyNamed False name)        = ["Check " <> name <> " failed"]
    printChecks (PropertyBoolean True)            = []
    printChecks (PropertyBoolean False)           = ["Check failed"]
    printChecks (PropertyFail _ e)                = ["Check failed: " <> e]
    printChecks (PropertyAnd True _ _)            = []
    printChecks (PropertyAnd False a1 a2)
        | checkHasFailed a1 && checkHasFailed a2  = ["And Property failed:\n    && left: "] <> printChecks a1 <> ["\n"] <> ["   && right: "] <> printChecks a2
        | checkHasFailed a1                       = ["And Property failed:\n    && left: "] <> printChecks a1 <> ["\n"]
        | otherwise                               = ["And Property failed:\n   && right: "] <> printChecks a2 <> ["\n"]

    getArgs (PropertyArg a p) = a : getArgs p
    getArgs (PropertyEOA _) = []

    getChecks (PropertyArg _ p) = getChecks p
    getChecks (PropertyEOA c  ) = c

    !rngIt  = genRng (contextSeed ctx) (s : contextGroups ctx)
    !params = GenParams { genMaxSizeIntegral = 32   -- 256 bits maximum numbers
                        , genMaxSizeArray    = 512  -- 512 elements
                        , genMaxSizeString   = 8192 -- 8K string
                        }

-- | Run tests
defaultMain :: Test -> IO ()
defaultMain test = do
    -- parse arguments
    --let arguments = [ "seed", "j" ]

    -- generate a new seed
    seed <- getRandomPrimType

    let context = Context { contextLevel  = 0
                          , contextGroups = []
                          , contextSeed   = seed
                          }

    printHeader context
    tr <- runTest context test
    if nbFail tr > 0
        then putStrLn (fromList (show $ nbFail tr) <> " failure(s)") >> exitFailure
        else putStrLn "Success" >> exitSuccess
  where
    printHeader ctx = do
        putStrLn ("seed: " <> fromList (show (contextSeed ctx))) -- TODO hexadecimal

    runTest :: Context -> Test -> IO TestResult
    runTest ctx (Group s l) = do
        printIndent ctx s
        results <- mapM (runTest (appendContext s ctx)) l
        return $ GroupResult s (foldl' (+) 0 $ fmap nbFail results) results
    runTest ctx (Property name prop) = do
        (res, nbTests) <- runProp ctx name (property prop)
        case res of
            PropertySuccess  -> printIndent ctx $ "[  OK   ]   " <> name <> " (" <> fromList (show nbTests) <> " completed)"
            PropertyFailed e -> printIndent ctx $ "[ ERROR ]   " <> name <> " after " <> fromList (show (nbTests-1)) <> " tests\n" <> e
        return (PropertyResult name nbTests res)

    runTest _ (Unit _ _) = do
        error "not implemented"

    printIndent ctx s = putStrLn (replicate (contextLevel ctx) ' ' <> s)
