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
import           Foundation.Monad.State
import           Foundation.List.DList
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

-- | Run tests
defaultMain :: Test -> IO ()
defaultMain t = do
    -- parse arguments
    --let arguments = [ "seed", "j" ]

    -- generate a new seed
    seed <- getRandomPrimType

    (_, cfg) <- runStateT (runCheck $ test t) $ defaultConfig seed
    let oks = testPassed cfg
        kos = testFailed cfg
        tot = oks + kos
    if kos > 0
        then do
          putStrLn $ "Failed " <> fromList (show kos) <> " out of " <> fromList (show tot)
          exitFailure
        else do
          putStrLn $ "Succeed " <> fromList (show oks) <> " test(s)"
          exitSuccess

-- | internal check monad for facilitating the tests traversal
newtype Check a = Check { runCheck :: StateT Config IO a }
  deriving (Functor, Applicative, Monad, MonadIO)
instance MonadState Check where
    type State Check = Config
    withState = Check . withState

type Seed = Word64
data Config = Config
    { testPath     :: !(DList String)
        -- ^ for internal use when pretty printing
    , indent       :: !Word
        -- ^ for internal use when pretty printing
    , testPassed   :: !Word
    , testFailed   :: !Word
    , getSeed      :: !Seed
        -- ^ the seed for the tests
    , getGenParams :: !GenParams
        -- ^ Parameters for the generator
        --
        -- default:
        --   * 32bits long numbers;
        --   * array of 512 elements max;
        --   * string of 8192 bytes max.
        --
    , numTests     :: !Word64
        -- ^ the number of tests to perform on every property.
        --
        -- default: 100
    , displayOptions :: ![DisplayOption]
    }

data DisplayOption
    = DisplayTerminalErrorOnly
    | DisplayTerminalVerbose
  deriving (Eq, Show)

onDisplayOption :: DisplayOption -> Check () -> Check ()
onDisplayOption opt chk = do
    on <- elem opt . displayOptions <$> get
    if on then chk else return ()

whenErrorOnly :: Check () -> Check ()
whenErrorOnly = onDisplayOption DisplayTerminalErrorOnly

whenVerbose :: Check () -> Check ()
whenVerbose = onDisplayOption DisplayTerminalVerbose

passed :: Check ()
passed = withState $ \s -> ((), s { testPassed = testPassed s + 1 })

failed :: Check ()
failed = withState $ \s -> ((), s { testFailed = testFailed s + 1 })

-- | create the default configuration
--
-- see @Config@ for details
defaultConfig :: Seed -> Config
defaultConfig s = Config
    { testPath     = mempty
    , indent       = 0
    , testPassed   = 0
    , testFailed   = 0
    , getSeed      = s
    , getGenParams = params
    , numTests     = 100
    , displayOptions = [DisplayTerminalErrorOnly]
    }
  where
    params = GenParams
        { genMaxSizeIntegral = 32   -- 256 bits maximum numbers
        , genMaxSizeArray    = 512  -- 512 elements
        , genMaxSizeString   = 8192 -- 8K string
        }

test :: Test -> Check TestResult
test (Group s l) = pushGroup s l
test (Unit _ _) = undefined
test (Property name prop) = do
    whenVerbose $ displayCurrentProperty name
    r'@(PropertyResult _ _ r) <- testProperty name (property prop)
    whenErrorOnly $ case r of
        PropertySuccess  -> return ()
        PropertyFailed w -> displayPropertyFailed name w
    return r'

displayCurrentProperty :: String -> Check ()
displayCurrentProperty name = do
    path <- toList . testPath <$> get
    liftIO $ putStrLn $ intercalate "::" $ path <> [name]

displayPropertyFailed :: String -> String -> Check ()
displayPropertyFailed name w = do
    path <- toList . testPath <$> get
    liftIO $ do
        putStrLn $ (intercalate "::" $ path <> [name]) <> " ERROR"
        putStrLn w

pushGroup :: String -> [Test] -> Check TestResult
pushGroup name list = do
    withState $ \s -> ((), s { testPath = push (testPath s) name, indent = indent s + 1 })
    results <- mapM test list
    withState $ \s -> ((), s { testPath = pop (testPath s), indent = indent s - 1 })
    return $ GroupResult name (foldl' (+) 0 $ fmap nbFail results) results
  where
    push = snoc
    pop c = case unsnoc c of
        Nothing     -> mempty
        Just (c, _) -> c

testProperty :: String -> Property -> Check TestResult
testProperty name prop = do
    seed <- getSeed <$> get
    path <- testPath <$> get
    let rngIt = genRng seed (name : toList path)

    maxTests <- numTests <$> get

    (res, nbTests) <- iterProp 1 maxTests rngIt
    return (PropertyResult name nbTests res)
  where
    iterProp !n !limit !rngIt
      | n == limit = passed >> return (PropertySuccess, n)
      | otherwise  = do
          params <- getGenParams <$> get
          r <- liftIO $ toResult n params
          case r of
              (PropertyFailed e, _)               -> failed >> return (PropertyFailed e, n)
              (PropertySuccess, cont) | cont      -> iterProp (n+1) limit rngIt
                                      | otherwise -> passed >> return (PropertySuccess, n)
        where
          toResult :: Word64 -> GenParams -> IO (PropertyResult, Bool)
          toResult it params =
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
    printChecks (PropertyBinaryOp True _ _ _)     = []
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
