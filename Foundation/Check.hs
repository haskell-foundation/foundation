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

import qualified Prelude (fromIntegral, read)
import           Foundation.Internal.Base
import           Foundation.Class.Bifunctor (bimap)
import           Foundation.System.Info (os, OS(..))
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
import           System.Environment (getArgs)

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

groupHasSubGroup :: [Test] -> Bool
groupHasSubGroup [] = False
groupHasSubGroup (Group{}:_) = True
groupHasSubGroup (_:xs) = groupHasSubGroup xs

data PropertyResult =
      PropertySuccess
    | PropertyFailed  String
    deriving (Show,Eq)

data TestResult =
      PropertyResult String Word64      PropertyResult
    | GroupResult    String HasFailures [TestResult]
    deriving (Show)

type HasFailures = Word64

nbFail :: TestResult -> HasFailures
nbFail (PropertyResult _ _ (PropertyFailed _)) = 1
nbFail (PropertyResult _ _ PropertySuccess)    = 0
nbFail (GroupResult    _ t _)                  = t

nbTests :: TestResult -> Word64
nbTests (PropertyResult _ t _) = t
nbTests (GroupResult _ _ l) = foldl' (+) 0 $ fmap nbTests l

parseArgs :: [[Char]] -> Config -> Config
parseArgs [] cfg = cfg
parseArgs ("--seed":[])     _  = error "option `--seed' is missing a parameter"
parseArgs ("--seed":x:xs)  cfg = parseArgs xs $ cfg { getSeed = Prelude.read x }
parseArgs ("--tests":[])   _   = error "option `--tests' is missing a parameter"
parseArgs ("--tests":x:xs) cfg = parseArgs xs $ cfg { numTests = Prelude.read x }
parseArgs ("--quiet":xs)   cfg = parseArgs xs $ cfg { displayOptions = DisplayTerminalErrorOnly }
parseArgs ("--verbose":xs) cfg = parseArgs xs $ cfg { displayOptions = DisplayTerminalVerbose }
parseArgs ("--help":_)     _   = error $ mconcat
    [ "--seed <seed>: the seed to use to generate arbitrary value.\n"
    , "--tests <tests>: the number of tests to perform for every property tests.\n"
    , "--quiet: print only the errors to the standard output\n"
    , "--verbose: print every property tests to the stand output.\n"
    ]
parseArgs (x:_) _ = error $ "unknown parameter: " <> show x

-- | Run tests
defaultMain :: Test -> IO ()
defaultMain t = do
    -- generate a new seed
    seed <- getRandomPrimType
    -- parse arguments
    cfg <- flip parseArgs (defaultConfig seed) <$> getArgs

    putStrLn $ "\nSeed: " <> fromList (show $ getSeed cfg) <> "\n"

    (_, cfg') <- runStateT (runCheck $ test t) cfg
    let oks = testPassed cfg'
        kos = testFailed cfg'
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
    , displayOptions :: !DisplayOption
    }

data DisplayOption
    = DisplayTerminalErrorOnly
    | DisplayGroupOnly
    | DisplayTerminalVerbose
  deriving (Eq, Ord, Enum, Bounded, Show)

onDisplayOption :: DisplayOption -> Check () -> Check ()
onDisplayOption opt chk = do
    on <- (<=) opt . displayOptions <$> get
    if on then chk else return ()

whenErrorOnly :: Check () -> Check ()
whenErrorOnly = onDisplayOption DisplayTerminalErrorOnly

whenGroupOnly :: Check () -> Check ()
whenGroupOnly = onDisplayOption DisplayGroupOnly

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
    , displayOptions = DisplayGroupOnly
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
    r'@(PropertyResult _ nb r) <- testProperty name (property prop)
    case r of
        PropertySuccess  -> whenVerbose $ displayPropertySucceed name nb
        PropertyFailed w -> whenErrorOnly $ displayPropertyFailed name nb w
    return r'

displayCurrent :: String -> Check ()
displayCurrent name = do
    i <- indent <$> get
    liftIO $ putStrLn $ replicate i ' ' <> name

displayPropertySucceed :: String -> Word64 -> Check ()
displayPropertySucceed name nb = do
    i <- indent <$> get
    liftIO $ putStrLn $ mconcat
        [ replicate i ' '
        , successString, name
        , " ("
        , fromList $ show nb
        , if nb == 1 then " test)" else " tests)"
        ]

successString :: String
successString = case os of
    Right Linux -> " ✓ "
    Right OSX   -> " ✓ "
    _           -> "[SUCCESS]"
{-# NOINLINE successString #-}

failureString :: String
failureString = case os of
    Right Linux -> " ✗ "
    Right OSX   -> " ✗ "
    _           -> "[ ERROR ]"
{-# NOINLINE failureString #-}

displayPropertyFailed :: String -> Word64 -> String -> Check ()
displayPropertyFailed name nb w = do
    seed <- getSeed <$> get
    i <- indent <$> get
    liftIO $ do
        putStrLn $ mconcat
          [ replicate i ' '
          , failureString, name
          , " failed after "
          , fromList $ show nb
          , if nb == 1 then " test" else " tests:"
          ]
        putStrLn $ replicate i ' ' <> "   use param: --seed " <> fromList (show seed)
        putStrLn w

pushGroup :: String -> [Test] -> Check TestResult
pushGroup name list = do
    whenGroupOnly $ if groupHasSubGroup list then displayCurrent name else return ()
    withState $ \s -> ((), s { testPath = push (testPath s) name, indent = indent s + 2 })
    results <- mapM test list
    withState $ \s -> ((), s { testPath = pop (testPath s), indent = indent s - 2 })
    let totFail = foldl' (+) 0 $ fmap nbFail results
        tot = foldl'(+) 0 $ fmap nbTests results
    whenGroupOnly $ case (groupHasSubGroup list, totFail) of
        (True, _)              -> return ()
        (False, n) | n > 0     -> displayPropertyFailed name n ""
                   | otherwise -> displayPropertySucceed name tot
    return $ GroupResult name totFail results
  where
    push = snoc
    pop = maybe mempty fst . unsnoc

testProperty :: String -> Property -> Check TestResult
testProperty name prop = do
    seed <- getSeed <$> get
    path <- testPath <$> get
    let rngIt = genRng seed (name : toList path)

    maxTests <- numTests <$> get

    (res, nb) <- iterProp 1 maxTests rngIt
    return (PropertyResult name nb res)
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
    printChecks (PropertyBinaryOp False n a b) =
        [ "Property `a " <> n <> " b' failed where:\n"
        , "    a = " <> a <> "\n"
        , "        " <> bl1 <> "\n"
        , "    b = " <> b <> "\n"
        , "        " <> bl2 <> "\n"
        ]
      where
        (bl1, bl2) = diffBlame a b
    printChecks (PropertyNamed True _)            = []
    printChecks (PropertyNamed False e)           = ["Property " <> e <> " failed"]
    printChecks (PropertyBoolean True)            = []
    printChecks (PropertyBoolean False)           = ["Property failed"]
    printChecks (PropertyFail _ e)                = ["Property failed: " <> e]
    printChecks (PropertyAnd True _ _)            = []
    printChecks (PropertyAnd False a1 a2) =
            [ "Property `cond1 && cond2' failed where:\n"
            , "   cond1 = " <> h1 <> "\n"

            ]
            <> ((<>) "           " <$>  hs1)
            <>
            [ "   cond2 = " <> h2 <> "\n"
            ]
            <> ((<>) "           " <$> hs2)
      where
        (h1, hs1) = f a1
        (h2, hs2) = f a2
        f a = case printChecks a of
                      [] -> ("Succeed", [])
                      (x:xs) -> (x, xs)

    getArgs (PropertyArg a p) = a : getArgs p
    getArgs (PropertyEOA _) = []

    getChecks (PropertyArg _ p) = getChecks p
    getChecks (PropertyEOA c  ) = c

diffBlame :: String -> String -> (String, String)
diffBlame a b = bimap fromList fromList $ go ([], []) (toList a) (toList b)
  where
    go (acc1, acc2) [] [] = (acc1, acc2)
    go (acc1, acc2) l1 [] = (acc1 <> blaming (length l1), acc2)
    go (acc1, acc2) [] l2 = (acc1                       , acc2 <> blaming (length l2))
    go (acc1, acc2) (x:xs) (y:ys)
        | x == y    = go (acc1 <> " ", acc2 <> " ") xs ys
        | otherwise = go (acc1 <> "^", acc2 <> "^") xs ys
    blaming n = replicate (Prelude.fromIntegral n) '^'
