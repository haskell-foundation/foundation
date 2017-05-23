{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Foundation.Check
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- A implementation of a test framework
-- and property expression & testing
--
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

import qualified Prelude (fromIntegral)
import           Foundation.Primitive.Imports
import           Foundation.Class.Bifunctor (bimap)
import           Foundation.System.Info (os, OS(..))
import           Foundation.Collection
import           Foundation.Numerical
import           Foundation.IO.Terminal
import           Foundation.Check.Gen
import           Foundation.Check.Arbitrary
import           Foundation.Check.Property
import           Foundation.Check.Config
import           Foundation.List.DList
import           Foundation.Random
import           Foundation.Monad
import           Foundation.Monad.State
import           Control.Exception (evaluate, SomeException)
import           Control.Monad (when)
import           Data.Maybe (catMaybes)
import           System.Exit
import           System.Environment (getArgs)

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

-- | Result of a property run
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

data TestState = TestState
    { config      :: !Config
    , getSeed     :: !Seed
    , indent      :: !Word
    , testPassed  :: !Word
    , testFailed  :: !Word
    , testPath    :: !(DList String)
    }

newState :: Config -> Seed -> TestState
newState cfg initSeed = TestState
    { testPath     = mempty
    , testPassed   = 0
    , testFailed   = 0
    , indent       = 0
    , getSeed      = initSeed
    , config       = cfg
    }

filterTestMatching :: Config -> Test -> Maybe Test
filterTestMatching cfg testRoot
    | null (testNameMatch cfg) = Just testRoot
    | otherwise                = testFilter [] testRoot
  where
    match acc s = or $ fmap (flip isInfixOf currentTestName) $ testNameMatch cfg
      where currentTestName = fqTestName (s:acc)
    or [] = False
    or (x:xs)
        | x == True = True
        | otherwise = or xs

    testFilter acc x =
        case x of
            Group s l    ->
                let filtered = catMaybes $ fmap (testFilter (s:acc)) l
                 in if null filtered then Nothing else Just (Group s filtered)
            Unit s _
                | match acc s -> Just x
                | otherwise   -> Nothing
            Property s _
                | match acc s -> Just x
                | otherwise   -> Nothing

-- | Run tests
defaultMain :: Test -> IO ()
defaultMain allTestRoot = do
    -- parse arguments
    ecfg <- flip parseArgs defaultConfig . fmap fromList <$> getArgs
    cfg  <- case ecfg of
            Left e  -> do
                putStrLn e
                mapM_ putStrLn configHelp
                exitFailure
            Right c -> pure c

    -- use the user defined seed or generate a new seed
    seed <- maybe getRandomPrimType pure $ udfSeed cfg

    let testState = newState cfg seed

    when (helpRequested cfg) (mapM_ putStrLn configHelp >> exitSuccess)
    when (listTests cfg) (printTestName >> exitSuccess)

    putStrLn $ "\nSeed: " <> show seed <> "\n"

    case filterTestMatching cfg allTestRoot of
        Nothing -> putStrLn "no tests to run" >> exitSuccess
        Just t  -> do
            (_, cfg') <- runStateT (runCheck $ test t) testState
            summary cfg'

  where
    -- display a summary of the result and use the right exit code
    summary cfg
        | kos > 0 = do
            putStrLn $ "Failed " <> show kos <> " out of " <> show tot
            exitFailure
        | otherwise = do
            putStrLn $ "Succeed " <> show oks <> " test(s)"
            exitSuccess
      where
        oks = testPassed cfg
        kos = testFailed cfg
        tot = oks + kos

    -- print all the tests recursively
    printTestName = mapM_ (\tst -> putStrLn (fqTestName tst)) $ testCases [] [] [] allTestRoot
      where
        testCases acc xs pre x =
            case x of
                Group s l    -> tToList (fmap (\z -> (z, pre)) xs <> acc) (s:pre) l
                Unit s _     -> (s : pre) : tToList acc pre xs
                Property s _ -> (s : pre) : tToList acc pre xs

        tToList []           _   []              = []
        tToList ((a,pre):as) _   []              = testCases as [] pre a
        tToList acc          pre (x:xs)          = testCases acc xs pre x

-- | internal check monad for facilitating the tests traversal
newtype Check a = Check { runCheck :: StateT TestState IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadState Check where
    type State Check = TestState
    withState = Check . withState

onDisplayOption :: DisplayOption -> Check () -> Check ()
onDisplayOption opt chk = do
    on <- (<=) opt . displayOptions . config <$> get
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
        , show nb
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
          , show nb
          , if nb == 1 then " test" else " tests:"
          ]
        putStrLn $ replicate i ' ' <> "   use param: --seed " <> show seed
        putStrLn w

pushGroup :: String -> [Test] -> Check TestResult
pushGroup name list = do
    whenGroupOnly $ if groupHasSubGroup list then displayCurrent name else return ()
    withState $ \s -> ((), s { testPath = push (testPath s) name, indent = indent s + 2 })
    results <- mapM test list
    withState $ \s -> ((), s { testPath = pop (testPath s), indent = indent s - 2 })
    let totFail = sum $ fmap nbFail results
        tot = sum $ fmap nbTests results
    whenGroupOnly $ case (groupHasSubGroup list, totFail) of
        (True, _)              -> return ()
        (False, n) | n > 0     -> displayPropertyFailed name n ""
                   | otherwise -> displayPropertySucceed name tot
    return $ GroupResult name totFail results
  where
    sum = foldl' (+) 0
    push = snoc
    pop = maybe mempty fst . unsnoc

testProperty :: String -> Property -> Check TestResult
testProperty name prop = do
    seed <- getSeed <$> get
    path <- testPath <$> get
    let rngIt = genRng seed (name : toList path)

    maxTests <- numTests . config <$> get

    (res, nb) <- iterProp 1 maxTests rngIt
    return (PropertyResult name nb res)
  where
    iterProp !n !limit !rngIt
      | n == limit = passed >> return (PropertySuccess, n)
      | otherwise  = do
          params <- getGenParams . config <$> get
          r <- liftIO $ toResult n params
          case r of
              (PropertyFailed e, _)               -> failed >> return (PropertyFailed e, n)
              (PropertySuccess, cont) | cont      -> iterProp (n+1) limit rngIt
                                      | otherwise -> passed >> return (PropertySuccess, n)
        where
          toResult :: Word64 -> GenParams -> IO (PropertyResult, Bool)
          toResult it params =
                    (propertyToResult <$> evaluate (runGen (unProp prop) (rngIt it) params))
            `catch` (\(e :: SomeException) -> return (PropertyFailed (show e), False))

    propertyToResult p =
        let args   = propertyGetArgs p
            checks = getChecks p
         in if checkHasFailed checks
                then printError args checks
                else (PropertySuccess, length args > 0)

    printError args checks = (PropertyFailed (mconcat $ loop 1 args), False)
      where
        loop :: Word -> [String] -> [String]
        loop _ []      = printChecks checks
        loop !i (a:as) = "parameter " <> show i <> " : " <> a <> "\n" : loop (i+1) as
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

    propertyGetArgs (PropertyArg a p) = a : propertyGetArgs p
    propertyGetArgs (PropertyEOA _) = []

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
