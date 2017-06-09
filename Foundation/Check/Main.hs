-- |
-- Module      : Foundation.Check.Main
-- License     : BSD-style
-- Maintainer  : Foundation maintainers
--
-- An application to check that integrate with the .cabal test-suite
--
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Foundation.Check.Main
    ( defaultMain
    ) where

import           Foundation.Primitive.Imports
import           Foundation.Primitive.IntegralConv
import           Foundation.Primitive.Types.OffsetSize
import           Foundation.System.Info (os, OS(..))
import           Foundation.Collection
import           Foundation.Numerical
import           Foundation.IO.Terminal
import           Foundation.Check (iterateProperty)
import           Foundation.Check.Gen
import           Foundation.Check.Property
import           Foundation.Check.Config
import           Foundation.Check.Types
import           Foundation.List.DList
import           Foundation.Random
import           Foundation.Monad
import           Foundation.Monad.State
import           Control.Monad (when)
import           Data.Maybe (catMaybes)

nbFail :: TestResult -> HasFailures
nbFail (PropertyResult _ _ (PropertyFailed _)) = 1
nbFail (PropertyResult _ _ PropertySuccess)    = 0
nbFail (GroupResult    _ t _ _)                = t

nbTests :: TestResult -> CountOf TestResult
nbTests (PropertyResult _ t _) = t
nbTests (GroupResult _ _ t _)  = t

data TestState = TestState
    { config      :: !Config
    , getSeed     :: !Seed
    , indent      :: !(CountOf Char)
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
        | x         = True
        | otherwise = or xs

    testFilter acc x =
        case x of
            Group s l    ->
                let filtered = catMaybes $ fmap (testFilter (s:acc)) l
                 in if null filtered then Nothing else Just (Group s filtered)
            CheckPlan s _
                | match acc s -> Just x
                | otherwise   -> Nothing
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
    ecfg <- flip parseArgs defaultConfig <$> getArgs
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
            (_, cfg') <- runStateT (runCheckMain $ test t) testState
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
                Group s l     -> tToList (fmap (\z -> (z, pre)) xs <> acc) (s:pre) l
                CheckPlan s _ -> (s : pre) : tToList acc pre xs
                Unit s _      -> (s : pre) : tToList acc pre xs
                Property s _  -> (s : pre) : tToList acc pre xs

        tToList []           _   []              = []
        tToList ((a,pre):as) _   []              = testCases as [] pre a
        tToList acc          pre (x:xs)          = testCases acc xs pre x

-- | internal check monad for facilitating the tests traversal
newtype CheckMain a = CheckMain { runCheckMain :: StateT TestState IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadState CheckMain where
    type State CheckMain = TestState
    withState = CheckMain . withState

onDisplayOption :: DisplayOption -> CheckMain () -> CheckMain ()
onDisplayOption opt chk = do
    on <- (<=) opt . displayOptions . config <$> get
    if on then chk else return ()

whenErrorOnly :: CheckMain () -> CheckMain ()
whenErrorOnly = onDisplayOption DisplayTerminalErrorOnly

whenGroupOnly :: CheckMain () -> CheckMain ()
whenGroupOnly = onDisplayOption DisplayGroupOnly

whenVerbose :: CheckMain () -> CheckMain ()
whenVerbose = onDisplayOption DisplayTerminalVerbose

passed :: CheckMain ()
passed = withState $ \s -> ((), s { testPassed = testPassed s + 1 })

failed :: CheckMain ()
failed = withState $ \s -> ((), s { testFailed = testFailed s + 1 })

test :: Test -> CheckMain TestResult
test (Group s l) = pushGroup s l
test (Unit _ _) = undefined
test (CheckPlan name plan) = do
    r <- testCheckPlan name plan
    return r
test (Property name prop) = do
    r'@(PropertyResult _ nb r) <- testProperty name (property prop)
    case r of
        PropertySuccess  -> whenVerbose $ displayPropertySucceed name nb
        PropertyFailed w -> whenErrorOnly $ displayPropertyFailed name nb w
    return r'

displayCurrent :: String -> CheckMain ()
displayCurrent name = do
    i <- indent <$> get
    liftIO $ putStrLn $ replicate i ' ' <> name

displayPropertySucceed :: String -> CountOf TestResult -> CheckMain ()
displayPropertySucceed name (CountOf nb) = do
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

displayPropertyFailed :: String -> CountOf TestResult -> String -> CheckMain ()
displayPropertyFailed name (CountOf nb) w = do
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

pushGroup :: String -> [Test] -> CheckMain TestResult
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
    return $ GroupResult name totFail tot results
  where
    sum = foldl' (+) 0
    push = snoc
    pop = maybe mempty fst . unsnoc

testCheckPlan :: String -> Check () -> CheckMain TestResult
testCheckPlan name actions = do
    seed <- getSeed <$> get
    path <- testPath <$> get
    params <- getGenParams . config <$> get
    let rngIt = genRng seed (name : toList path)

    let planState = PlanState { planRng         = rngIt
                              , planValidations = 0
                              , planParams      = params
                              , planFailures    = []
                              }
    st <- liftIO (snd <$> runStateT (runCheck actions) planState)
    let fails = planFailures st
    if null fails
        then return (GroupResult name 0 (planValidations st) [])
        else do
            displayCurrent name
            forM_ fails $ \f ->
                liftIO $ putStrLn $ show f
            return (GroupResult name (length fails) (planValidations st) fails)

testProperty :: String -> Property -> CheckMain TestResult
testProperty name prop = do
    seed <- getSeed <$> get
    path <- testPath <$> get
    let rngIt = genRng seed (name : toList path)

    params <- getGenParams . config <$> get
    maxTests <- numTests . config <$> get

    (res,nb) <- liftIO $ iterateProperty (CountOf $ integralDownsize (integralCast maxTests :: Int64)) params rngIt prop
    case res of
        PropertyFailed {} -> failed
        PropertySuccess   -> passed
    return (PropertyResult name nb res)
