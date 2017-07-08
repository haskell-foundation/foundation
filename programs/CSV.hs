{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
module Main where

import Foundation hiding (scale)
import Foundation.Conduit
import Foundation.Conduit.Textual
import Foundation.String (Encoding(..))
import Foundation.String.Read (readDouble)
import Foundation.Collection
import Foundation.IO
import qualified Prelude
import qualified Data.List

type Line = String

newtype Row = Row [Cell]
    deriving (Show,Eq)
newtype Cell = Cell String
    deriving (Show,Eq)

-- split lines and make cells from a handle.
-- not compliant, easy to 
badCSVParser handle =
    runConduit (sourceHandle handle .| fromBytes UTF8 .| lines .| cells .| sinkList)
  where
    cells = await >>= maybe (return ()) (\s -> yieldIfValid (splitLine $ dropCR s) >> cells)
      where yieldIfValid r@(Row cells)
                | length cells < 5 = pure ()
                | otherwise        = yield r
            splitLine s = Row $ fmap Cell $ splitOn (== ',') s
            dropCR :: String -> String
            dropCR s = if isSuffixOf "\r" s then revDrop 1 s else s

data BenchResult = BenchResult
    { benchName :: BenchName
    , mean      :: Double
    , meanLB    :: Double
    , meanUB    :: Double
    , stdDev    :: Double
    , stdDevLB  :: Double
    , stdDevUB  :: Double
    } deriving (Show,Eq)

-- | the best range associated with a (Double) time value in seconds
data Range = 
      RNanoSeconds
    | RMicroSeconds
    | RMilliSeconds
    | RSeconds
    deriving (Eq, Ord)

-- | Return the range display associated with a time value in seconds
getRange :: Double -> Range
getRange d
    | d < 0.000002 = RNanoSeconds
    | d < 0.002    = RMicroSeconds
    | d < 1.5      = RMilliSeconds
    | otherwise    = RSeconds

-- | Return the short name for the range
rangeToHName :: Range -> String
rangeToHName RSeconds      = "s"
rangeToHName RMilliSeconds = "ms"
rangeToHName RMicroSeconds = "μs"
rangeToHName RNanoSeconds  = "ns"

-- | Return the long name for the range
rangeToFullHName :: Range -> String
rangeToFullHName RSeconds      = "seconds"
rangeToFullHName RMilliSeconds = "milliseconds"
rangeToFullHName RMicroSeconds = "microseconds"
rangeToFullHName RNanoSeconds  = "nanoseconds"

-- | Scale the value to the required precision range
--
-- > scale RMilliSeconds 1.2 = 1200.0
-- > scale RMicroSeconds 0.00002 = 20.0
scale :: Range -> Double -> Double
scale r d = Prelude.fromIntegral (Prelude.round (d * (10 ^ (n+prec))) :: Int) / (10^n)
  where
    prec :: Word
    (prec, n) = case r of
                    RSeconds      -> (0,1)
                    RMilliSeconds -> (3,0)
                    RMicroSeconds -> (6,0)
                    RNanoSeconds  -> (9,0)

autoScale :: Double -> Double
autoScale d = scale (getRange d) d

printScale :: Range -> Double -> String
printScale r d = show (scale r d) <> " " <> rangeToHName r

printAutoScale :: Double -> String
printAutoScale d = show (autoScale d) <> " " <> rangeToHName (getRange d)


data BenchName = BenchName
    { benchCategory :: String
    , benchType     :: String
    , benchFct      :: String
    , benchOther    :: [String]
    } deriving (Show,Eq)

groupByBench :: [BenchResult] -> [(String, Range, [BenchResult])]
groupByBench bs = fmap (\b -> (prettyHeader $ benchName $ head $ nonEmpty_ b, 
                               minimum $ nonEmpty_ $ fmap (getRange . mean) b,
                               b))
                . Data.List.groupBy ((==) `on` (sel . benchName))
                $ bs
  where sel b = [benchCategory b, benchType b, benchFct b]

pretty :: BenchName -> String
pretty b = benchCategory b <> " | " <> benchType b <> " | " <> benchFct b <> " | "  <> (mconcat $ intersperse "," $ benchOther b)

prettyHeader :: BenchName -> String
prettyHeader b = benchCategory b <> " | " <> benchType b <> " | " <> benchFct b

prettyOther b = mconcat $ intersperse "," $ benchOther b

toBenchName :: String -> BenchName
toBenchName s =
    case splitOn (== '/') s of
        cat:ty:fct:r -> BenchName cat ty fct r
        _            -> error ("cannot process benchmark name " <> s)

toBench (Row [Cell n,Cell m,Cell ml,Cell mu,Cell s,Cell sl,Cell su]) =
    BenchResult <$> (pure $ toBenchName n)
                <*> (readDouble m)
                <*> (readDouble ml)
                <*> (readDouble mu)
                <*> (readDouble s)
                <*> (readDouble sl)
                <*> (readDouble su)
toBench r = error (show r)

justifyLeft :: CountOf Char -> String -> String
justifyLeft n s =
    case n - len of
        x | x <= 0    -> s
          | otherwise -> s <> replicate x ' '
  where
    len = length s

main = do
    rows <- badCSVParser stdin
    case rows of
        []             -> error "nothing parsed"
        (Row names:xs) -> do
            let benchs = groupByBench $ catMaybes $ fmap toBench xs
            flip mapM_ benchs $ \(benchHdr, rng, benchs) -> do
                putStrLn ("=== " <> benchHdr)
                flip mapM_ benchs $ \bench -> do
                    putStrLn (justifyLeft 30 (prettyOther (benchName bench)) <> ": " <> printScale rng (mean bench))
