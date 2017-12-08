{-# LANGUAGE OverloadedStrings #-}
 
module SpecialCasing
    (
      SpecialCasing(..)
    , Case(..)
    , parseSC
    , mapSC
    ) where

import qualified Data.Char as C (toUpper)  

import           Foundation 
import           Foundation.IO
import qualified Foundation.Parser as P 
import           Foundation.VFS.FilePath
import           Foundation.Collection   (nonEmpty_)
import qualified Foundation.String as S 

import UnicodeParsers

data SpecialCasing = SC {scComments :: [Comment], scCasing :: [Case]}
                        deriving (Show)

data Case = Case {
    code       :: String
  , lower      :: [String]
  , title      :: [String]
  , upper      :: [String]
  , conditions :: String
  , name       :: String
} deriving (Eq, Ord, Show)


entries :: P.Parser String SpecialCasing
entries = SC <$> P.many comment <*> P.many (entry <* P.many comment)
  where
    entry = Case <$> unichar  <* P.string ";"
                 <*> unichars <* P.string ";"
                 <*> unichars <* P.string ";"
                 <*> unichars <* P.string "; " 
                 <*> (P.takeWhile (/= '#') <* P.string "# ")
                 <*> P.takeWhile (/= '\n') <* P.string "\n" 

parseSC :: FilePath -> IO (Either (P.ParseError String) SpecialCasing)
parseSC name = P.parseOnly entries . S.fromBytesUnsafe <$> readFile name

mapSC :: String -> (Case -> [String]) -> (String -> String) -> SpecialCasing -> [String]
mapSC wich access twiddle (SC _ ms) =
    typ `mappend` (fmap nice . filter p $ ms) `mappend` last
  where
    typ    = [wich <>  "Mapping :: forall s. Char -> s -> Step (CC s) Char",
              "{-# NOINLINE " <> wich <> "Mapping #-}"]
    last   = [wich <> "Mapping c s = Yield (to" <> ucFst wich 
              <> " c) (CC s '\\0' '\\0')","",""]
    p c = [k] /= a && a /= [twiddle k] && null (conditions c)
        where a = access c
              k = code c
    nice c = "-- " <> name c <> "\n" <>
             wich <> "Mapping " <> pHex(code c) <> " s = Yield " 
             <> x <> " (CC s " <> y <> " " <> z <> ")"
        where pMap = (pHex <$> access c) <> ["'\\0'","'\\0'","'\\0'"]
              pHex x = "'\\x" <> x <> "'" 
              [x,y,z] = take (CountOf 3) pMap

ucFst :: String -> String
ucFst s 
  | null s  = "" 
  | otherwise = (fromString [C.toUpper (head neS)]) <> tail neS
  where neS = nonEmpty_ s
