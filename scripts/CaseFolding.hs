{-# LANGUAGE OverloadedStrings #-}
 
module CaseFolding
    (
      CaseFolding(..)
    , Fold(..)
    , parseCF
    , mapCF
    ) where

import Foundation 
import Foundation.IO
import qualified Foundation.Parser as P 
import Foundation.Collection (Element, nonEmpty_)
import qualified Foundation.String as S (lower, fromBytesUnsafe, Encoding(..))

import UnicodeParsers

type FilePath = [Char]

data Fold = Fold {
      code :: String
    , status :: Char
    , mapping :: [String]
    , name :: String
    } deriving (Eq, Ord, Show)

data CaseFolding = CF { cfComments :: [Comment], cfFolding :: [Fold] }
                 deriving (Show)

entries :: P.Parser String CaseFolding
entries = CF <$> P.many comment <*> P.some entry
  where
    entry = Fold <$> unichar <* semiCol
                 <*> oneOf "CFST" <* P.string ";"
                 <*> unichars <* semiCol
                 <*> (P.string "# " *> P.takeWhile (/= '\n')) <* P.string "\n"

parseCF :: FilePath -> IO (P.Result String CaseFolding)
parseCF name = P.parse entries . S.fromBytesUnsafe <$> readFile (fromString name)

mapCF :: CaseFolding -> [String]
mapCF (CF _ ms) = typ <> (fmap nice . filter p $ ms) <> [last]
    where
      typ = ["foldMapping :: forall s. Char -> s -> Step (CC s) Char"
             ,"{-# NOINLINE foldMapping #-}"]
      last = "foldMapping c s = Yield (toLower c) (CC s '\\0' '\\0')"
      nice c = "-- " <> name c <> "\n" <>
               "foldMapping " <> code c <> " s = Yield " <> x <> " (CC s " <> y <> " " <> z <> ")"
         where pMap = mapping c <> ["'\\0'","'\\0'","'\\0'"]
               [x,y,z] = take (CountOf 3) pMap
      p f = status f `elem` ("CF" :: String) &&
            mapping f /= [S.lower (code f)]
