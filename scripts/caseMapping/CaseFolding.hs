{-# LANGUAGE OverloadedStrings #-}
 
module CaseFolding
    (
      CaseFolding(..)
    , Fold(..)
    , parseCF
    , mapCF
    ) where

import           Foundation 
import           Foundation.IO
import qualified Foundation.Parser as P 
import qualified Foundation.String as S (lower, fromBytesUnsafe)
import           Foundation.VFS.FilePath

import UnicodeParsers

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

parseCF :: FilePath -> IO (Either (P.ParseError String) CaseFolding)
parseCF name = P.parseOnly entries . S.fromBytesUnsafe <$> readFile name

mapCF :: (String -> String) -> CaseFolding -> [String]
mapCF twiddle (CF _ ms) = typ <> (fmap nice . filter p $ ms) <> [last]
    where
      typ    = ["foldMapping :: Char -> CM",
                "{-# NOINLINE foldMapping #-}"]
      last   = "foldMapping c = CM (toLower c) '\\0' '\\0'"
      p f    = status f `elem` ("CF" :: String) &&
               mapping f /= [twiddle (code f)]
      nice c = "-- " <> name c <> "\n" <>
               "foldMapping " <> niceMap (code c) <> " = CM " <> x <> " " <> y <> " " <> z
           where pMap = (niceMap <$> mapping c) <> ["'\\0'","'\\0'","'\\0'"]
                 niceMap x = "'\\x" <> x <> "'" 
                 [x,y,z] = take (CountOf 3) pMap
