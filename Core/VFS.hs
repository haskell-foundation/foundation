{-# LANGUAGE MagicHash #-}
module Core.VFS
    (
      FilePath
    , FileName
    , filePathToString
    , filePathToLString
    , Path(..)
    ) where

import           Core.Internal.Base
import           Core
import qualified Core.Collection as C
import qualified Core.Vector.Unboxed as Vec
import           Core.Vector.Unboxed (UVector)
import qualified Core.String.UTF8 as S

class Path path where
    type PathEnt path
    (</>)        :: path -> PathEnt path -> path
    splitPath    :: path -> [PathEnt path]
    splitPathEnt :: path -> Maybe (path, PathEnt path)

instance Path FilePath where
    type PathEnt FilePath = FileName
    (</>)        = fileAppendEnt
    splitPath    = fileSplitPath
    splitPathEnt = fileSplitPathEnt

pathSeparator :: Word8
pathSeparator = 0x2f -- '/'

vPathSeparator :: UVector Word8
vPathSeparator = fromList [pathSeparator]

newtype FilePath = FilePath (UVector Word8)
    deriving (Eq,Ord)

data FileName = FileName (UVector Word8)
    deriving (Eq,Ord)

fileAppendEnt :: FilePath -> FileName -> FilePath
fileAppendEnt (FilePath path) (FileName name) =
    FilePath (mconcat [path, vPathSeparator, name])

fileSplitPath :: FilePath -> [FileName]
fileSplitPath (FilePath p) = fmap FileName $ C.splitOn (== pathSeparator) p

fileSplitPathEnt :: FilePath -> Maybe (FilePath, FileName)
fileSplitPathEnt (FilePath p)
    | C.null p            = Nothing
    | p == vPathSeparator = Nothing
    | otherwise           = Nothing

filePathToString :: FilePath -> String
filePathToString (FilePath fp) =
    -- FIXME probably incorrect considering windows.
    -- this is just to get going to be able to be able to reuse System.IO functions which
    -- works on [Char]
    case S.fromBytes S.UTF8 fp of
        Just s -> s
        Nothing -> error "cannot convert path"

filePathToLString :: FilePath -> [Char]
filePathToLString = toList . filePathToString
