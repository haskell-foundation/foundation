{-# LANGUAGE MagicHash #-}
module Core.VFS
    (
      FilePath
    , FileName
    , filePathToString
    , filePathFromString
    , filePathToLString
    , Path(..)
    ) where

import           Core.Internal.Base
import           Core
import qualified Core.Collection as C
import qualified Core.String.UTF8 as S

-- | Represent an abstract Path that is made of PathEnt
class Path path where
    type PathEnt path
    (</>)        :: path -> PathEnt path -> path
    splitPath    :: path -> [PathEnt path]
    splitPathEnt :: path -> Maybe (path, PathEnt path)

-- | operating system local file
instance Path FilePath where
    type PathEnt FilePath = FileName
    (</>)        = fileAppendEnt
    splitPath    = fileSplitPath
    splitPathEnt = fileSplitPathEnt

pathSeparator :: Word8
pathSeparator = 0x2f -- '/'

vPathSeparator :: UArray Word8
vPathSeparator = fromList [pathSeparator]

-- | Represent an opaque filepath on the operating system
newtype FilePath = FilePath ByteArray
    deriving (Eq,Ord)

-- | Represent a file name on the operating system
--
-- typically a file name doesn't contains any path separator
data FileName = FileName ByteArray
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
        (s, bs)
            | C.null bs -> s
            | otherwise -> error "cannot convert path"

filePathFromString :: String -> FilePath
filePathFromString s = FilePath (S.toBytes S.UTF8 s)

filePathToLString :: FilePath -> [Char]
filePathToLString = toList . filePathToString
