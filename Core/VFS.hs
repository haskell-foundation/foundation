-- |
-- Module      : Core.VFS
-- License     : BSD-style
-- Maintainer  : foundation
-- Stability   : experimental
-- Portability : portable
--
module Core.VFS
    ( Path(..)
    , filename
    , parent
    , prefix
    , suffix

      -- * FilePath
    , FilePath
    , FileName
      -- ** conversion
    , filePathToString
    , filePathToLString
    ) where


import Core.VFS.Path
          ( Path(..)
          , filename, parent, suffix, prefix
          )
import Core.VFS.FilePath
          ( FilePath, FileName
          , filePathToString
          , filePathToLString
          )
