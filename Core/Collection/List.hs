-- |
-- Module      : Core.Vector.List
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
module Core.Collection.List
    ( wordsWhen
    ) where

import qualified Data.List
import           Core.Internal.Base

-- | Simple helper to split a list repeatly when the predicate match
wordsWhen     :: (x -> Bool) -> [x] -> [[x]]
wordsWhen _ [] = []
wordsWhen p is = loop is
  where
    loop s =
        let (w, s') = Data.List.break p s
         in case s' of
                []   -> [w]
                _:xs -> w : loop xs
