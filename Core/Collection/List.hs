-- |
-- Module      : Core.Vector.List
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
module Core.Collection.List
    ( wordsWhen
    , revTake
    , revDrop
    , revSplitAt
    ) where

import qualified Data.List
import           Data.Tuple (swap)
import           Core.Internal.Base
import           Core.Number

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

revTake :: Int -> [a] -> [a]
revTake n l = Data.List.drop (len - n) l
  where
    len = Data.List.length l

revDrop :: Int -> [a] -> [a]
revDrop n l = Data.List.take (len - n) l
  where
    len = Data.List.length l

revSplitAt :: Int -> [a] -> ([a],[a])
revSplitAt n l = swap $ Data.List.splitAt (len - n) l
  where
    len = Data.List.length l
