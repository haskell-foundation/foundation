module Foundation.Boot.List
    ( length
    , sum
    ) where

import Foundation.Internal.Base
import Foundation.Numerical.Additive

-- | Compute the size of the list
length :: [a] -> Int
length []     = 0
length (_:xs) = succ (length xs)

-- | Sum the element in a list
sum :: Additive n => [n] -> n
sum []     = azero
sum (i:is) = loop i is
  where
    loop !acc [] = acc
    loop !acc (x:xs) = loop (acc+x) xs
    {-# INLINE loop #-}
