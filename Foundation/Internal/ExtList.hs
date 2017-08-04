{-# LANGUAGE CPP #-}
module Foundation.Internal.ExtList
    ( length
    , null
    , sum
    , reverse
    ) where

import Foundation.Internal.Base
import Foundation.Primitive.Numerical.Additive
import qualified GHC.List as List

-- | Compute the size of the list
length :: [a] -> Int
#if MIN_VERSION_base(4,8,0)
length = List.foldl' (\c _ -> c+1) 0
#else
length = loop 0
  where loop !acc []     = acc
        loop !acc (_:xs) = loop (1+acc) xs
#endif

null :: [a] -> Bool
null []    = True
null (_:_) = False

-- | Sum the element in a list
sum :: Additive n => [n] -> n
sum []     = azero
sum (i:is) = loop i is
  where
    loop !acc [] = acc
    loop !acc (x:xs) = loop (acc+x) xs
    {-# INLINE loop #-}

reverse :: [a] -> [a]
reverse l =  go l []
  where
    go []     acc = acc
    go (x:xs) acc = go xs (x:acc)
