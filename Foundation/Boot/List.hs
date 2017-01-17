module Foundation.Boot.List
    ( length
    ) where

import Foundation.Internal.Base

length :: [a] -> Int
length []     = 0
length (_:xs) = succ (length xs)
