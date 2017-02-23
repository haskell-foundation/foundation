module Test.Foundation.Misc
    ( testHexadecimal
    ) where

import Foundation
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic

import Foundation.Array.Internal (toHexadecimal)
import Test.Foundation.Collection (fromListP, toListP)

hex :: [Word8] -> [Word8]
hex = loop
  where
    toHex :: Int -> Word8
    toHex n
        | n < 10   = fromIntegral (n + fromEnum '0')
        | otherwise = fromIntegral (n - 10 + fromEnum 'a')
    loop []     = []
    loop (x:xs) = toHex (fromIntegral q):toHex (fromIntegral r):loop xs
      where
        (q,r) = x `divMod` 16

testHexadecimal = testGroup "hexadecimal"
    [ testProperty  "UArray(W8)" $ \l -> 
        toList (toHexadecimal (fromListP (Proxy :: Proxy (UArray Word8)) l)) == hex l
    ]
