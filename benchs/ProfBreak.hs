module Main where

import Core
import Core.Collection

main = do
    let v = fromList [1..255] :: UArray Word8
    let (v1,v2) = break ((==) 128) v
    putStrLn $ (fromList $ show v1) <> (fromList $ show v2)
