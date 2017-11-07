{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Foundation
import qualified Prelude as P
import Basement.Terminal.Size
 
main :: IO ()
main = do
    (width, height) <- size
    putStrLn (show width)
    putStrLn (show height)
    
    
