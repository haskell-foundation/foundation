{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Foundation
import qualified Prelude as P
import qualified Basement.Terminal as T
import Foundation.System.Bindings.POSIX.Stropts 
 
main :: IO ()
main = do
    (width, height) <- size
    putStrLn (show width)
    putStrLn (show height)
    
    
