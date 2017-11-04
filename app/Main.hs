{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Foundation
import qualified Prelude as P
import qualified Basement.Terminal as T
 
main :: IO ()
main = do
     (width, height) <- T.size
     P.putStrLn ("width: " P.++ (P.show width) P.++ "height: " P.++ (P.show height))
