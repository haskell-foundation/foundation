{-# LANGUAGE RebindableSyntax #-}
module Main where

import Control.Monad
import Foundation
import Foundation.Monad
import Foundation.Time.Bindings
import Foundation.IO.Terminal

main = do
    r <- replicateM 10000 getMonotonicTime
    mapM_ (putStrLn . show) r
