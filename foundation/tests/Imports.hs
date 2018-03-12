{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Imports
    ( diffList
    ) where

import Foundation

diffList :: (Eq a, Show a) => [a] -> [a] -> String
diffList a b = "left : " <> show a <> "\nright: " <> show b <> "\ndiff : " <> show d
  where
    d = loop 0 a b
    loop :: (Eq a, Show a) => Int -> [a] -> [a] -> String
    loop _ [] []       = "internal error : list is equal"
    loop n l1@(_:_) [] = "offset=" <> show n <> " extra left=" <> show l1
    loop n [] l2@(_:_) = "offset=" <> show n <> " extra right=" <> show l2
    loop n l1@(x:xs) l2@(y:ys)
        | x == y    = loop (n+1) xs ys
        | otherwise = "offset=" <> show n <> " left=" <> show l1 <> " right= " <> show l2
