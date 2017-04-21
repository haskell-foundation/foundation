{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Test.Foundation.DList
    ( testDList
    ) where

import Foundation
import Foundation.List.DList

import Foundation.Foreign
import Foundation.Primitive

import Test.Tasty
import Test.Tasty.QuickCheck

import Test.Foundation.Collection

testDList :: TestTree
testDList = testGroup "DList"
    [ testCollection "DList(W8)"  (Proxy :: Proxy (DList Word8))  arbitrary
    , testCollection "DList(W16)" (Proxy :: Proxy (DList Word16)) arbitrary
    , testCollection "DList(W32)" (Proxy :: Proxy (DList Word32)) arbitrary
    , testCollection "DList(W64)" (Proxy :: Proxy (DList Word64)) arbitrary
    , testCollection "DList(I8)"  (Proxy :: Proxy (DList Int8))   arbitrary
    , testCollection "DList(I16)" (Proxy :: Proxy (DList Int16))  arbitrary
    , testCollection "DList(I32)" (Proxy :: Proxy (DList Int32))  arbitrary
    , testCollection "DList(I64)" (Proxy :: Proxy (DList Int64))  arbitrary
    , testCollection "DList(F32)" (Proxy :: Proxy (DList Float))  arbitrary
    , testCollection "DList(F64)" (Proxy :: Proxy (DList Double)) arbitrary
    , testCollection "DList(CChar)"  (Proxy :: Proxy (DList CChar))  (CChar <$> arbitrary)
    , testCollection "DList(CUChar)" (Proxy :: Proxy (DList CUChar)) (CUChar <$> arbitrary)
    , testCollection "DList(BE W16)" (Proxy :: Proxy (DList (BE Word16))) (toBE <$> arbitrary)
    , testCollection "DList(BE W32)" (Proxy :: Proxy (DList (BE Word32))) (toBE <$> arbitrary)
    , testCollection "DList(BE W64)" (Proxy :: Proxy (DList (BE Word64))) (toBE <$> arbitrary)
    , testCollection "DList(LE W16)" (Proxy :: Proxy (DList (LE Word16))) (toLE <$> arbitrary)
    , testCollection "DList(LE W32)" (Proxy :: Proxy (DList (LE Word32))) (toLE <$> arbitrary)
    , testCollection "DList(LE W64)" (Proxy :: Proxy (DList (LE Word64))) (toLE <$> arbitrary)
    ]
