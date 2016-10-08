{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Test.Foundation.Array
    ( testArrayRefs
    ) where

import Control.Monad
import Foundation
import Foundation.Collection
import Foundation.Foreign

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic

import Test.Foundation.Collection
import Test.Data.List
import Test.Utils.Foreign

testArrayRefs :: TestTree
testArrayRefs = testGroup "Array"
    [ testGroup "Unboxed"
        [ testCollection "UArray(W8)"  (Proxy :: Proxy (UArray Word8))  arbitrary
        , testCollection "UArray(W16)" (Proxy :: Proxy (UArray Word16)) arbitrary
        , testCollection "UArray(W32)" (Proxy :: Proxy (UArray Word32)) arbitrary
        , testCollection "UArray(W64)" (Proxy :: Proxy (UArray Word64)) arbitrary
        , testCollection "UArray(I8)"  (Proxy :: Proxy (UArray Int8))   arbitrary
        , testCollection "UArray(I16)" (Proxy :: Proxy (UArray Int16))  arbitrary
        , testCollection "UArray(I32)" (Proxy :: Proxy (UArray Int32))  arbitrary
        , testCollection "UArray(I64)" (Proxy :: Proxy (UArray Int64))  arbitrary
        , testCollection "UArray(F32)" (Proxy :: Proxy (UArray Float))  arbitrary
        , testCollection "UArray(F64)" (Proxy :: Proxy (UArray Double)) arbitrary
        , testCollection "UArray(CChar)"  (Proxy :: Proxy (UArray CChar))  (CChar <$> arbitrary)
        , testCollection "UArray(CUChar)" (Proxy :: Proxy (UArray CUChar)) (CUChar <$> arbitrary)
        ]
    , testGroup "Unboxed-Foreign"
        [ testGroup "UArray(W8)"  (testUnboxedForeign (Proxy :: Proxy (UArray Word8))  arbitrary)
        , testGroup "UArray(W16)" (testUnboxedForeign (Proxy :: Proxy (UArray Word16)) arbitrary)
        , testGroup "UArray(W32)" (testUnboxedForeign (Proxy :: Proxy (UArray Word32)) arbitrary)
        , testGroup "UArray(W64)" (testUnboxedForeign (Proxy :: Proxy (UArray Word64)) arbitrary)
        , testGroup "UArray(I8)"  (testUnboxedForeign (Proxy :: Proxy (UArray Int8))   arbitrary)
        , testGroup "UArray(I16)" (testUnboxedForeign (Proxy :: Proxy (UArray Int16))  arbitrary)
        , testGroup "UArray(I32)" (testUnboxedForeign (Proxy :: Proxy (UArray Int32))  arbitrary)
        , testGroup "UArray(I64)" (testUnboxedForeign (Proxy :: Proxy (UArray Int64))  arbitrary)
        , testGroup "UArray(F32)" (testUnboxedForeign (Proxy :: Proxy (UArray Float))  arbitrary)
        , testGroup "UArray(F64)" (testUnboxedForeign (Proxy :: Proxy (UArray Double)) arbitrary)
        , testGroup "UArray(CChar)"  (testUnboxedForeign (Proxy :: Proxy (UArray CChar))  (CChar <$> arbitrary))
        , testGroup "UArray(CUChar)" (testUnboxedForeign (Proxy :: Proxy (UArray CUChar)) (CUChar <$> arbitrary))
        ]
    , testGroup "Boxed"
        [ testCollection "Array(W8)"  (Proxy :: Proxy (Array Word8))  arbitrary
        , testCollection "Array(W16)" (Proxy :: Proxy (Array Word16)) arbitrary
        , testCollection "Array(W32)" (Proxy :: Proxy (Array Word32)) arbitrary
        , testCollection "Array(W64)" (Proxy :: Proxy (Array Word64)) arbitrary
        , testCollection "Array(I8)"  (Proxy :: Proxy (Array Int8))   arbitrary
        , testCollection "Array(I16)" (Proxy :: Proxy (Array Int16))  arbitrary
        , testCollection "Array(I32)" (Proxy :: Proxy (Array Int32))  arbitrary
        , testCollection "Array(I64)" (Proxy :: Proxy (Array Int64))  arbitrary
        , testCollection "Array(F32)" (Proxy :: Proxy (Array Float))  arbitrary
        , testCollection "Array(F64)" (Proxy :: Proxy (Array Double)) arbitrary
        , testCollection "Array(Int)" (Proxy :: Proxy (Array Int))  arbitrary
        , testCollection "Array(Int,Int)" (Proxy :: Proxy (Array (Int,Int)))  arbitrary
        , testCollection "Array(Integer)" (Proxy :: Proxy (Array Integer)) arbitrary
        , testCollection "Array(CChar)"   (Proxy :: Proxy (Array CChar))  (CChar <$> arbitrary)
        , testCollection "Array(CUChar)"  (Proxy :: Proxy (Array CUChar)) (CUChar <$> arbitrary)
        ]
    ]

testUnboxedForeign :: (PrimType e, Show e, Element a ~ e, Storable e)
                   => Proxy a -> Gen e -> [TestTree]
testUnboxedForeign proxy genElement =
    [ testProperty "equal" $ withElementsM $ \fptr l ->
        return $ toArrayP proxy l == foreignMem fptr (length l)
    , testProperty "take" $ withElementsM $ \fptr l -> do
        n <- pick arbitrary
        return $ take n (toArrayP proxy l) == take n (foreignMem fptr (length l))
    , testProperty "take" $ withElementsM $ \fptr l -> do
        n <- pick arbitrary
        return $ drop n (toArrayP proxy l) == drop n (foreignMem fptr (length l))
    ]
  where
    withElementsM f = monadicIO $ forAllM (generateListOfElement genElement) $ \l -> run (createPtr l) >>= \fptr -> f fptr l
    toArrayP :: PrimType (Element c) => Proxy c -> [Element c] -> UArray (Element c)
    toArrayP _ l = fromList l
