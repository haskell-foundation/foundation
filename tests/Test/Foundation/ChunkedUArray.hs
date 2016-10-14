{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Test.Foundation.ChunkedUArray
    ( testChunkedUArrayRefs
    ) where

import Control.Monad
import Foundation
import Foundation.Collection
import Foundation.Array
import Foundation.Foreign

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic

import Test.Foundation.Collection
import Test.Data.List
import Test.Utils.Foreign

testChunkedUArrayRefs :: TestTree
testChunkedUArrayRefs = testGroup "ChunkedArray"
    [ testGroup "Unboxed"
        [ testCollection "ChunkedUArray(W8)"  (Proxy :: Proxy (ChunkedUArray Word8))  arbitrary
        , testCollection "ChunkedUArray(W16)" (Proxy :: Proxy (ChunkedUArray Word16)) arbitrary
        , testCollection "ChunkedUArray(W32)" (Proxy :: Proxy (ChunkedUArray Word32)) arbitrary
        , testCollection "ChunkedUArray(W64)" (Proxy :: Proxy (ChunkedUArray Word64)) arbitrary
        , testCollection "ChunkedUArray(I8)"  (Proxy :: Proxy (ChunkedUArray Int8))   arbitrary
        , testCollection "ChunkedUArray(I16)" (Proxy :: Proxy (ChunkedUArray Int16))  arbitrary
        , testCollection "ChunkedUArray(I32)" (Proxy :: Proxy (ChunkedUArray Int32))  arbitrary
        , testCollection "ChunkedUArray(I64)" (Proxy :: Proxy (ChunkedUArray Int64))  arbitrary
        , testCollection "ChunkedUArray(F32)" (Proxy :: Proxy (ChunkedUArray Float))  arbitrary
        , testCollection "ChunkedUArray(F64)" (Proxy :: Proxy (ChunkedUArray Double)) arbitrary
        ]
    , testGroup "Unboxed-Foreign"
        [ testGroup "UArray(W8)"  (testUnboxedForeign (Proxy :: Proxy (ChunkedUArray Word8))  arbitrary)
        , testGroup "UArray(W16)" (testUnboxedForeign (Proxy :: Proxy (ChunkedUArray Word16)) arbitrary)
        , testGroup "UArray(W32)" (testUnboxedForeign (Proxy :: Proxy (ChunkedUArray Word32)) arbitrary)
        , testGroup "UArray(W64)" (testUnboxedForeign (Proxy :: Proxy (ChunkedUArray Word64)) arbitrary)
        , testGroup "UArray(I8)"  (testUnboxedForeign (Proxy :: Proxy (ChunkedUArray Int8))   arbitrary)
        , testGroup "UArray(I16)" (testUnboxedForeign (Proxy :: Proxy (ChunkedUArray Int16))  arbitrary)
        , testGroup "UArray(I32)" (testUnboxedForeign (Proxy :: Proxy (ChunkedUArray Int32))  arbitrary)
        , testGroup "UArray(I64)" (testUnboxedForeign (Proxy :: Proxy (ChunkedUArray Int64))  arbitrary)
        , testGroup "UArray(F32)" (testUnboxedForeign (Proxy :: Proxy (ChunkedUArray Float))  arbitrary)
        , testGroup "UArray(F64)" (testUnboxedForeign (Proxy :: Proxy (ChunkedUArray Double)) arbitrary)
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
