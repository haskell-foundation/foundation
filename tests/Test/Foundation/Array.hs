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
import Foundation.Primitive
import Foundation.Class.Storable

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic

import Test.Foundation.Collection
import Test.Data.List
import Test.Utils.Foreign

testArrayRefs :: TestTree
testArrayRefs = testGroup "Array"
    [ testGroup "Unboxed-Foreign"
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
        , testGroup "UArray(BE W16)" (testUnboxedForeign (Proxy :: Proxy (UArray (BE Word16))) (toBE <$> arbitrary))
        , testGroup "UArray(BE W32)" (testUnboxedForeign (Proxy :: Proxy (UArray (BE Word32))) (toBE <$> arbitrary))
        , testGroup "UArray(BE W64)" (testUnboxedForeign (Proxy :: Proxy (UArray (BE Word64))) (toBE <$> arbitrary))
        , testGroup "UArray(LE W16)" (testUnboxedForeign (Proxy :: Proxy (UArray (LE Word16))) (toLE <$> arbitrary))
        , testGroup "UArray(LE W32)" (testUnboxedForeign (Proxy :: Proxy (UArray (LE Word32))) (toLE <$> arbitrary))
        , testGroup "UArray(LE W64)" (testUnboxedForeign (Proxy :: Proxy (UArray (LE Word64))) (toLE <$> arbitrary))
        ]
    ]

testUnboxedForeign :: (PrimType e, Show e, Element a ~ e, StorableFixed e)
                   => Proxy a -> Gen e -> [TestTree]
testUnboxedForeign proxy genElement =
    [ testProperty "equal" $ withElementsM $ \fptr l ->
        return $ toArrayP proxy l == foreignMem fptr (length l)
    , testProperty "take" $ withElementsM $ \fptr l -> do
        n <- CountOf <$> pick arbitrary
        return $ take n (toArrayP proxy l) == take n (foreignMem fptr (length l))
    , testProperty "take" $ withElementsM $ \fptr l -> do
        n <- CountOf <$> pick arbitrary
        return $ drop n (toArrayP proxy l) == drop n (foreignMem fptr (length l))
    ]
  where
    withElementsM f = monadicIO $ forAllM (generateListOfElement genElement) $ \l -> run (createPtr l) >>= \fptr -> f fptr l
    toArrayP :: PrimType (Element c) => Proxy c -> [Element c] -> UArray (Element c)
    toArrayP _ l = fromList l
