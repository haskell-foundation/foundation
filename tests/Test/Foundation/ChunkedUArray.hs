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
import Foundation.Class.Storable
import Foundation.Primitive

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic

import Test.Foundation.Collection
import Test.Data.List
import Test.Utils.Foreign

testChunkedUArrayRefs :: TestTree
testChunkedUArrayRefs = testGroup "ChunkedArray"
    [ testGroup "Unboxed-Foreign"
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
        , testGroup "UArray(BE W16)" (testUnboxedForeign (Proxy :: Proxy (ChunkedUArray (BE Word16))) (toBE <$> arbitrary))
        , testGroup "UArray(BE W32)" (testUnboxedForeign (Proxy :: Proxy (ChunkedUArray (BE Word32))) (toBE <$> arbitrary))
        , testGroup "UArray(BE W64)" (testUnboxedForeign (Proxy :: Proxy (ChunkedUArray (BE Word64))) (toBE <$> arbitrary))
        , testGroup "UArray(LE W16)" (testUnboxedForeign (Proxy :: Proxy (ChunkedUArray (LE Word16))) (toLE <$> arbitrary))
        , testGroup "UArray(LE W32)" (testUnboxedForeign (Proxy :: Proxy (ChunkedUArray (LE Word32))) (toLE <$> arbitrary))
        , testGroup "UArray(LE W64)" (testUnboxedForeign (Proxy :: Proxy (ChunkedUArray (LE Word64))) (toLE <$> arbitrary))
        ]
    ]

testUnboxedForeign :: (PrimType e, Show e, Element a ~ e, StorableFixed e)
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
