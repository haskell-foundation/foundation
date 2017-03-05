{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Test.Foundation.Storable
    ( testForeignStorableRefs
    , testPropertyStorable, testPropertyStorableFixed
    ) where

import Foundation
import Foundation.Class.Storable
import Foundation.Primitive

import qualified Foreign.Storable
import qualified Foreign.Marshal.Alloc
import qualified Foreign.Marshal.Array

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic

testForeignStorableRefs :: TestTree
testForeignStorableRefs = testGroup "Storable"
    [ testGroup "Storable"
        [ testPropertyStorable "Word8" (Proxy :: Proxy Word8)
        , testPropertyStorable "Word16" (Proxy :: Proxy Word16)
        , testPropertyStorable "Word32" (Proxy :: Proxy Word32)
        , testPropertyStorable "Word64" (Proxy :: Proxy Word64)
        , testPropertyStorable "Int8" (Proxy :: Proxy Int8)
        , testPropertyStorable "Int16" (Proxy :: Proxy Int16)
        , testPropertyStorable "Int32" (Proxy :: Proxy Int32)
        , testPropertyStorable "Int64" (Proxy :: Proxy Int64)
        , testPropertyStorable "Char" (Proxy :: Proxy Char)
        , testPropertyStorable "Double" (Proxy :: Proxy Double)
        , testPropertyStorable "Float" (Proxy :: Proxy Float)
        ]
    , testGroup "StorableFixed"
        [ testPropertyStorableFixed "Word8" (Proxy :: Proxy Word8)
        , testPropertyStorableFixed "Word16" (Proxy :: Proxy Word16)
        , testPropertyStorableFixed "Word32" (Proxy :: Proxy Word32)
        , testPropertyStorableFixed "Word64" (Proxy :: Proxy Word64)
        , testPropertyStorableFixed "Int8" (Proxy :: Proxy Int8)
        , testPropertyStorableFixed "Int16" (Proxy :: Proxy Int16)
        , testPropertyStorableFixed "Int32" (Proxy :: Proxy Int32)
        , testPropertyStorableFixed "Int64" (Proxy :: Proxy Int64)
        , testPropertyStorableFixed "Char" (Proxy :: Proxy Char)
        , testPropertyStorableFixed "Double" (Proxy :: Proxy Double)
        , testPropertyStorableFixed "Float" (Proxy :: Proxy Float)
        ]
    , testGroup "Endianness"
        [ testPropertyBE "Word16" (Proxy :: Proxy Word16)
        , testPropertyBE "Word32" (Proxy :: Proxy Word32)
        , testPropertyBE "Word64" (Proxy :: Proxy Word64)
        ]
    ]

testPropertyBE :: (ByteSwap a, StorableFixed a, Arbitrary a, Eq a, Show a)
               => LString
               -> Proxy a
               -> TestTree
testPropertyBE name p = testGroup name
    [ testProperty "fromBE . toBE == id" $ withProxy p $ \a ->
        fromBE (toBE a) === a
    , testProperty "fromLE . toLE == id" $ withProxy p $ \a ->
        fromLE (toLE a) === a
    ]
  where
    withProxy :: (ByteSwap a, StorableFixed a, Arbitrary a, Show a, Eq a)
              => Proxy a -> (a -> Property) -> (a -> Property)
    withProxy _ f = f

testPropertyStorable :: (Storable a, Foreign.Storable.Storable a, Arbitrary a, Eq a, Show a)
                     => LString
                     -> Proxy a
                     -> TestTree
testPropertyStorable name p = testGroup name
    [ testProperty "peek" $ testPropertyStorablePeek p
    , testProperty "poke" $ testPropertyStorablePoke p
    ]

testPropertyStorableFixed :: (StorableFixed a, Foreign.Storable.Storable a, Arbitrary a, Eq a, Show a)
                          => LString
                          -> Proxy a
                          -> TestTree
testPropertyStorableFixed name p = testGroup name
    [ testProperty "size"      $ withProxy p $ \a -> size p === (Size $ Foreign.Storable.sizeOf a)
    , testProperty "alignment" $ withProxy p $ \a -> alignment p === (Size $ Foreign.Storable.alignment a)
    , testProperty "peekOff"   $ testPropertyStorableFixedPeekOff p
    , testProperty "pokeOff"   $ testPropertyStorableFixedPokeOff p
    ]
  where
    withProxy :: (Arbitrary a, Storable a, Show a, Eq a, Foreign.Storable.Storable a)
              => Proxy a -> (a -> Property) -> (a -> Property)
    withProxy _ f = f

testPropertyStorablePeek :: (Storable a, Foreign.Storable.Storable a, Arbitrary a, Eq a, Show a)
                         => Proxy a
                         -> a
                         -> Property
testPropertyStorablePeek _ v = monadicIO $ do
    v' <- run $ Foreign.Marshal.Alloc.alloca $ \ptr -> do
            Foreign.Storable.poke ptr v
            peek ptr
    assertEq v v'

testPropertyStorablePoke :: (Storable a, Foreign.Storable.Storable a, Arbitrary a, Eq a, Show a)
                         => Proxy a
                         -> a
                         -> Property
testPropertyStorablePoke _ v = monadicIO $ do
    v' <- run $ Foreign.Marshal.Alloc.alloca $ \ptr -> do
            poke ptr v
            Foreign.Storable.peek ptr
    assertEq v v'

assertEq a b
  | a == b = assert True
  | otherwise = do
      run $ putStrLn $ show a <> " /= " <> show b
      assert False

data SomeWhereInArray a = SomeWhereInArray a Int Int
    deriving (Show, Eq)
instance (StorableFixed a, Arbitrary a) => Arbitrary (SomeWhereInArray a) where
    arbitrary = do
        a  <- arbitrary
        let p = toProxy a
            (Size minsz) = size p + (alignment p - size p)
        sz <- choose (minsz, 512)
        o  <- choose (0, sz - minsz)
        return $ SomeWhereInArray a sz o
      where
        toProxy :: a -> Proxy a
        toProxy _ = Proxy

testPropertyStorableFixedPeekOff :: (StorableFixed a, Foreign.Storable.Storable a, Arbitrary a, Eq a, Show a)
                         => Proxy a
                         -> SomeWhereInArray a
                         -> Property
testPropertyStorableFixedPeekOff _ (SomeWhereInArray v sz off) = monadicIO $ do
    v' <- run $ Foreign.Marshal.Array.allocaArray sz $ \ptr -> do
            Foreign.Storable.pokeElemOff ptr off v
            peekOff ptr (Offset off)
    assert $ v == v'

testPropertyStorableFixedPokeOff :: (StorableFixed a, Foreign.Storable.Storable a, Arbitrary a, Eq a, Show a)
                         => Proxy a
                         -> SomeWhereInArray a
                         -> Property
testPropertyStorableFixedPokeOff _ (SomeWhereInArray v sz off) = monadicIO $ do
    v' <- run $ Foreign.Marshal.Array.allocaArray sz $ \ptr -> do
            pokeOff ptr (Offset off) v
            Foreign.Storable.peekElemOff ptr off
    assert $ v == v'
