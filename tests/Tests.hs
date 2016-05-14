{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Test.Tasty
--import           Test.Tasty.Options
--import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Control.Monad

import           Core.String
import           Core.Vector
import           Core.Collection
import           Core
import qualified Data.List as L
import qualified Prelude

data Unicode = Unicode { unUnicode :: LString }
    deriving (Show)

data Split = Split Unicode Char
    deriving (Show)

data CharMap = CharMap Unicode Prelude.Int
    deriving (Show)

addChar :: Prelude.Int -> Char -> Char
addChar n c = toEnum ((fromEnum c + n) `Prelude.mod` 0x10ffff)

--instance Show Unicode where
--    show = unUnicode
--
data ListElement a = ListElement [a]

--instance Element a => Arbitrary ListElement where

-- | A better version of arbitrary for Char
arbitraryChar :: Gen Char
arbitraryChar =
    toEnum <$> oneof [choose (1, 0xff), choose (0x100, 0x1000), choose (0x100, 0x10000), choose (0x1, 0x1000)]

instance Arbitrary Unicode where
    arbitrary = do
        n <- choose (0,49)
        oneof
            [ Unicode <$> replicateM n (toEnum <$> choose (1, 0xff))
            , Unicode <$> replicateM n (toEnum <$> choose (0x100, 0x1000))
            , Unicode <$> replicateM n (toEnum <$> choose (0x100, 0x10000))
            , Unicode <$> replicateM n (toEnum <$> choose (0x1, 0x1000))
            ]

instance Arbitrary Split where
    arbitrary = do
        ch <- oneof [ toEnum <$> choose (0,0x7f), toEnum <$> choose (0x100, 0x10000) ]
        l <- choose (0,4) >>= \n -> fmap unUnicode <$> replicateM n arbitrary
        return (Split (Unicode $ L.intercalate [ch] l) ch)

instance Arbitrary CharMap where
    arbitrary = do
        CharMap <$> arbitrary <*> choose (1,12)

isIdemPotent f s = f s == s

transEq unWrap f g s =
    let s' = unWrap s in f s' == g s'

--stringEq :: Eq a => (b -> a) -> (String -> b) -> (LString -> a) -> Unicode -> Bool
--stringEq back f g s =

assertEq a b
    | a == b    = True
    | otherwise = error ("got: " <> show a <> " expected: " <> show b)

listOfElement :: Gen e -> Gen [e]
listOfElement e = choose (0,49) >>= flip replicateM e

listOfElementMaxN :: Int -> Gen e -> Gen [e]
listOfElementMaxN n e = choose (0,n) >>= flip replicateM e

-- | Set in front of tests to make them verbose
qcv = adjustOption (\(QuickCheckVerbose _) -> QuickCheckVerbose True)

testEq :: (Show e, Eq e, Eq a, Monoid a, Element a ~ e, IsList a, Item a ~ Element a) => Proxy a -> Gen e -> [TestTree]
testEq proxy genElement =
    [ testProperty "x == x" $ withElements $ \l -> let col = fromListP proxy l in col == col
    , testProperty "x == y" $ with2Elements $ \(l1, l2) ->
        (fromListP proxy l1 == fromListP proxy l2) == (l1 == l2)
    ]
  where
    withElements f = forAll (listOfElement genElement) f
    with2Elements f = forAll ((,) <$> listOfElement genElement <*> listOfElement genElement) f

testOrd :: (Show e, Eq e, Eq a, Ord a, Ord e, Monoid a, Element a ~ e, IsList a, Item a ~ Element a) => Proxy a -> Gen e -> [TestTree]
testOrd proxy genElement =
    [ testProperty "x `compare` y" $ with2Elements $ \(l1, l2) ->
        (fromListP proxy l1 `compare` fromListP proxy l2) == (l1 `compare` l2)
    ]
  where
    with2Elements f = forAll ((,) <$> listOfElement genElement <*> listOfElement genElement) f

testMonoid :: (Show e, Eq a, Eq e, Ord a, Ord e, Monoid a, Element a ~ e, IsList a, Item a ~ Element a) => Proxy a -> Gen e -> [TestTree]
testMonoid proxy genElement =
    testEq proxy genElement <>
    testOrd proxy genElement <>
    --[ testProperty "mempty <> mempty == mempty" $ \l ->
    [ testProperty "mempty <> x == x" $ withElements $ \l -> let col = fromListP proxy l in (col <> mempty) == col
    , testProperty "x <> mempty == x" $ withElements $ \l -> let col = fromListP proxy l in (mempty <> col) == col
    , testProperty "x1 <> x2 == x1|x2" $ with2Elements $ \(l1,l2) ->
        (fromListP proxy l1 <> fromListP proxy l2) == fromListP proxy (l1 <> l2)
    , testProperty "mconcat [map fromList [e]] = fromList (concat [e])" $ withNElements $ \l ->
        mconcat (fmap (fromListP proxy) l) == fromListP proxy (mconcat l)
    ]
  where
    withElements f = forAll (listOfElement genElement) f
    with2Elements f = forAll ((,) <$> listOfElement genElement <*> listOfElement genElement) f
    withNElements f = forAll (listOfElementMaxN 5 (listOfElement genElement)) f

testCollection :: (Show e, Eq a, Eq e, Ord a, Ord e, Arbitrary e, OrderedCollection a, Item a ~ Element a, Element a ~ e) => Proxy a -> Gen e -> [TestTree]
testCollection proxy genElement =
    testMonoid proxy genElement <>
    [ testProperty "LString-convert" $ withElements $ isIdemPotent (toList . fromListP proxy)
    , testProperty "length" $ withElements $ \l -> (length $ fromListP proxy l) == length l
    , testProperty "take" $ withElements2 $ \(l, n) -> toList (take n $ fromListP proxy l) == (take n) l
    , testProperty "drop" $ withElements2 $ \(l, n) -> toList (drop n $ fromListP proxy l) == (drop n) l
    , testProperty "splitAt" $ withElements2 $ \(l, n) -> toList2 (splitAt n $ fromListP proxy l) == (L.splitAt n) l
    , testProperty "snoc" $ withElements2E $ \(l, c) -> toList (snoc (fromListP proxy l) c) == (l <> [c])
    , testProperty "cons" $ withElements2E $ \(l, c) -> toList (cons c (fromListP proxy l)) == (c : l)
    , testProperty "splitOn" $ withElements2E $ \(l, ch) ->
         fmap toList (splitOn (== ch) (fromListP proxy l)) == splitOn (== ch) l
    , testProperty "sortBy" $ withElements $ \l ->
        (sortBy compare $ fromListP proxy l) == fromListP proxy (sortBy compare l)
    , testProperty "reverse" $ withElements $ \l ->
        (reverse $ fromListP proxy l) == fromListP proxy (reverse l)
    ]
{-
    , testProperty "imap" $ \(CharMap (Unicode u) i) ->
        (imap (addChar i) (fromList u) :: String) `assertEq` fromList (Prelude.map (addChar i) u)
    ]
-}
  where
    toList2 (x,y) = (toList x, toList y)
    withElements f = forAll (listOfElement genElement) f
    withElements2 f = forAll ((,) <$> listOfElement genElement <*> arbitrary) f
    withElements2E f = forAll ((,) <$> listOfElement genElement <*> genElement) f

fromListP :: (IsList c, Item c ~ Element c) => Proxy c -> [Element c] -> c
fromListP p = \x -> asProxyTypeOf (fromList x) p

tests =
    [ testGroup "String"       (testCollection (Proxy :: Proxy String)           arbitraryChar)
    , testGroup "Vector"
        [ testGroup "Unboxed"
            [ testGroup "UVector(W8)"  (testCollection (Proxy :: Proxy (UVector Word8))  arbitrary)
            , testGroup "UVector(W16)" (testCollection (Proxy :: Proxy (UVector Word16)) arbitrary)
            , testGroup "UVector(W32)" (testCollection (Proxy :: Proxy (UVector Word32)) arbitrary)
            , testGroup "UVector(W64)" (testCollection (Proxy :: Proxy (UVector Word64)) arbitrary)
            , testGroup "UVector(I8)"  (testCollection (Proxy :: Proxy (UVector Int8))   arbitrary)
            , testGroup "UVector(I16)" (testCollection (Proxy :: Proxy (UVector Int16))  arbitrary)
            , testGroup "UVector(I32)" (testCollection (Proxy :: Proxy (UVector Int32))  arbitrary)
            , testGroup "UVector(I64)" (testCollection (Proxy :: Proxy (UVector Int64))  arbitrary)
            , testGroup "UVector(F32)" (testCollection (Proxy :: Proxy (UVector Float))  arbitrary)
            , testGroup "UVector(F64)" (testCollection (Proxy :: Proxy (UVector Double)) arbitrary)
            ]
        , testGroup "Boxed"
            [ testGroup "Vector(W8)"  (testCollection (Proxy :: Proxy (Vector Word8))  arbitrary)
            , testGroup "Vector(W16)" (testCollection (Proxy :: Proxy (Vector Word16)) arbitrary)
            , testGroup "Vector(W32)" (testCollection (Proxy :: Proxy (Vector Word32)) arbitrary)
            , testGroup "Vector(W64)" (testCollection (Proxy :: Proxy (Vector Word64)) arbitrary)
            , testGroup "Vector(I8)"  (testCollection (Proxy :: Proxy (Vector Int8))   arbitrary)
            , testGroup "Vector(I16)" (testCollection (Proxy :: Proxy (Vector Int16))  arbitrary)
            , testGroup "Vector(I32)" (testCollection (Proxy :: Proxy (Vector Int32))  arbitrary)
            , testGroup "Vector(I64)" (testCollection (Proxy :: Proxy (Vector Int64))  arbitrary)
            , testGroup "Vector(F32)" (testCollection (Proxy :: Proxy (Vector Float))  arbitrary)
            , testGroup "Vector(F64)" (testCollection (Proxy :: Proxy (Vector Double)) arbitrary)
            , testGroup "Vector(Int)" (testCollection (Proxy :: Proxy (Vector Int))  arbitrary)
            , testGroup "Vector(Integer)" (testCollection (Proxy :: Proxy (Vector Integer)) arbitrary)
            ]
        ]
    ]

main = defaultMain $ testGroup "foundation" tests
