{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Test.Tasty
--import           Test.Tasty.Options
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Monadic
import           Control.Monad

import           Core.String
import           Core.Array
import           Core.Foreign
import           Core.Collection
import           Core.VFS (Path(..), parent, filename)
import           Core.VFS.FilePath
import           Core
import qualified Data.List as L
import qualified Prelude

import           ForeignUtils

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

instance Arbitrary FileName where
    arbitrary = do
        s <- choose (1, 30)
        unsafeFileName . fromList <$> vectorOf s genChar
      where
        genChar :: Gen Word8
        genChar = frequency
                    [ (10, pure 0x2e) -- '.'
                    , (10, choose (0x41, 0x5A)) -- [A-Z]
                    , (10, choose (0x61, 0x7A)) -- [a-z]
                    , (5, choose (0x30, 0x39)) -- [a-z]
                    , (5, elements [0x2d, 0x5f]) -- [-_]
                    ]

instance Arbitrary Relativity where
    arbitrary = elements [ Absolute, Relative ]

instance Arbitrary FilePath where
    arbitrary = do
        s <- choose (0, 10)
        unsafeFilePath <$> arbitrary
                       <*> vectorOf s arbitrary

isIdemPotent :: Eq a => (a -> a) -> a -> Bool
isIdemPotent f s = f s == s

transEq :: Eq a => (t -> t1) -> (t1 -> a) -> (t1 -> a) -> t -> Bool
transEq unWrap f g s =
    let s' = unWrap s in f s' == g s'

--stringEq :: Eq a => (b -> a) -> (String -> b) -> (LString -> a) -> Unicode -> Bool
--stringEq back f g s =

assertEq :: (Eq a, Show a) => a -> a -> Bool
assertEq got expected
    | got == expected = True
    | otherwise       = error ("got: " <> show got <> " expected: " <> show expected)

listOfElement :: Gen e -> Gen [e]
listOfElement e = choose (0,49) >>= flip replicateM e

listOfElementMaxN :: Int -> Gen e -> Gen [e]
listOfElementMaxN n e = choose (0,n) >>= flip replicateM e

-- | Set in front of tests to make them verbose
qcv :: TestTree -> TestTree
qcv = adjustOption (\(QuickCheckVerbose _) -> QuickCheckVerbose True)

-- | Set the number of tests
qcnSet :: Int -> TestTree -> TestTree
qcnSet n = adjustOption (\(QuickCheckTests _) -> QuickCheckTests n)

-- | Scale the number of tests
qcnScale :: Int -> TestTree -> TestTree
qcnScale n = adjustOption (\(QuickCheckTests actual) -> QuickCheckTests (actual * n))

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

testCollectionProps :: (Show a, Sequential a, Eq a, e ~ Item a) => Proxy a -> Gen e -> [TestTree]
testCollectionProps proxy genElement =
    [ testProperty "splitAt == (take, drop)" $ withCollection2 $ \(col, n) ->
        splitAt n col == (take n col, drop n col)
    , testProperty "revSplitAt == (revTake, revDrop)" $ withCollection2 $ \(col, n) ->
        revSplitAt n col == (revTake n col, revDrop n col)
    ]
  where
    withCollection2 f = forAll ((,) <$> (fromListP proxy <$> listOfElement genElement) <*> arbitrary) f

testCaseFilePath :: [TestTree]
testCaseFilePath = Prelude.map (makeTestCases . (\x -> (show x, x)))
    [ "/"
    , "."
    , ".."
    , "C:" </> "Users" </> "haskell-lang"
    , "/home"
    , "/home" </> "haskell-lang" </> "new hope" </> "foundation"
    , "~" </> "new hope" </> "foundation"
    , "new hope" </> "foundation"
    , "new hope" </> "foundation" </> ".."
    , "." </> "new hope" </> ".." </> ".." </> "haskell-lang" </> "new hope"
    ]
  where
    makeTestCases :: ([Char], FilePath) -> TestTree
    makeTestCases (title, p) = testGroup title
        [ testCase "buildPath . splitPath == id)" $ assertBuildSplitIdemPotent p
        , testCase "p == (parent p </> filename p)" $ assertParentFilenameIdemPotent p
        ]

    assertParentFilenameIdemPotent :: FilePath -> Assertion
    assertParentFilenameIdemPotent p =
      unless (assertEq (parent p </> filename p) p) $
         error "assertion failed"
    assertBuildSplitIdemPotent :: FilePath -> Assertion
    assertBuildSplitIdemPotent p =
      unless (assertEq (buildPath $ splitPath p) p) $
         error "assertion failed"

testPath :: (Path path, Show path, Eq path)
         => Gen path
         -> [TestTree]
testPath genElement =
    [ testProperty "buildPath . splitPath == id" $ withElements $ isIdemPotent (buildPath . splitPath)
    ]
  where
    withElements f = forAll genElement f

testCollection :: ( Show a, Show (Element a)
                  , Eq a, Eq (Element a)
                  , Ord a, Ord (Item a)
                  , Sequential a, Item a ~ Element a) => Proxy a -> Gen (Element a) -> [TestTree]
testCollection proxy genElement =
    testMonoid proxy genElement <>
    [ testProperty "LString-convert" $ withElements $ isIdemPotent (toList . fromListP proxy)
    , testProperty "length" $ withElements $ \l -> (length $ fromListP proxy l) == length l
    , testProperty "take" $ withElements2 $ \(l, n) -> toList (take n $ fromListP proxy l) == (take n) l
    , testProperty "drop" $ withElements2 $ \(l, n) -> toList (drop n $ fromListP proxy l) == (drop n) l
    , testProperty "splitAt" $ withElements2 $ \(l, n) -> toList2 (splitAt n $ fromListP proxy l) == (splitAt n) l
    , testProperty "revTake" $ withElements2 $ \(l, n) -> toList (revTake n $ fromListP proxy l) == (revTake n) l
    , testProperty "revDrop" $ withElements2 $ \(l, n) -> toList (revDrop n $ fromListP proxy l) == (revDrop n) l
    , testProperty "revSplitAt" $ withElements2 $ \(l, n) -> toList2 (revSplitAt n $ fromListP proxy l) == (revSplitAt n) l
    , testProperty "snoc" $ withElements2E $ \(l, c) -> toList (snoc (fromListP proxy l) c) == (l <> [c])
    , testProperty "cons" $ withElements2E $ \(l, c) -> toList (cons c (fromListP proxy l)) == (c : l)
    , testProperty "unsnoc" $ withElements $ \l -> fmap toListFirst (unsnoc (fromListP proxy l)) == unsnoc l
    , testProperty "uncons" $ withElements $ \l -> fmap toListSecond (uncons (fromListP proxy l)) == uncons l
    , testProperty "splitOn" $ withElements2E $ \(l, ch) ->
         fmap toList (splitOn (== ch) (fromListP proxy l)) == splitOn (== ch) l
    , testProperty "intersperse" $ withElements2E $ \(l, c) ->
        toList (intersperse c (fromListP proxy l)) == intersperse c l
    , testProperty "intercalate" $ withElements2E $ \(l, c) ->
        let ls = Prelude.replicate 5 l
            cs = Prelude.replicate 5 c
        in toList (intercalate (fromListP proxy cs) (fromListP proxy <$> ls)) == intercalate cs ls
    , testProperty "sortBy" $ withElements $ \l ->
        (sortBy compare $ fromListP proxy l) == fromListP proxy (sortBy compare l)
    , testProperty "reverse" $ withElements $ \l ->
        (reverse $ fromListP proxy l) == fromListP proxy (reverse l)
    -- stress slicing
    , testProperty "take . take" $ withElements3 $ \(l, n1, n2) -> toList (take n2 $ take n1 $ fromListP proxy l) == (take n2 $ take n1 l)
    , testProperty "drop . take" $ withElements3 $ \(l, n1, n2) -> toList (drop n2 $ take n1 $ fromListP proxy l) == (drop n2 $ take n1 l)
    , testProperty "drop . drop" $ withElements3 $ \(l, n1, n2) -> toList (drop n2 $ drop n1 $ fromListP proxy l) == (drop n2 $ drop n1 l)
    , testProperty "drop . take" $ withElements3 $ \(l, n1, n2) -> toList (drop n2 $ take n1 $ fromListP proxy l) == (drop n2 $ take n1 l)
    , testProperty "second take . splitAt" $ withElements3 $ \(l, n1, n2) ->
        (toList2 $ (second (take n1) . splitAt n2) $ fromListP proxy l) == (second (take n1) . splitAt n2) l
    ]
    <> testCollectionProps proxy genElement
{-
    , testProperty "imap" $ \(CharMap (Unicode u) i) ->
        (imap (addChar i) (fromList u) :: String) `assertEq` fromList (Prelude.map (addChar i) u)
    ]
-}
  where
    toList2 (x,y) = (toList x, toList y)
    toListFirst (x,y) = (toList x, y)
    toListSecond (x,y) = (x, toList y)
    withElements f = forAll (listOfElement genElement) f
    withElements2 f = forAll ((,) <$> listOfElement genElement <*> arbitrary) f
    withElements3 f = forAll ((,,) <$> listOfElement genElement <*> arbitrary <*> arbitrary) f
    withElements2E f = forAll ((,) <$> listOfElement genElement <*> genElement) f

testUnboxedForeign :: (PrimType e, Show e, Eq a, Eq e, Ord a, Ord e, Arbitrary e, Sequential a, Item a ~ Element a, Element a ~ e, Storable e)
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
    withElementsM f = monadicIO $ forAllM (listOfElement genElement) $ \l -> run (createPtr l) >>= \fptr -> f fptr l
    toArrayP :: PrimType (Element c) => Proxy c -> [Element c] -> UArray (Element c)
    toArrayP _ l = fromList l

fromListP :: (IsList c, Item c ~ Element c) => Proxy c -> [Element c] -> c
fromListP p = \x -> asProxyTypeOf (fromList x) p

data RandomList = RandomList [Int]
    deriving (Show,Eq)

instance Arbitrary RandomList where
    arbitrary = RandomList <$> (choose (100,400) >>= flip replicateM (choose (0,8)))

chunks :: Sequential c => RandomList -> c -> [c]
chunks (RandomList randomInts) = loop (randomInts <> [1..])
  where
    loop rx c
        | null c  = []
        | otherwise =
            case rx of
                r:rs ->
                    let (c1,c2) = splitAt r c
                     in c1 : loop rs c2
                [] ->
                    loop randomInts c


testStringCases :: [TestTree]
testStringCases =
    [ testGroup "Validation"
        [ testProperty "fromBytes . toBytes == valid" $ \(Unicode l) ->
            let s = fromList l
             in (fromBytes UTF8 $ toBytes UTF8 s) === (s, Nothing, mempty)
        , testProperty "Streaming" $ \(Unicode l, randomInts) ->
            let wholeS  = fromList l
                wholeBA = toBytes UTF8 wholeS
                reconstruct (prevBa, errs, acc) ba =
                    let ba' = prevBa `mappend` ba
                        (s, merr, nextBa) = fromBytes UTF8 ba'
                     in (nextBa, merr : errs, s : acc)

                (remainingBa, allErrs, chunkS) = foldl reconstruct (mempty, [], []) $ chunks randomInts wholeBA
             in (catMaybes allErrs === []) .&&. (remainingBa === mempty) .&&. (mconcat (reverse chunkS) === wholeS)
        ]
    , testGroup "Cases"
        [ testGroup "Invalid-UTF8"
            [ testCase "ff" $ expectFromBytesErr ("", Just InvalidHeader, 0) (fromList [0xff])
            , testCase "80" $ expectFromBytesErr ("", Just InvalidHeader, 0) (fromList [0x80])
            , testCase "E2 82 0C" $ expectFromBytesErr ("", Just InvalidContinuation, 0) (fromList [0xE2,0x82,0x0c])
            , testCase "30 31 E2 82 0C" $ expectFromBytesErr ("01", Just InvalidContinuation, 2) (fromList [0x30,0x31,0xE2,0x82,0x0c])
            ]
        ]
    ]
  where
    expectFromBytesErr (expectedString,expectedErr,positionErr) ba = do
        let (s', merr, ba') = fromBytes UTF8 ba
        assertEqual "error" expectedErr merr
        assertEqual "remaining" (drop positionErr ba) ba'
        assertEqual "string" expectedString (toList s')

tests :: [TestTree]
tests =
    [ testGroup "Array"
        [ testGroup "Unboxed"
            [ testGroup "UArray(W8)"  (testCollection (Proxy :: Proxy (UArray Word8))  arbitrary)
            , testGroup "UArray(W16)" (testCollection (Proxy :: Proxy (UArray Word16)) arbitrary)
            , testGroup "UArray(W32)" (testCollection (Proxy :: Proxy (UArray Word32)) arbitrary)
            , testGroup "UArray(W64)" (testCollection (Proxy :: Proxy (UArray Word64)) arbitrary)
            , testGroup "UArray(I8)"  (testCollection (Proxy :: Proxy (UArray Int8))   arbitrary)
            , testGroup "UArray(I16)" (testCollection (Proxy :: Proxy (UArray Int16))  arbitrary)
            , testGroup "UArray(I32)" (testCollection (Proxy :: Proxy (UArray Int32))  arbitrary)
            , testGroup "UArray(I64)" (testCollection (Proxy :: Proxy (UArray Int64))  arbitrary)
            , testGroup "UArray(F32)" (testCollection (Proxy :: Proxy (UArray Float))  arbitrary)
            , testGroup "UArray(F64)" (testCollection (Proxy :: Proxy (UArray Double)) arbitrary)
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
            ]
        , testGroup "Boxed"
            [ testGroup "Array(W8)"  (testCollection (Proxy :: Proxy (Array Word8))  arbitrary)
            , testGroup "Array(W16)" (testCollection (Proxy :: Proxy (Array Word16)) arbitrary)
            , testGroup "Array(W32)" (testCollection (Proxy :: Proxy (Array Word32)) arbitrary)
            , testGroup "Array(W64)" (testCollection (Proxy :: Proxy (Array Word64)) arbitrary)
            , testGroup "Array(I8)"  (testCollection (Proxy :: Proxy (Array Int8))   arbitrary)
            , testGroup "Array(I16)" (testCollection (Proxy :: Proxy (Array Int16))  arbitrary)
            , testGroup "Array(I32)" (testCollection (Proxy :: Proxy (Array Int32))  arbitrary)
            , testGroup "Array(I64)" (testCollection (Proxy :: Proxy (Array Int64))  arbitrary)
            , testGroup "Array(F32)" (testCollection (Proxy :: Proxy (Array Float))  arbitrary)
            , testGroup "Array(F64)" (testCollection (Proxy :: Proxy (Array Double)) arbitrary)
            , testGroup "Array(Int)" (testCollection (Proxy :: Proxy (Array Int))  arbitrary)
            , testGroup "Array(Int,Int)" (testCollection (Proxy :: Proxy (Array (Int,Int)))  arbitrary)
            , testGroup "Array(Integer)" (testCollection (Proxy :: Proxy (Array Integer)) arbitrary)
            ]
        ]
    , testGroup "String"
        (  testCollection (Proxy :: Proxy String) arbitraryChar
        <> testStringCases)
    , testGroup "VFS"
        [ testGroup "FilePath" $ testCaseFilePath <> (testPath (arbitrary :: Gen FilePath))
        ]
    , testGroup "Number"
        [ testGroup "Precedence"
            [ testProperty "+ and - (1)" $ \a (b :: Int) (c :: Int) -> (a + b - c) === ((a + b) - c)
            , testProperty "+ and - (2)" $ \a (b :: Int) (c :: Int) -> (a - b + c) === ((a - b) + c)
            , testProperty "+ and * (1)" $ \a b (c :: Int) -> (a + b * c) === (a + (b * c))
            , testProperty "+ and * (2)" $ \a b (c :: Int) -> (a * b + c) === ((a * b) + c)
            , testProperty "- and * (1)" $ \a b (c :: Int) -> (a - b * c) === (a - (b * c))
            , testProperty "- and * (2)" $ \a b (c :: Int) -> (a * b - c) === ((a * b) - c)
            , testProperty "* and ^ (1)" $ \a (Positive b :: Positive Int) (c :: Int) -> (a ^ b * c) === ((a ^ b) * c)
            , testProperty "* and ^ (2)" $ \a (b :: Int) (Positive c :: Positive Int) -> (a * b ^ c) === (a * (b ^ c))
            ]
        ]
    , testGroup "ModifiedUTF8"
        [ testCase "基地系列" $ testCaseModifiedUTF8 ("基地系列", 1, 0x9F)
        , testCase "has null byte" $ testCaseModifiedUTF8 ("let's\0 do \0 it", 5, 0)
        ]
    ]

testCaseModifiedUTF8 :: (String, Int, Word8) -> Assertion
testCaseModifiedUTF8 (str, off, expected)
    | c == expected = return ()
    | otherwise = assertFailure $ "test case error, expecting " <> show expected <> " at " <> show off <> " : " <> show c
  where
    ba = toBytes UTF8 str
    c :: Word8
    c = case (!) ba off of
            Nothing -> error $ "cannot get : " <> show str
            Just c  -> c

main :: IO ()
main = defaultMain $ testGroup "foundation" tests
