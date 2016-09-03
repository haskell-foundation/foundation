{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Main where

import           Test.Tasty
import           Control.Monad
import           Test.QuickCheck.Monadic
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Foundation hiding (second)
import           Foundation.Array
import           Foundation.Collection
import           Foundation.Foreign
import           Foundation.String
import           Foundation.VFS                (Path (..), filename, parent)
import           Foundation.VFS.FilePath
import qualified Prelude

import           ForeignUtils
import           Encoding
import           Parser
import Test.Data.Unicode
import Test.Data.List

import Test.Foundation.Collection

data CharMap = CharMap LUString Prelude.Int
    deriving (Show)

addChar :: Prelude.Int -> Char -> Char
addChar n c = toEnum ((fromEnum c + n) `Prelude.mod` 0x10ffff)


instance Arbitrary CharMap where
    arbitrary =
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

transEq :: Eq a => (t -> t1) -> (t1 -> a) -> (t1 -> a) -> t -> Bool
transEq unWrap f g s =
    let s' = unWrap s in f s' == g s'

--stringEq :: Eq a => (b -> a) -> (String -> b) -> (LString -> a) -> LUString -> Bool
--stringEq back f g s =

assertEq :: (Eq a, Show a) => a -> a -> Bool
assertEq got expected
    | got == expected = True
    | otherwise       = error ("got: " <> show got <> " expected: " <> show expected)

-- | Set in front of tests to make them verbose
qcv :: TestTree -> TestTree
qcv = adjustOption (\(QuickCheckVerbose _) -> QuickCheckVerbose True)

-- | Set the number of tests
qcnSet :: Int -> TestTree -> TestTree
qcnSet n = adjustOption (\(QuickCheckTests _) -> QuickCheckTests n)

-- | Scale the number of tests
qcnScale :: Int -> TestTree -> TestTree
qcnScale n = adjustOption (\(QuickCheckTests actual) -> QuickCheckTests (actual * n))


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
    [ testProperty "buildPath . splitPath == id" $ withElements $ \l -> (buildPath $ splitPath l) === l
    ]
  where
    withElements f = forAll genElement f

testBoxedZippable :: ( Eq (Element col) , Show (Item a), Show (Item b)
                     , BoxedZippable col, Zippable a, Zippable b
                     , Element col ~ (Item a, Item b) )
                  => Proxy a -> Proxy b -> Proxy col -> Gen (Element a) -> Gen (Element b) -> [TestTree]
testBoxedZippable proxyA proxyB proxyCol genElementA genElementB =
    [ testProperty "zip" $ withList2 $ \(as, bs) ->
        toListP proxyCol (zip (fromListP proxyA as) (fromListP proxyB bs)) == zip as bs
    , testProperty "zip . unzip == id" $ withListOfTuples $ \xs ->
        let (as, bs) = unzip (fromListP proxyCol xs)
        in toListP proxyCol (zip (as `asProxyTypeOf` proxyA) (bs `asProxyTypeOf` proxyB)) == xs
    ]
  where
    withList2 = forAll ((,) <$> generateListOfElement genElementA <*> generateListOfElement genElementB)
    withListOfTuples = forAll (generateListOfElement ((,) <$> genElementA <*> genElementB))

testZippable :: ( Eq (Element col), Show (Item col), Show (Item a), Show (Item b)
                , Zippable col, Zippable a, Zippable b )
             => Proxy a -> Proxy b -> Proxy col -> Gen (Element a) -> Gen (Element b) -> Gen (Element col) -> [TestTree]
testZippable proxyA proxyB proxyCol genElementA genElementB genElementCol =
    [ testProperty "zipWith" $ withList2AndE $ \(as, bs, c) ->
        toListP proxyCol (zipWith (const (const c)) (fromListP proxyA as) (fromListP proxyB bs)
            ) == Prelude.replicate (Prelude.min (length as) (length bs)) c
    ]
  where
    withList2AndE = forAll ( (,,) <$> generateListOfElement genElementA <*> generateListOfElement genElementB
                                  <*> genElementCol )

testZippableProps :: (Eq (Item a), Eq (Item b), Show (Item a), Show (Item b), Zippable a, Zippable b)
                  => Proxy a -> Proxy b -> Gen (Element a) -> Gen (Element b) -> [TestTree]
testZippableProps proxyA proxyB genElementA genElementB =
    [ testProperty "zipWith ⊥ [] xs == []" $ withList $ \as ->
        toListP proxyA (zipWith undefined [] (fromListP proxyA as)) == []
    , testProperty "zipWith f a b == zipWith (flip f) b a" $ withList2 $ \(as, bs) ->
        let f = ignore1
            as' = fromListP proxyA as
            bs' = fromListP proxyB bs
        in toListP proxyB (zipWith f as' bs')
            == toListP proxyB (zipWith (flip f) bs' as')
    , testProperty "zipWith3 f […] xs == zipWith id (zipWith f […]) xs)" $ withList2 $ \(as, bs) ->
        let f = ignore2
            as' = fromListP proxyA as
            bs' = fromListP proxyB bs
        in toListP proxyB (zipWith3 f as' as' bs')
            == Prelude.zipWith id (zipWith f as as) bs
    , testProperty "zipWith4 f […] xs == zipWith id (zipWith3 f […]) xs)" $ withList2 $ \(as, bs) ->
        let f = ignore3
            as' = fromListP proxyA as
            bs' = fromListP proxyB bs
        in toListP proxyB (zipWith4 f as' as' as' bs')
            == Prelude.zipWith id (zipWith3 f as as as) bs
    , testProperty "zipWith5 f […] xs == zipWith id (zipWith4 f […]) xs)" $ withList2 $ \(as, bs) ->
        let f = ignore4
            as' = fromListP proxyA as
            bs' = fromListP proxyB bs
        in toListP proxyB (zipWith5 f as' as' as' as' bs')
            == Prelude.zipWith id (zipWith4 f as as as as) bs
    , testProperty "zipWith6 f […] xs == zipWith id (zipWith5 f […]) xs)" $ withList2 $ \(as, bs) ->
        let f = ignore5
            as' = fromListP proxyA as
            bs' = fromListP proxyB bs
        in toListP proxyB (zipWith6 f as' as' as' as' as' bs')
            == Prelude.zipWith id (zipWith5 f as as as as as) bs
    , testProperty "zipWith7 f […] xs == zipWith id (zipWith6 f […]) xs)" $ withList2 $ \(as, bs) ->
        let f = ignore6
            as' = fromListP proxyA as
            bs' = fromListP proxyB bs
        in toListP proxyB (zipWith7 f as' as' as' as' as' as' bs')
            == Prelude.zipWith id (zipWith6 f as as as as as as) bs
    ]
  where
    -- ignore the first n arguments
    ignore1 = flip const
    ignore2 = const . ignore1
    ignore3 = const . ignore2
    ignore4 = const . ignore3
    ignore5 = const . ignore4
    ignore6 = const . ignore5
    withList  = forAll (generateListOfElement genElementA)
    withList2 = forAll ((,) <$> generateListOfElement genElementA <*> generateListOfElement genElementB)

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
        [ testProperty "fromBytes . toBytes == valid" $ \(LUString l) ->
            let s = fromList l
             in (fromBytes UTF8 $ toBytes UTF8 s) === (s, Nothing, mempty)
        , testProperty "Streaming" $ \(LUString l, randomInts) ->
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
        , testCollection "Bitmap"  (Proxy :: Proxy (Bitmap))  arbitrary
        ]
    , testGroup "String" $
        [  testCollection "UTF8" (Proxy :: Proxy String) genUnicodeChar ]
        <> testStringCases
        <> [ testGroup "Encoding Sample0" (testEncodings sample0)
           , testGroup "Encoding Sample1" (testEncodings sample1)
           , testGroup "Encoding Sample2" (testEncodings sample2)
           ]
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
        [ testCase "The foundation Serie" $ testCaseModifiedUTF8 "基地系列" "基地系列"
        , testCase "has null bytes" $ testCaseModifiedUTF8 "let's\0 do \0 it" "let's\0 do \0 it"
        , testCase "Vincent's special" $ testCaseModifiedUTF8 "abc\0안, 蠀\0, ☃" "abc\0안, 蠀\0, ☃"
        , testCase "Long string" $ testCaseModifiedUTF8
              "this is only a simple string but quite longer than the 64 bytes used in the modified UTF8 parser"
              "this is only a simple string but quite longer than the 64 bytes used in the modified UTF8 parser"
        ]
    , testGroup "BoxedZippable"
        [ testGroup "Array"
            [ testGroup "from Array Int"
                ( testBoxedZippable
                    (Proxy :: Proxy (Array Int)) (Proxy :: Proxy (Array Int))
                    (Proxy :: Proxy (Array (Int, Int))) arbitrary arbitrary )
            , testGroup "from String"
                ( testBoxedZippable
                    (Proxy :: Proxy String) (Proxy :: Proxy String)
                    (Proxy :: Proxy (Array (Char, Char))) arbitrary arbitrary )
            , testGroup "from String and Array Char"
                ( testBoxedZippable
                    (Proxy :: Proxy String) (Proxy :: Proxy (Array Char))
                    (Proxy :: Proxy (Array (Char, Char))) arbitrary arbitrary )
            , testGroup "from Array Int and Array Char"
                ( testBoxedZippable
                    (Proxy :: Proxy (Array Int)) (Proxy :: Proxy (Array Char))
                    (Proxy :: Proxy (Array (Int, Char))) arbitrary arbitrary )
            ]
        ]
    , testGroup "Zippable"
        [ testGroup "String"
            [ testGroup "from String"
                ( testZippable
                    (Proxy :: Proxy String) (Proxy :: Proxy String)
                    (Proxy :: Proxy String) arbitrary arbitrary arbitrary )
            , testGroup "from Array Char"
                ( testZippable
                    (Proxy :: Proxy (Array Char)) (Proxy :: Proxy (Array Char))
                    (Proxy :: Proxy String) arbitrary arbitrary arbitrary )
            , testGroup "from UArray Word8 and Array Int"
                ( testZippable
                    (Proxy :: Proxy (UArray Word8)) (Proxy :: Proxy (Array Int))
                    (Proxy :: Proxy String) arbitrary arbitrary arbitrary )
            ]
        , testGroup "Array"
            [ testGroup "from String"
                ( testZippable
                    (Proxy :: Proxy String) (Proxy :: Proxy String)
                    (Proxy :: Proxy (Array Int)) arbitrary arbitrary arbitrary )
            , testGroup "from Array Char"
                ( testZippable
                    (Proxy :: Proxy (Array Char)) (Proxy :: Proxy (Array Char))
                    (Proxy :: Proxy (Array Char)) arbitrary arbitrary arbitrary )
            , testGroup "from UArray Word8 and Array Int"
                ( testZippable
                    (Proxy :: Proxy (UArray Word8)) (Proxy :: Proxy (Array Int))
                    (Proxy :: Proxy (Array Int)) arbitrary arbitrary arbitrary )
            ]
        , testGroup "UArray"
            [ testGroup "from String"
                ( testZippable
                    (Proxy :: Proxy String) (Proxy :: Proxy String)
                    (Proxy :: Proxy (UArray Word8)) arbitrary arbitrary arbitrary )
            , testGroup "from Array Char"
                ( testZippable
                    (Proxy :: Proxy (Array Char)) (Proxy :: Proxy (Array Char))
                    (Proxy :: Proxy (UArray Word16)) arbitrary arbitrary arbitrary )
            , testGroup "from UArray Word8 and Array Int"
                ( testZippable
                    (Proxy :: Proxy (UArray Word8)) (Proxy :: Proxy (Array Int))
                    (Proxy :: Proxy (UArray Word32)) arbitrary arbitrary arbitrary )
            ]
        , testGroup "Properties"
            ( testZippableProps (Proxy :: Proxy (Array Int)) (Proxy :: Proxy (Array Char))
                arbitrary arbitrary )
        ]
    , testParsers
    ]

testCaseModifiedUTF8 :: [Char] -> String -> Assertion
testCaseModifiedUTF8 ghcStr str
    | ghcStr == fStr = return ()
    | otherwise      = assertFailure $
        "expecting: " <> show ghcStr <> " received: " <> show fStr
  where
    fStr :: [Char]
    fStr = toList str

main :: IO ()
main = defaultMain $ testGroup "foundation" tests
