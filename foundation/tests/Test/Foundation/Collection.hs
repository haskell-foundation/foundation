{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Test.Foundation.Collection
    ( testCollection
    , fromListP
    , toListP
    ) where

import qualified Prelude

import Imports

import Foundation
import Foundation.Collection
import Test.Data.List

-- | internal helper to convert a list of element into a collection
--
fromListP :: (IsList c, Item c ~ Element c) => Proxy c -> [Element c] -> c
fromListP p = \x -> asProxyTypeOf (fromList x) p

-- | internal helper to convert a given Collection into a list of its element
--
toListP :: (IsList c, Item c ~ Element c) => Proxy c -> c -> [Element c]
toListP p x = toList (asProxyTypeOf x p)

-- | test property equality for the given Collection
--
-- This does to enforce
testEquality :: ( Show e
                , Eq e, Eq a
                , Element a ~ e
                , IsList a, Item a ~ Element a
                )
             => Proxy a
             -> Gen e
             -> TestTree
testEquality proxy genElement = testGroup "equality"
    [ testProperty "x == x" $ withElements $ \l -> let col = fromListP proxy l in col == col
    , testProperty "x == y" $ with2Elements $ \(l1, l2) ->
        (fromListP proxy l1 == fromListP proxy l2) == (l1 == l2)
    ]
  where
    withElements f = forAll (generateListOfElement genElement) f
    with2Elements f = forAll ((,) <$> generateListOfElement genElement <*> generateListOfElement genElement) f


testOrdering :: ( Show e
                , Ord a, Ord e
                , Element a ~ e
                , IsList a, Item a ~ Element a
                )
             => Proxy a
             -> Gen e
             -> TestTree
testOrdering proxy genElement = testGroup "ordering"
    [ testProperty "x `compare` y" $ with2Elements $ \(l1, l2) ->
        (fromListP proxy l1 `compare` fromListP proxy l2) == (l1 `compare` l2)
    ]
  where
    with2Elements f = forAll ((,) <$> generateListOfElement genElement <*> generateListOfElement genElement) f

testIsList :: ( Show e
              , Eq e, Eq a
              , Element a ~ e
              , IsList a, Item a ~ Element a
              )
           => Proxy a
           -> Gen e
           -> TestTree
testIsList proxy genElement = testGroup "IsList"
    [ testProperty "fromList . toList == id" $ withElements $ \l -> (toList $ fromListP proxy l) === l
    ]
  where
    withElements f = forAll (generateListOfElement genElement) f

-- | group of all the property a given collection should have
--
-- > splitAt == (take, drop)
--
-- > revSplitAt == (revTake, revDrop)
--
-- > c == [Element c]
--
testSequentialProperties :: (Show a, Sequential a, Eq a, e ~ Item a) => Proxy a -> Gen e -> TestTree
testSequentialProperties proxy genElement = testGroup "Properties"
    [ testProperty "splitAt == (take, drop)" $ withCollection2 $ \(col, n) ->
        splitAt n col == (take n col, drop n col)
    , testProperty "revSplitAt == (revTake, revDrop)" $ withCollection2 $ \(col, n) ->
        revSplitAt n col == (revTake n col, revDrop n col)
    ]
  where
    withCollection2 f = forAll ((,) <$> (fromListP proxy <$> generateListOfElement genElement) <*> (CountOf <$> arbitrary)) f

testMonoid :: ( Show a, Show e
              , Eq a, Eq e
              , Monoid a
              , Element a ~ e, IsList a, Item a ~ Element a
              )
           => Proxy a
           -> Gen e
           -> TestTree
testMonoid proxy genElement = testGroup "Monoid"
    [ testProperty "mempty <> x == x" $ withElements $ \l -> let col = fromListP proxy l in (col <> mempty) === col
    , testProperty "x <> mempty == x" $ withElements $ \l -> let col = fromListP proxy l in (mempty <> col) === col
    , testProperty "x1 <> x2 == x1|x2" $ with2Elements $ \(l1,l2) ->
        (fromListP proxy l1 <> fromListP proxy l2) === fromListP proxy (l1 <> l2)
    , testProperty "mconcat [map fromList [e]] = fromList (concat [e])" $ withNElements $ \l ->
        mconcat (fmap (fromListP proxy) l) === fromListP proxy (mconcat l)
    ]
  where
    withElements f = forAll (generateListOfElement genElement) f
    with2Elements f = forAll ((,) <$> generateListOfElement genElement <*> generateListOfElement genElement) f
    withNElements f = forAll (generateListOfElementMaxN 5 (generateListOfElement genElement)) f

testCollection :: ( Sequential a
                  , Show a, Show (Element a)
                  , Eq (Element a)
                  , Ord a, Ord (Item a)
                  )
               => String
               -> Proxy a
               -> Gen (Element a)
               -> TestTree
testCollection name proxy genElement = testGroup name
    [ testEquality proxy genElement
    , testOrdering proxy genElement
    , testIsList   proxy genElement
    , testMonoid   proxy genElement
    , testCollectionOps proxy genElement
    , testSequentialOps proxy genElement
    ]

fromListNonEmptyP :: Collection a => Proxy a -> NonEmpty [Element a] -> NonEmpty a
fromListNonEmptyP proxy = nonEmpty_ . fromListP proxy . getNonEmpty

testCollectionOps :: ( Collection a
                     , Show a, Show (Element a)
                     , Eq (Element a)
                     , Ord a, Ord (Item a)
                     )
                  => Proxy a
                  -> Gen (Element a)
                  -> TestTree
testCollectionOps proxy genElement = testGroup "Collection"
    [ testProperty "length" $ withElements $ \l -> (length $ fromListP proxy l) === length l
    , testProperty "elem" $ withListAndElement $ \(l,e) -> elem e (fromListP proxy l) == elem e l
    , testProperty "notElem" $ withListAndElement $ \(l,e) -> notElem e (fromListP proxy l) == notElem e l
    , testProperty "minimum" $ withNonEmptyElements $ \els -> minimum (fromListNonEmptyP proxy els) === minimum els
    , testProperty "maximum" $ withNonEmptyElements $ \els -> maximum (fromListNonEmptyP proxy els) === maximum els
    , testProperty "all" $ withListAndElement $ \(l, e) ->
        all (/= e) (fromListP proxy l) == all (/= e) l &&
        all (== e) (fromListP proxy l) == all (== e) l
    , testProperty "any" $ withListAndElement $ \(l, e) ->
        any (/= e) (fromListP proxy l) == any (/= e) l &&
        any (== e) (fromListP proxy l) == any (== e) l
    ]
  where
    withElements f = forAll (generateListOfElement genElement) f
    withListAndElement = forAll ((,) <$> generateListOfElement genElement <*> genElement)
    withNonEmptyElements f = forAll (generateNonEmptyListOfElement 80 genElement) f

testSplitOn :: ( Sequential a
               , Show a, Show (Element a)
               , Eq (Element a)
               , Eq a, Ord a, Ord (Item a), Show a
               )
              => Proxy a -> (Element a -> Bool) -> a
              -> TestTree
testSplitOn _ predicate col = testCase "splitOn (const True) mempty == [mempty]" $
    assertEq' (splitOn predicate col) [col]

testSequentialOps :: ( Sequential a
                     , Show a, Show (Element a)
                     , Eq (Element a)
                     , Eq a, Ord a, Ord (Item a), Show a
                     )
                  => Proxy a
                  -> Gen (Element a)
                  -> TestTree
testSequentialOps proxy genElement = testGroup "Sequential"
    [ testProperty "take" $ withElements2 $ \(l, n) -> toList (take n $ fromListP proxy l) === (take n) l
    , testProperty "drop" $ withElements2 $ \(l, n) -> toList (drop n $ fromListP proxy l) === (drop n) l
    , testProperty "splitAt" $ withElements2 $ \(l, n) -> toList2 (splitAt n $ fromListP proxy l) === (splitAt n) l
    , testProperty "revTake" $ withElements2 $ \(l, n) -> toList (revTake n $ fromListP proxy l) === (revTake n) l
    , testProperty "revDrop" $ withElements2 $ \(l, n) -> toList (revDrop n $ fromListP proxy l) === (revDrop n) l
    , testProperty "revSplitAt" $ withElements2 $ \(l, n) -> toList2 (revSplitAt n $ fromListP proxy l) === (revSplitAt n) l
    , testProperty "break" $ withElements2E $ \(l, c) -> toList2 (break (== c) $ fromListP proxy l) === (break (== c)) l
    , testProperty "breakEnd" $ withElements2E $ \(l, c) -> toList2 (breakEnd (== c) $ fromListP proxy l) === (breakEnd (== c)) l
    , testProperty "breakElem" $ withElements2E $ \(l, c) -> toList2 (breakElem c $ fromListP proxy l) === (breakElem c) l
    , testProperty "span" $ withElements2E $ \(l, c) -> toList2 (span (== c) $ fromListP proxy l) === (span (== c)) l
    , testProperty "filter" $ withElements2E $ \(l, c) -> toList (filter (== c) $ fromListP proxy l) === (filter (== c)) l
    , testProperty "partition" $ withElements2E $ \(l, c) -> toList2 (partition (== c) $ fromListP proxy l) === (partition (== c)) l
    , testProperty "snoc" $ withElements2E $ \(l, c) -> toList (snoc (fromListP proxy l) c) === (l <> [c])
    , testProperty "cons" $ withElements2E $ \(l, c) -> toList (cons c (fromListP proxy l)) === (c : l)
    , testProperty "unsnoc" $ withElements $ \l -> fmap toListFirst (unsnoc (fromListP proxy l)) === unsnoc l
    , testProperty "uncons" $ withElements $ \l -> fmap toListSecond (uncons (fromListP proxy l)) === uncons l
    , testProperty "head" $ withNonEmptyElements $ \els -> head (fromListNonEmptyP proxy els) === head els
    , testProperty "last" $ withNonEmptyElements $ \els -> last (fromListNonEmptyP proxy els) === last els
    , testProperty "tail" $ withNonEmptyElements $ \els -> toList (tail $ fromListNonEmptyP proxy els) === tail els
    , testProperty "init" $ withNonEmptyElements $ \els -> toList (init $ fromListNonEmptyP proxy els) === init els
    , testProperty "splitOn" $ withElements2E $ \(l, ch) ->
         fmap toList (splitOn (== ch) (fromListP proxy l)) === splitOn (== ch) l
    , testSplitOn proxy (const True) mempty
    , testProperty "intercalate c (splitOn (c ==) col) == col" $ withElements2E $ \(c, ch) ->
        intercalate [ch] (splitOn (== ch) c) === c
    , testProperty "intercalate c (splitOn (c ==) (col ++ [c]) == (col ++ [c])" $ withElements2E $ \(c, ch) ->
        intercalate [ch] (splitOn (== ch) $ snoc c ch) === (snoc c ch)
    , testProperty "intercalate c (splitOn (c ==) (col ++ [c,c]) == (col ++ [c,c])" $ withElements2E $ \(c, ch) ->
        intercalate [ch] (splitOn (== ch) $ snoc (snoc c ch) ch) === (snoc (snoc c ch) ch)
    , testProperty "intersperse" $ withElements2E $ \(l, c) ->
        toList (intersperse c (fromListP proxy l)) === intersperse c l
    , testProperty "intercalate" $ withElements2E $ \(l, c) ->
        let ls = Prelude.replicate 5 l
            cs = Prelude.replicate 5 c
        in toList (intercalate (fromListP proxy cs) (fromListP proxy <$> ls)) === intercalate cs ls
    , testProperty "sortBy" $ withElements $ \l ->
        (sortBy compare $ fromListP proxy l) === fromListP proxy (sortBy compare l)
    , testProperty "reverse" $ withElements $ \l ->
        (reverse $ fromListP proxy l) === fromListP proxy (reverse l)
    -- stress slicing
    , testProperty "take . take" $ withElements3 $ \(l, n1, n2) -> toList (take n2 $ take n1 $ fromListP proxy l) === (take n2 $ take n1 l)
    , testProperty "drop . take" $ withElements3 $ \(l, n1, n2) -> toList (drop n2 $ take n1 $ fromListP proxy l) === (drop n2 $ take n1 l)
    , testProperty "drop . drop" $ withElements3 $ \(l, n1, n2) -> toList (drop n2 $ drop n1 $ fromListP proxy l) === (drop n2 $ drop n1 l)
    , testProperty "drop . take" $ withElements3 $ \(l, n1, n2) -> toList (drop n2 $ take n1 $ fromListP proxy l) === (drop n2 $ take n1 l)
    , testProperty "second take . splitAt" $ withElements3 $ \(l, n1, n2) ->
        (toList2 $ (second (take n1) . splitAt n2) $ fromListP proxy l) === (second (take n1) . splitAt n2) l
    , testSequentialProperties proxy genElement
    , testGroup "isSuffixOf"
        [ testProperty "collection + sub" $ withElements2 $ \(l1, n) ->
            let c1 = fromListP proxy l1 in isSuffixOf (revTake n c1) c1 === isSuffixOf (revTake n l1) l1
        , testProperty "2 collections" $ with2Elements $ \(l1, l2) -> isSuffixOf (fromListP proxy l1) (fromListP proxy l2) === isSuffixOf l1 l2
        , testProperty "collection + empty" $ withElements $ \l1 ->
            isSuffixOf (fromListP proxy []) (fromListP proxy l1) === isSuffixOf [] l1
        ]
    , testGroup "isPrefixOf"
        [ testProperty "collection + sub" $ withElements2 $ \(l1, n) ->
            let c1 = fromListP proxy l1 in isPrefixOf (take n c1) c1 === isPrefixOf (take n l1) l1
        , testProperty "2 collections" $ with2Elements $ \(l1, l2) -> isPrefixOf (fromListP proxy l1) (fromListP proxy l2) === isPrefixOf l1 l2
        , testProperty "collection + empty" $ withElements $ \l1 ->
            isPrefixOf (fromListP proxy []) (fromListP proxy l1) === isPrefixOf [] l1
        ]
    , testGroup "isInfixOf"
        [ testProperty "b isInfixOf 'a b c'" $ with3Elements $ \(a, b, c) -> 
            isInfixOf (toCol b) (toCol a <> toCol b <> toCol c)
        , testProperty "the reverse is typically not an infix" $ withElements $ \a' ->
            let a = toCol a'; rev = reverse a in isInfixOf rev a === (a == rev)
        ]
    ]
{-
    , testProperty "imap" $ \(CharMap (LUString u) i) ->
        (imap (addChar i) (fromList u) :: String) `assertEq` fromList (Prelude.map (addChar i) u)
    ]
-}
  where
    toCol = fromListP proxy
    toList2 (x,y) = (toList x, toList y)
    toListFirst (x,y) = (toList x, y)
    toListSecond (x,y) = (x, toList y)
    withElements f = forAll (generateListOfElement genElement) f
    with2Elements f = forAll ((,) <$> generateListOfElement genElement <*> generateListOfElement genElement) f
    with3Elements f = forAll ((,,) <$> generateListOfElement genElement <*> generateListOfElement genElement <*> generateListOfElement genElement) f
    withElements2 f = forAll ((,) <$> generateListOfElement genElement <*> (CountOf <$> arbitrary)) f
    withElements3 f = forAll ((,,) <$> generateListOfElement genElement <*> (CountOf <$> arbitrary) <*> (CountOf <$> arbitrary)) f
    withElements2E f = forAll ((,) <$> generateListOfElement genElement <*> genElement) f
    withNonEmptyElements f = forAll (generateNonEmptyListOfElement 80 genElement) f
