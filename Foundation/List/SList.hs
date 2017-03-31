-- |
-- Module      : Foundation.List.SList
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- A Nat-sized list abstraction
--
-- Using this module is limited to GHC 7.10 and above.
--
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE UndecidableInstances      #-}
module Foundation.List.SList
    ( SList
    , toSList
    , unSList
    , length
    , create
    , createFrom
    , empty
    , singleton
    , uncons
    , cons
    , map
    , elem
    , foldl
    , append
    , minimum
    , maximum
    , head
    , tail
    , take
    , drop
    , zip, zip3, zip4, zip5
    , zipWith, zipWith3, zipWith4, zipWith5
    , replicateM
    ) where

import           GHC.TypeLits
import           Data.Proxy
import           Prelude       hiding ( map, zip, zip3, zipWith, zipWith3
                                      , foldl
                                      , length, maximum, minimum, head, tail, take, drop, elem)
import qualified Prelude
import qualified Control.Monad as M (replicateM)

impossible :: a
impossible = error "SList: internal error: the impossible happened"

newtype SList (n :: Nat) a = SList { unSList :: [a] }

toSList :: forall (n :: Nat) a . KnownNat n => [a] -> Maybe (SList n a)
toSList l
    | expected == fromIntegral (Prelude.length l) = Just (SList l)
    | otherwise                                   = Nothing
  where
    expected = natVal (Proxy :: Proxy n)

replicateM :: forall (n :: Nat) m a . (n <= 0x100000, Monad m, KnownNat n) => m a -> m (SList n a)
replicateM action = SList <$> M.replicateM (Prelude.fromIntegral $ natVal (Proxy :: Proxy n)) action

uncons :: CmpNat n 0 ~ 'GT => SList n a -> (a, SList (n-1) a)
uncons (SList (x:xs)) = (x, SList xs)
uncons _ = impossible

cons :: a -> SList n a -> SList (n+1) a
cons a (SList l) = SList (a : l)

empty :: SList 0 a
empty = SList []

length :: forall a (n :: Nat) . KnownNat n => SList n a -> Int
length _ = fromIntegral $ natVal (Proxy :: Proxy n)

create :: forall a (n :: Nat) . KnownNat n => (Integer -> a) -> SList n a
create f = SList $ Prelude.map f [0..(len-1)]
  where
    len = natVal (Proxy :: Proxy n)

createFrom :: forall a (n :: Nat) (start :: Nat) . (KnownNat n, KnownNat start)
           => Proxy start -> (Integer -> a) -> SList n a
createFrom p f = SList $ Prelude.map f [idx..(idx+len)]
  where
    len = natVal (Proxy :: Proxy n)
    idx = natVal p

singleton :: a -> SList 1 a
singleton a = SList [a]

elem :: Eq a => a -> SList n a -> Bool
elem a (SList l) = Prelude.elem a l

append :: SList n a -> SList m a -> SList (n+m) a
append (SList l1) (SList l2) = SList (l1 ++ l2)

maximum :: (Ord a, CmpNat n 0 ~ 'GT) => SList n a -> a
maximum (SList l) = Prelude.maximum l

minimum :: (Ord a, CmpNat n 0 ~ 'GT) => SList n a -> a
minimum (SList l) = Prelude.minimum l

head :: CmpNat n 0 ~ 'GT => SList n a -> a
head (SList (x:_)) = x
head _ = impossible

tail :: CmpNat n 0 ~ 'GT => SList n a -> SList (n-1) a
tail (SList (_:xs)) = SList xs
tail _ = impossible

take :: forall a (m :: Nat) (n :: Nat) . (KnownNat m, m <= n) => SList n a -> SList m a
take (SList l) = SList (Prelude.take n l)
  where n = fromIntegral $ natVal (Proxy :: Proxy m)

drop :: forall a d (m :: Nat) (n :: Nat) . (KnownNat d, (n - m) ~ d, m <= n) => SList n a -> SList m a
drop (SList l) = SList (Prelude.drop n l)
  where n = fromIntegral $ natVal (Proxy :: Proxy d)

map :: (a -> b) -> SList n a -> SList n b
map f (SList l) = SList (Prelude.map f l)

foldl :: (b -> a -> b) -> b -> SList n a -> b
foldl f acc (SList l) = Prelude.foldl f acc l

zip :: SList n a -> SList n b -> SList n (a,b)
zip (SList l1) (SList l2) = SList (Prelude.zip l1 l2)

zip3 :: SList n a -> SList n b -> SList n c -> SList n (a,b,c)
zip3 (SList x1) (SList x2) (SList x3) = SList (loop x1 x2 x3)
  where loop (l1:l1s) (l2:l2s) (l3:l3s) = (l1,l2,l3) : loop l1s l2s l3s
        loop []       _        _        = []
        loop _        _        _        = impossible

zip4 :: SList n a -> SList n b -> SList n c -> SList n d -> SList n (a,b,c,d)
zip4 (SList x1) (SList x2) (SList x3) (SList x4) = SList (loop x1 x2 x3 x4)
  where loop (l1:l1s) (l2:l2s) (l3:l3s) (l4:l4s) = (l1,l2,l3,l4) : loop l1s l2s l3s l4s
        loop []       _        _        _        = []
        loop _        _        _        _        = impossible

zip5 :: SList n a -> SList n b -> SList n c -> SList n d -> SList n e -> SList n (a,b,c,d,e)
zip5 (SList x1) (SList x2) (SList x3) (SList x4) (SList x5) = SList (loop x1 x2 x3 x4 x5)
  where loop (l1:l1s) (l2:l2s) (l3:l3s) (l4:l4s) (l5:l5s) = (l1,l2,l3,l4,l5) : loop l1s l2s l3s l4s l5s
        loop []       _        _        _        _        = []
        loop _        _        _        _        _        = impossible

zipWith :: (a -> b -> x) -> SList n a -> SList n b -> SList n x
zipWith f (SList (v1:vs)) (SList (w1:ws)) = SList (f v1 w1 : unSList (zipWith f (SList vs) (SList ws)))
zipWith _ (SList [])       _ = SList []
zipWith _ _                _ = impossible

zipWith3 :: (a -> b -> c -> x)
         -> SList n a
         -> SList n b
         -> SList n c
         -> SList n x
zipWith3 f (SList (v1:vs)) (SList (w1:ws)) (SList (x1:xs)) =
    SList (f v1 w1 x1 : unSList (zipWith3 f (SList vs) (SList ws) (SList xs)))
zipWith3 _ (SList []) _       _ = SList []
zipWith3 _ _          _       _ = impossible

zipWith4 :: (a -> b -> c -> d -> x)
         -> SList n a
         -> SList n b
         -> SList n c
         -> SList n d
         -> SList n x
zipWith4 f (SList (v1:vs)) (SList (w1:ws)) (SList (x1:xs)) (SList (y1:ys)) =
    SList (f v1 w1 x1 y1 : unSList (zipWith4 f (SList vs) (SList ws) (SList xs) (SList ys)))
zipWith4 _ (SList []) _       _       _ = SList []
zipWith4 _ _          _       _       _ = impossible

zipWith5 :: (a -> b -> c -> d -> e -> x)
         -> SList n a
         -> SList n b
         -> SList n c
         -> SList n d
         -> SList n e
         -> SList n x
zipWith5 f (SList (v1:vs)) (SList (w1:ws)) (SList (x1:xs)) (SList (y1:ys)) (SList (z1:zs)) =
    SList (f v1 w1 x1 y1 z1 : unSList (zipWith5 f (SList vs) (SList ws) (SList xs) (SList ys) (SList zs)))
zipWith5 _ (SList []) _       _       _       _ = SList []
zipWith5 _ _          _       _       _       _ = impossible
