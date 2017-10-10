-- |
-- Module      : Basement.Sized.List
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
{-# LANGUAGE UndecidableInstances      #-}
module Basement.Sized.List
    ( ListN
    , toListN
    , unListN
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
    , foldl'
    , foldr
    , append
    , minimum
    , maximum
    , head
    , tail
    , take
    , drop
    , splitAt
    , zip, zip3, zip4, zip5
    , zipWith, zipWith3, zipWith4, zipWith5
    , replicate
    -- * Applicative And Monadic
    , replicateM
    , mapM
    , mapM_
    ) where

import           Data.Proxy
import qualified Data.List
import           Basement.Compat.Base
import           Basement.Nat
import           Basement.NormalForm
import           Basement.Numerical.Additive
import           Basement.Numerical.Subtractive
import qualified Prelude
import qualified Control.Monad as M (replicateM, mapM, mapM_)

impossible :: a
impossible = error "ListN: internal error: the impossible happened"

newtype ListN (n :: Nat) a = ListN { unListN :: [a] }
    deriving (Eq,Ord)

instance Show a => Show (ListN n a) where
    show (ListN l) = show l

instance NormalForm a => NormalForm (ListN n a) where
    toNormalForm (ListN l) = toNormalForm l

toListN :: forall (n :: Nat) a . (KnownNat n, NatWithinBound Int n) => [a] -> Maybe (ListN n a)
toListN l
    | expected == Prelude.fromIntegral (Prelude.length l) = Just (ListN l)
    | otherwise                                           = Nothing
  where
    expected = natValInt (Proxy :: Proxy n)

replicateM :: forall (n :: Nat) m a . (NatWithinBound Int n, Monad m, KnownNat n) => m a -> m (ListN n a)
replicateM action = ListN <$> M.replicateM (Prelude.fromIntegral $ natVal (Proxy :: Proxy n)) action

mapM :: Monad m => (a -> m b) -> ListN n a -> m (ListN n b)
mapM f (ListN l) = ListN <$> M.mapM f l

mapM_ :: Monad m => (a -> m b) -> ListN n a -> m ()
mapM_ f (ListN l) = M.mapM_ f l

replicate :: forall (n :: Nat) a . (NatWithinBound Int n, KnownNat n) => a -> ListN n a
replicate a = ListN $ Prelude.replicate (Prelude.fromIntegral $ natVal (Proxy :: Proxy n)) a

uncons :: CmpNat n 0 ~ 'GT => ListN n a -> (a, ListN (n-1) a)
uncons (ListN (x:xs)) = (x, ListN xs)
uncons _ = impossible

cons :: a -> ListN n a -> ListN (n+1) a
cons a (ListN l) = ListN (a : l)

empty :: ListN 0 a
empty = ListN []

length :: forall a (n :: Nat) . (KnownNat n, NatWithinBound Int n) => ListN n a -> Int
length _ = natValInt (Proxy :: Proxy n)

create :: forall a (n :: Nat) . KnownNat n => (Integer -> a) -> ListN n a
create f = ListN $ Prelude.map f [0..(len-1)]
  where
    len = natVal (Proxy :: Proxy n)

createFrom :: forall a (n :: Nat) (start :: Nat) . (KnownNat n, KnownNat start)
           => Proxy start -> (Integer -> a) -> ListN n a
createFrom p f = ListN $ Prelude.map f [idx..(idx+len-1)]
  where
    len = natVal (Proxy :: Proxy n)
    idx = natVal p

singleton :: a -> ListN 1 a
singleton a = ListN [a]

elem :: Eq a => a -> ListN n a -> Bool
elem a (ListN l) = Prelude.elem a l

append :: ListN n a -> ListN m a -> ListN (n+m) a
append (ListN l1) (ListN l2) = ListN (l1 <> l2)

maximum :: (Ord a, CmpNat n 0 ~ 'GT) => ListN n a -> a
maximum (ListN l) = Prelude.maximum l

minimum :: (Ord a, CmpNat n 0 ~ 'GT) => ListN n a -> a
minimum (ListN l) = Prelude.minimum l

head :: CmpNat n 0 ~ 'GT => ListN n a -> a
head (ListN (x:_)) = x
head _ = impossible

tail :: CmpNat n 0 ~ 'GT => ListN n a -> ListN (n-1) a
tail (ListN (_:xs)) = ListN xs
tail _ = impossible

take :: forall a (m :: Nat) (n :: Nat) . (KnownNat m, NatWithinBound Int m, m <= n) => ListN n a -> ListN m a
take (ListN l) = ListN (Prelude.take n l)
  where n = natValInt (Proxy :: Proxy m)

drop :: forall a d (m :: Nat) (n :: Nat) . (KnownNat d, NatWithinBound Int d, (n - m) ~ d, m <= n) => ListN n a -> ListN m a
drop (ListN l) = ListN (Prelude.drop n l)
  where n = natValInt (Proxy :: Proxy d)

splitAt :: forall a d (m :: Nat) (n :: Nat) . (KnownNat d, NatWithinBound Int d, (n - m) ~ d, m <= n) => ListN n a -> (ListN m a, ListN (n-m) a)
splitAt (ListN l) = let (l1, l2) = Prelude.splitAt n l in (ListN l1, ListN l2)
  where n = natValInt (Proxy :: Proxy d)

map :: (a -> b) -> ListN n a -> ListN n b
map f (ListN l) = ListN (Prelude.map f l)

foldl :: (b -> a -> b) -> b -> ListN n a -> b
foldl f acc (ListN l) = Prelude.foldl f acc l

foldl' :: (b -> a -> b) -> b -> ListN n a -> b
foldl' f acc (ListN l) = Data.List.foldl' f acc l

foldr :: (a -> b -> b) -> b -> ListN n a -> b
foldr f acc (ListN l) = Prelude.foldr f acc l

zip :: ListN n a -> ListN n b -> ListN n (a,b)
zip (ListN l1) (ListN l2) = ListN (Prelude.zip l1 l2)

zip3 :: ListN n a -> ListN n b -> ListN n c -> ListN n (a,b,c)
zip3 (ListN x1) (ListN x2) (ListN x3) = ListN (loop x1 x2 x3)
  where loop (l1:l1s) (l2:l2s) (l3:l3s) = (l1,l2,l3) : loop l1s l2s l3s
        loop []       _        _        = []
        loop _        _        _        = impossible

zip4 :: ListN n a -> ListN n b -> ListN n c -> ListN n d -> ListN n (a,b,c,d)
zip4 (ListN x1) (ListN x2) (ListN x3) (ListN x4) = ListN (loop x1 x2 x3 x4)
  where loop (l1:l1s) (l2:l2s) (l3:l3s) (l4:l4s) = (l1,l2,l3,l4) : loop l1s l2s l3s l4s
        loop []       _        _        _        = []
        loop _        _        _        _        = impossible

zip5 :: ListN n a -> ListN n b -> ListN n c -> ListN n d -> ListN n e -> ListN n (a,b,c,d,e)
zip5 (ListN x1) (ListN x2) (ListN x3) (ListN x4) (ListN x5) = ListN (loop x1 x2 x3 x4 x5)
  where loop (l1:l1s) (l2:l2s) (l3:l3s) (l4:l4s) (l5:l5s) = (l1,l2,l3,l4,l5) : loop l1s l2s l3s l4s l5s
        loop []       _        _        _        _        = []
        loop _        _        _        _        _        = impossible

zipWith :: (a -> b -> x) -> ListN n a -> ListN n b -> ListN n x
zipWith f (ListN (v1:vs)) (ListN (w1:ws)) = ListN (f v1 w1 : unListN (zipWith f (ListN vs) (ListN ws)))
zipWith _ (ListN [])       _ = ListN []
zipWith _ _                _ = impossible

zipWith3 :: (a -> b -> c -> x)
         -> ListN n a
         -> ListN n b
         -> ListN n c
         -> ListN n x
zipWith3 f (ListN (v1:vs)) (ListN (w1:ws)) (ListN (x1:xs)) =
    ListN (f v1 w1 x1 : unListN (zipWith3 f (ListN vs) (ListN ws) (ListN xs)))
zipWith3 _ (ListN []) _       _ = ListN []
zipWith3 _ _          _       _ = impossible

zipWith4 :: (a -> b -> c -> d -> x)
         -> ListN n a
         -> ListN n b
         -> ListN n c
         -> ListN n d
         -> ListN n x
zipWith4 f (ListN (v1:vs)) (ListN (w1:ws)) (ListN (x1:xs)) (ListN (y1:ys)) =
    ListN (f v1 w1 x1 y1 : unListN (zipWith4 f (ListN vs) (ListN ws) (ListN xs) (ListN ys)))
zipWith4 _ (ListN []) _       _       _ = ListN []
zipWith4 _ _          _       _       _ = impossible

zipWith5 :: (a -> b -> c -> d -> e -> x)
         -> ListN n a
         -> ListN n b
         -> ListN n c
         -> ListN n d
         -> ListN n e
         -> ListN n x
zipWith5 f (ListN (v1:vs)) (ListN (w1:ws)) (ListN (x1:xs)) (ListN (y1:ys)) (ListN (z1:zs)) =
    ListN (f v1 w1 x1 y1 z1 : unListN (zipWith5 f (ListN vs) (ListN ws) (ListN xs) (ListN ys) (ListN zs)))
zipWith5 _ (ListN []) _       _       _       _ = ListN []
zipWith5 _ _          _       _       _       _ = impossible
