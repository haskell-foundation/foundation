{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Core.Data.STupleNat (Nthable(..)) where

import GHC.TypeLits

class KnownNat n => Nthable n a where
    type NthTy n a
    nth :: proxy n -> a -> NthTy n a

instance Nthable 1 (a,b) where
    type NthTy 1 (a,b) = a
    nth _ (a,_) = a
instance Nthable 2 (a,b) where
    type NthTy 2 (a,b) = b
    nth _ (_,b) = b
instance Nthable 1 (a,b,c) where
    type NthTy 1 (a,b,c) = a
    nth _ (a,_,_) = a
instance Nthable 2 (a,b,c) where
    type NthTy 2 (a,b,c) = b
    nth _ (_,b,_) = b
instance Nthable 3 (a,b,c) where
    type NthTy 3 (a,b,c) = c
    nth _ (_,_,c) = c
instance Nthable 1 (a,b,c,d) where
    type NthTy 1 (a,b,c,d) = a
    nth _ (a,_,_,_) = a
instance Nthable 2 (a,b,c,d) where
    type NthTy 2 (a,b,c,d) = b
    nth _ (_,b,_,_) = b
instance Nthable 3 (a,b,c,d) where
    type NthTy 3 (a,b,c,d) = c
    nth _ (_,_,c,_) = c
instance Nthable 4 (a,b,c,d) where
    type NthTy 4 (a,b,c,d) = d
    nth _ (_,_,_,d) = d
