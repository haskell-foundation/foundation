{-# LANGUAGE CPP             #-}
{-# LANGUAGE ConstraintKinds #-}
-- a compat module for ghc < 7.10 to handle the AMP change smoothly
module Basement.Compat.AMP
    ( AMPMonad
    ) where

import Basement.Compat.Base

#if MIN_VERSION_base(4,8,0)
type AMPMonad m = Monad m
#else
type AMPMonad m = (Functor m, Applicative m, Monad m)
#endif
