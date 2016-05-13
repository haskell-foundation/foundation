-- |
-- Module      : Core.Partial
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- Partial give a way to annotate your partial function with
-- a simple wrapper, which can only evaluated using 'fromPartial'
--
-- > fromPartial ( head [] )
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Core.Partial
    ( Partial
    , partial
    , fromPartial
    , head
    , fromJust
    ) where

import Core.Internal.Base
import Data.Functor.Identity

-- | Partialiality wrapper.
newtype Partial a = Partial (Identity a)
    deriving (Functor, Applicative, Monad)

-- | Create a value that is partial. this can only be
-- unwrap using the 'fromPartial' function
partial :: a -> Partial a
partial = pure

-- | Dewrap a possible partial value
fromPartial :: Partial a -> a
fromPartial (Partial ida) = runIdentity ida

head :: [a] -> Partial a
head l = partial $
    case l of
        []  -> error "head: empty list"
        x:_ -> x

fromJust :: Maybe a -> Partial a
fromJust x = partial $
    case x of
        Nothing -> error "fromJust: Nothing"
        Just y  -> y
