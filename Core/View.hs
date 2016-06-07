-- |
-- Module      : Core.View
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
module Core.View
    ( View(..)
    , view
    ) where

import           Core.Internal.Base
import qualified Core.Collection as C

data View a = View
    { viewOffset     :: Int
    , viewLength     :: Int
    , viewCollection :: a
    }

{-
data ViewL a = ViewL
    { viewLOffset     :: Int
    , viewLCollection :: a
    }
-}

type instance C.Element (View a) = C.Element a

{-
instance Monoid (View s) where
    mempty  = End
    mappend = append
    mconcat = concat
-}

instance (IsList s, C.InnerFunctor s, C.Element (View s) ~ Item s) => C.InnerFunctor (View s) where
    imap f (View s r) = View (C.imap f s) (C.imap f r)

instance (IsList s, C.InnerFunctor s, C.Sequential s, C.Element (View s) ~ Item s)
         => C.SemiOrderedCollection (View s) where
    snoc = snoc
    cons = cons
    find = find
    --sortBy = sortBy
    length = viewLength
    singleton x = View 0 0 (fromList [x])

instance (IsList s, C.InnerFunctor s, C.Sequential s, C.Element (View s) ~ Item s)
        => C.Sequential (View s) where
    null = (==) 0 . viewLength
    take = take
    drop = drop
    splitAt = splitAt
    splitOn = splitOn
    break = break
    span = span
    reverse = reverse
    filter = filter

view :: C.SemiOrderedCollection a => a -> View a
view a = View 0 (C.length a) a

subView :: C.Sequential c => View c -> c
subView (View ofs len c) = C.take len $ C.drop ofs len

cons :: C.Sequential c => C.Element c -> View c -> View c
cons e v = View 0 (succ $ viewLength v) (C.cons e $ subView v)

snoc :: C.Sequential c => View c -> C.Element c -> View c
snoc v e = View 0 (succ $ viewLength v) (C.snoc (subView v) e)

find :: (C.Element c -> View c) -> View c -> Maybe (C.Element c)
find predicate v = undefined

take = undefined
drop = undefined
splitAt = undefined
splitOn = undefined
break = undefined
span = undefined

reverse :: C.Sequential c => View c -> View c
reverse v = View 0 (viewLength v) $ C.reverse (C.drop (viewOffset v) $ subView v)

filter = undefined
