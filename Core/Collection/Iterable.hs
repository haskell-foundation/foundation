module Core.Collection.Iterable
    ( Iterable(..)
    ) where

import qualified Core.Array.Unboxed as UV
import           Core.Collection.Element
import           Core.Collection.Sequential
import           Core.Internal.Base
import           Core.Number

class Sequential col => Iterable col where
  data State col

  initialState :: col -> State col

  hasNext :: State col -> Bool

  next :: State col -> Maybe (State col, Element col)

instance Iterable [a] where
  data State [a] = SL [a]

  initialState = SL

  hasNext (SL []) = False
  hasNext _       = True

  next (SL [])     = Nothing
  next (SL (s:ss)) = Just (SL ss, s)

instance UV.PrimType ty => Iterable (UV.UArray ty) where
  data State (UV.UArray ty) = SUV (UV.UArray ty) Int

  initialState vec = SUV vec 0

  hasNext (SUV vec i) = i >= 0 && i < length vec

  next (SUV vec i)
      | i >= 0 && i < length vec = Just (SUV vec (i + 1), vec `UV.unsafeIndex` i)
      | otherwise                = Nothing
