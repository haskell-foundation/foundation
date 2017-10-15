{-# LANGUAGE MultiParamTypeClasses      #-}

module Basement.Alg.Class
    ( Indexable
    , index ) where

import           Basement.Types.OffsetSize

class Indexable container ty where
    index :: container -> (Offset ty) -> ty
