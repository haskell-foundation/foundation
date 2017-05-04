-- |
-- Module      : Foundation.JSON.Types
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
--
-- Declaration of JSON types
--
module Foundation.JSON.Types
    ( JsonEvent(..)
    , JKey(..)
    , JInt(..)
    , JFloat(..)
    , JString(..)
    ) where

import Foundation.Primitive.Imports

-- | JSON Key
newtype JKey = JKey String
    deriving (Show,Eq,Ord)

-- | JSON Integral
newtype JInt = JInt String
    deriving (Show,Eq,Ord)

-- | JSON Float
newtype JFloat = JFloat String
    deriving (Show,Eq,Ord)

-- | JSON String
newtype JString = JString String
    deriving (Show,Eq,Ord)

-- | JSON Event element
data JsonEvent =
      JsonArrayBegin
    | JsonObjectBegin
    | JsonArrayEnd
    | JsonObjectEnd
    | JsonInt JInt
    | JsonFloat JFloat
    | JsonString JString
    | JsonKey JKey
    | JsonTrue
    | JsonFalse
    | JsonNull
    deriving (Show,Eq)
