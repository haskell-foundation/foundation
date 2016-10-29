{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Foundation.Internal.Natural
    ( Natural
    ) where

#if MIN_VERSION_base(4,8,0)

import Numeric.Natural

#else

import Prelude (Show,Eq,Ord,Enum,Num(..),Integer,error,(<), (>), otherwise)
import Data.Typeable

newtype Natural = Natural Integer
    deriving (Show,Eq,Ord,Enum,Typeable)

-- re-create the buggy Num instance for Natural
instance Num Natural where
    fromInteger n
        | n < 0     = error "natural should be positive: "
        | otherwise = Natural n
    (+) (Natural a) (Natural b) = Natural (a + b)
    (-) (Natural a) (Natural b)
        | r < 0     = error "natural should be positve"
        | otherwise = Natural (a - b)
      where r = (a - b)
    (*) (Natural a) (Natural b) = Natural (a * b)
    abs n = n
    negate n = n
    signum (Natural n)
        | n > 0     = 1
        | otherwise = 0

#endif
