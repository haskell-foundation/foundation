{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Foundation.Internal.Natural
    ( Natural
    ) where

#if MIN_VERSION_base(4,8,0)

import Numeric.Natural

#else

newtype Natural = Natural Integer
    deriving (Show,Eq,Ord)

#endif
