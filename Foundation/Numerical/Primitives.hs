{-# LANGUAGE MagicHash #-}
module Foundation.Numerical.Primitives
    ( intToWord
    ) where

import GHC.Types
import GHC.Prim
--import GHC.Word
--import GHC.Int

intToWord :: Int -> Word
intToWord (I# i) = W# (int2Word# i)
{-# INLINE intToWord #-}
