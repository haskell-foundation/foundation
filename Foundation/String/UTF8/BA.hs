{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE CPP                        #-}
module Foundation.String.UTF8.BA
    ( copyFilter
    ) where

import           GHC.Prim
import           GHC.ST
import           Foundation.Internal.Base
import           Foundation.Primitive.Types.OffsetSize

import qualified Foundation.Primitive.UTF8.BA as PrimBA
import qualified Foundation.Primitive.UTF8.BA as PrimBackend

copyFilter :: (Char -> Bool)
           -> CountOf Word8
           -> MutableByteArray# s
           -> PrimBackend.Immutable
           -> Offset Word8
           -> ST s (CountOf Word8)
copyFilter predicate !sz dst src start = loop (Offset 0) start
  where
    !end = start `offsetPlusE` sz
    loop !d !s
        | s == end  = pure (offsetAsSize d)
        | otherwise =
            case PrimBackend.next src s of
                (# c, s' #) | predicate c -> PrimBA.write dst d c >>= \d' -> loop d' s'
                            | otherwise   -> loop d s'
