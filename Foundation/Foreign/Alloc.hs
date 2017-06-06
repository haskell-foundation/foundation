{-# LANGUAGE MagicHash #-}
module Foundation.Foreign.Alloc
    ( allocaBytes
    ) where

import qualified Foreign.Marshal.Alloc as A (allocaBytes)
import           Foundation.Primitive.Imports
import           Foundation.Primitive.Types.OffsetSize

allocaBytes :: Size Word8 -> (Ptr a -> IO b) -> IO b
allocaBytes (Size i) f = A.allocaBytes i f
