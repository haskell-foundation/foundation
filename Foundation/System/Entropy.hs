-- |
-- Module      : Foundation.System.Entropy
-- License     : BSD-style
-- Maintainer  : Foundation
-- Stability   : stable
-- Portability : good
--
{-# LANGUAGE CPP #-}
module Foundation.System.Entropy
    ( getEntropy
    ) where


import           Foundation.Internal.Base
import           Foundation.Internal.Types
import qualified Foundation.Array.Unboxed.Mutable as A
import qualified Foundation.Array.Unboxed as A
import           Control.Exception
import           Foreign.Ptr
import           Foundation.Number

import           Foundation.System.Entropy.Common
#ifdef WINDOWS
import           Foundation.System.Entropy.Windows
#else
import           Foundation.System.Entropy.Unix
#endif

-- | Get some of the system entropy
getEntropy :: Int -> IO (A.UArray Word8)
getEntropy n = do
    m <- A.newPinned (Size n)
    bracket entropyOpen entropyClose $ \ctx -> A.withMutablePtr m $ loop ctx n
    A.unsafeFreeze m
  where
    loop :: EntropyCtx -> Int -> Ptr Word8 -> IO ()
    loop _   0 _ = return ()
    loop ctx i p = do
        let chSz = min entropyMaximumSize i
        r <- entropyGather ctx p chSz
        if r
            then loop ctx (n-chSz) (p `plusPtr` chSz)
            else throwIO EntropySystemMissing
