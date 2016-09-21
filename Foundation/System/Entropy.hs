-- |
-- Module      : Foundation.System.Entropy
-- License     : BSD-style
-- Maintainer  : Foundation
-- Stability   : stable
-- Portability : good
--
{-# LANGUAGE CPP #-}
module Foundation.System.Entropy
    ( EntropyCtx
    , entropyOpen
    , entropyGather
    , entropyClose
    ) where

#ifdef WINDOWS
import Foundation.System.Entropy.Windows
#else
import Foundation.System.Entropy.Unix
#endif
