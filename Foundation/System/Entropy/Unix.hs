-- |
-- Module      : Foundation.System.Entropy.Unix
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
{-# LANGUAGE ScopedTypeVariables #-}
module Foundation.System.Entropy.Unix
    ( EntropyCtx
    , entropyOpen
    , entropyGather
    , entropyClose
    ) where

import Foreign.Ptr
import Control.Exception as E
import Control.Monad
import System.IO
import Foundation.Internal.Base
import Prelude (fromIntegral)

newtype EntropyCtx = EntropyCtx Handle

entropyOpen :: IO (Maybe EntropyCtx)
entropyOpen = fmap EntropyCtx `fmap` openDev "/dev/urandom"

entropyGather :: EntropyCtx -> Ptr Word8 -> Int -> IO Int
entropyGather (EntropyCtx h) ptr n = gatherDevEntropy h ptr n

entropyClose :: EntropyCtx -> IO ()
entropyClose (EntropyCtx h)  = hClose h

openDev :: [Char] -> IO (Maybe Handle)
openDev filepath = (Just `fmap` openAndNoBuffering) `E.catch` \(_ :: IOException) -> return Nothing
  where openAndNoBuffering = do
            h <- openBinaryFile filepath ReadMode
            hSetBuffering h NoBuffering
            return h

gatherDevEntropy :: Handle -> Ptr Word8 -> Int -> IO Int
gatherDevEntropy h ptr sz =
     (fromIntegral `fmap` hGetBufSome h ptr (fromIntegral sz))
    `E.catch` \(_ :: E.IOException) -> return 0
