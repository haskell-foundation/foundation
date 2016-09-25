-- |
-- Module      : Foundation.System.Entropy.Unix
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Foundation.System.Entropy.Unix
    ( EntropyCtx
    , entropyOpen
    , entropyGather
    , entropyClose
    ) where

import Foreign.Ptr
import Foreign.C.Types
import Control.Exception as E
import Control.Monad
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Foundation.Internal.Base
import Prelude (fromIntegral)
import Foundation.System.Entropy.Common

data EntropyCtx =
      EntropyCtx Handle
    | EntropySyscall

entropyOpen :: IO EntropyCtx
entropyOpen = do
    if supportSyscall
        then return EntropySyscall
        else do
            mh <- openDev "/dev/urandom"
            case mh of
                Nothing -> E.throwIO EntropySystemMissing
                Just h  -> return $ EntropyCtx h

-- | try to fill the ptr with the amount of data required.
-- Return the number of bytes, or a negative number otherwise
entropyGather :: EntropyCtx -> Ptr Word8 -> Int -> IO Int
entropyGather (EntropyCtx h) ptr n = gatherDevEntropy h ptr n
entropyGather EntropySyscall ptr n = c_sysrandom_linux ptr (fromIntegral n)

entropyClose :: EntropyCtx -> IO ()
entropyClose (EntropyCtx h)  = hClose h
entropyClose EntropySyscall  = return ()

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

supportSyscall :: Bool
supportSyscall = unsafePerformIO $ do
    r <- c_sysrandom_linux nullPtr 0
    return $! r == 0
{-# NOINLINE supportSyscall #-}

foreign import ccall unsafe "foundation_sysrandom_linux"
   c_sysrandom_linux :: Ptr Word8 -> CSize -> IO Int
