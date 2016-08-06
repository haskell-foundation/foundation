-- |
-- Module      : Core.System.Info
-- License     : BSD-style
-- Maintainer  : foundation
-- Stability   : experimental
-- Portability : portable
--

{-# LANGUAGE CPP #-}

module Core.System.Info
    (
      -- * Operation System info
      OS(..)
    , os
      -- * CPU info
    , arch
    , cpus
    , Endianness(..)
    , endianness
      -- * Compiler info
    , compilerName
    , System.Info.compilerVersion
    , Data.Version.Version(..)
    ) where

import qualified System.Info
import qualified Data.Version
import qualified GHC.Conc
import Core.String
import Core.Internal.Base

#ifdef ARCH_IS_UNKNOWN_ENDIAN
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (castPtr)
import Foreign.Storable (poke, peek)
import Data.Word (Word8, Word32)
import System.IO.Unsafe (unsafePerformIO)
#endif

data OS
    = Windows
    | OSX
    | Linux
    | Android
    | BSD
    | Other String
  deriving (Show, Eq)

instance IsString OS where
    fromString str = case str of
        "darwin"  -> OSX
        "mingw32" -> Windows
        "linux"   -> Linux
        "linux-android" -> Android
        "openbsd" -> BSD
        "netbsd"  -> BSD
        "freebsd" -> BSD
        _ -> Other $ fromList str

-- | get the OS name
--
-- get the `os` from base package but convert
-- it into a strict String
os :: OS
os = fromString System.Info.os

-- | Enumeration of the known GHC supported architecture.
--
data Arch
    = I386
    | X86_64
    | PowerPC
    | PowerPC64
    | PowerPC64LE
    | Sparc
    | Sparc64
    | ARM
    | AArch64
    | Unknown String
  deriving (Show, Eq, Ord)

-- based on the `ghc` and `base` source as documented in:
--
-- https://github.com/haskell-foundation/foundation/pull/67
--
instance IsString Arch where
    fromString str = case str of
        "i386"          -> I386
        "x86_64"        -> X86_64
        "powerpc"       -> PowerPC
        "powerpc64"     -> PowerPC64
        "powerpc64le"   -> PowerPC64LE
        "sparc"         -> Sparc
        "sparc64"       -> Sparc64
        "arm"           -> ARM
        "aarch64"       -> AArch64
        _               -> Unknown $ fromList str

-- | get the machine architecture on which the program is running
--
-- This function uses base implementation:
--
-- > fromString System.Info.arch
--
arch :: Arch
arch = fromString System.Info.arch

-- | get the compiler name
--
-- get the compilerName from base package but convert
-- it into a strict String
compilerName :: String
compilerName = fromList System.Info.compilerName

-- | returns the number of CPUs the machine has
cpus :: IO Int
cpus = GHC.Conc.getNumProcessors

data Endianness
    = LittleEndian
    | BigEndian
  deriving (Eq, Show)

-- | endianness of the current architecture
endianness :: Endianness
#ifdef ARCH_IS_LITTLE_ENDIAN
endianness = LittleEndian
#elif ARCH_IS_BIG_ENDIAN
endianness = BigEndian
#else
-- ! ARCH_IS_UNKNOWN_ENDIAN
endianness = unsafePerformIO $ bytesToEndianness <$> word32ToByte input
  where
    input :: Word32
    input = 0x01020304
{-# NOINLINE endianness #-}

word32ToByte :: Word32 -> IO Word8
word32ToByte word = alloca $ \wordPtr -> do
         poke wordPtr word
         peek (castPtr wordPtr)

bytesToEndianness :: Word8 -> Endianness
bytesToEndianness 1 = BigEndian
bytesToEndianness _ = LittleEndian
#endif
