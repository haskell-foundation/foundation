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
    , compilerVersion
    ) where

import qualified System.Info
import qualified GHC.Conc
import Core.String
import Core.Internal.Base
import Core

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
    | Unknown
    | Other String
  deriving (Show, Eq)

instance IsString OS where
    fromString str = case str of
        []        -> Unknown
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

-- | get the architecture info
--
-- get the `arch` from base package but convert
-- it into a strict String
arch :: String
arch = fromList System.Info.arch

-- | get the compiler name
--
-- get the compilerName from base package but convert
-- it into a strict String
compilerName :: String
compilerName = fromList System.Info.compilerName

-- | get the compiler version
--
-- get the compilerVersion from base package but convert
-- it into a strict String
compilerVersion :: String
compilerVersion = fromList $ show System.Info.compilerVersion

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
