-- |
-- Module      : Core.System.Info
-- License     : BSD-style
-- Maintainer  : foundation
-- Stability   : experimental
-- Portability : portable
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

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

#ifdef ARCH_IS_UNKNOWN_ENDIAN
import qualified Foreign.C.Types as C
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall safe "is_little_endian" c_is_little_endian :: IO C.CInt

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
endianness = if littleEndian then LittleEndian else BigEndian
littleEndian :: Bool
littleEndian = unsafePerformIO c_is_little_endian == 1
#endif
