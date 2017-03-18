-- |
-- Module      : Foundation.Network.Address
-- License     : BSD-style
-- Maintainer  : Nicolas Di Prima <nicolas@primetype.co.uk>
-- Stability   : experimental
-- Portability : portable
--
-- Network address
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Foundation.Network.Address
    ( Address(..)
    ) where

import Foundation.Class.Storable
import Foundation.Internal.Base

import           Foundation.Network.IPv4 (IPv4)
import qualified Foundation.Network.IPv4 as IPv4 (loopback, any)
import           Foundation.Network.IPv6 (IPv6)
import qualified Foundation.Network.IPv6 as IPv6 (loopback, any)

import Foreign.C.Types

#ifdef mingw32_HOST_OS
#include <winsock2.h>
#else
#include "netinet/in.h"
#endif

-- | Network Address class
--
class StorableFixed addr => Address addr where
    -- | retrieve associated Network Family code for this type of address
    familyCode :: proxy addr -> CInt
    -- | the loopback
    --
    -- For IPv4: `127.0.0.1`, for IPv6: `::1`
    loopback   :: addr
    -- | any address
    --
    -- For IPv4: `0.0.0.0`, for IPv6: `::`
    any        :: addr

instance Address IPv4 where
    loopback   = IPv4.loopback
    any        = IPv4.any
    familyCode _ = (#const AF_INET)
instance Address IPv6 where
    loopback = IPv6.loopback
    any      = IPv6.any
    familyCode _ = (#const AF_INET6)
