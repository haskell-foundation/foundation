module Foundation.Network.Socket.Internal.Protocol
    ( Protocol(..)
    , UDP, TCP
    ) where

#include "netinet/in.h"

import Foreign.C.Types (CInt)
import Foundation.Internal.Base (fromInteger)

class Protocol p where
    protocolCode :: p -> CInt

data UDP
instance Protocol UDP where
    protocolCode _ = (#const IPPROTO_UDP)

data TCP
instance Protocol TCP where
    protocolCode _ = (#const IPPROTO_TCP)
