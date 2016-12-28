module Foundation.Network.Socket.Internal.Type
    ( Type(..)
    , Datagram, Stream, Raw, SequentialPacket
    ) where

#include "netinet/in.h"

import Foreign.C.Types (CInt)
import Foundation.Internal.Base (fromInteger)

class Type p where
    typeCode :: p -> CInt

data Datagram
instance Type Datagram where
    typeCode _ = (#const SOCK_DGRAM)

data Stream
instance Type Stream where
    typeCode _ = (#const SOCK_STREAM)

data Raw
instance Type Raw where
    typeCode _ = (#const SOCK_RAW)

data SequentialPacket
instance Type SequentialPacket where
    typeCode _ = (#const SOCK_SEQPACKET)
