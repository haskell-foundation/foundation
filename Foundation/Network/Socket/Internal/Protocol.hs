module Foundation.Network.Socket.Internal.Protocol
    ( Protocol(..)
    , UDP, TCP
    ) where

import Foreign.C.Types (CInt)
import Foundation.Internal.Base (fromInteger)

class Protocol p where
    protocolCode :: p -> CInt

data UDP

data TCP
