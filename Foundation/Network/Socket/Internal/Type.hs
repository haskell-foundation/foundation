module Foundation.Network.Socket.Internal.Type
    ( Type(..)
    , Datagram, Stream, Raw, SequentialPacket
    ) where

import Foreign.C.Types (CInt)

class Type p where
    typeCode :: p -> CInt

data Datagram

data Stream

data Raw

data SequentialPacket
