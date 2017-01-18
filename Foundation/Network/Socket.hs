{-# LANGUAGE FlexibleContexts #-}

module Foundation.Network.Socket
    ( Socket

      -- * Family
    , Family(..)
    , SocketAddress (SocketAddressInet, inetAddress, inetPort)
    , Inet, InetPort(..)
      -- * Types
    , Type(..)
    , Datagram, Stream, Raw, SequentialPacket
      -- * Protocols
    , Protocol(..)
    , TCP, UDP

      -- * Flags
    , Flag
    , flagWaitAll, flagOutOfBand, flagNoSignal, flagEndOfRecord

      -- * operations
      -- ** socket
    , socket, SocketError(..)
      -- ** close
    , close
      -- ** connect
    , connect, ConnectError(..)
      -- ** bind
    , bind, BindError(..)
      -- ** listen
    , listen, ListenError(..)
      -- ** accept
    , accept, AcceptError(..)
      -- ** send
    , send, SendError(..)
      -- ** recv
    , recv, RecvError(..)
    ) where

import Foundation.Network.Socket.Internal
            ( Protocol(..), UDP, TCP
            , Type(..), Raw, SequentialPacket, Datagram, Stream
            , Family(..), Inet, InetPort(..)
            , SocketAddress(..)
            , Flag
            , flagWaitAll, flagOutOfBand, flagNoSignal, flagEndOfRecord
            , Socket(..)
            )

import Foundation.Network.Socket.Socket
import Foundation.Network.Socket.Close
import Foundation.Network.Socket.Connect
import Foundation.Network.Socket.Bind
import Foundation.Network.Socket.Listen
import Foundation.Network.Socket.Accept
import Foundation.Network.Socket.Send
import Foundation.Network.Socket.Recv
