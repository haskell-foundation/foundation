{-# LANGUAGE CPP #-}

module Foundation.Network.Socket.Internal
    ( module X
    ) where

import Foundation.Network.Socket.Internal.Protocol as X
import Foundation.Network.Socket.Internal.Type as X
import Foundation.Network.Socket.Internal.Family as X

#ifdef mingw32_HOST_OS
# error Windows is not yet supported
#else
import Foundation.Network.Socket.Internal.Unix as X
#endif
