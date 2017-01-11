{-# LANGUAGE CPP #-}

module Foundation.Network.Socket.Internal
    ( module X
    , Socket(..)
    , retryWith
    ) where

import Foundation.Network.Socket.Internal.Protocol as X
import Foundation.Network.Socket.Internal.Type as X
import Foundation.Network.Socket.Internal.Family as X

#ifdef mingw32_HOST_OS
import Foundation.Network.Socket.Internal.Windows as X
#else
import Foundation.Network.Socket.Internal.Unix as X
#endif

import Control.Concurrent.MVar
import Foreign.C.Error hiding (throwErrno)

import Foundation.Internal.Base
import Foundation.Collection

newtype Socket s = Socket (MVar Fd)

data Return a = Error Errno | Retry (IO ()) | Ok a

retryWith :: Socket s
          -> (Errno -> IO a)
          -> (X.Fd -> IO ())
          -> (X.Fd -> IO (Either Errno a))
          -> IO a
retryWith s@(Socket mvar) handleErr waitFunction action = do
    e <- withMVar mvar $ \fd -> do
            e <- action fd
            return $ case e of
                Left err | err `elem` [eAGAIN, eWOULDBLOCK, eINTR] ->
                               Retry (waitFunction fd)
                         | otherwise -> Error err
                Right a -> Ok a
    case e of
        Retry wait -> wait >> retryWith s handleErr waitFunction action
        Error err  -> handleErr err
        Ok a       -> return a
