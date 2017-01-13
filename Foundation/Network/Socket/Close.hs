module Foundation.Network.Socket.Close
    ( close
    , CloseError(..)
    ) where

import Data.Function
import Control.Concurrent.MVar

import GHC.Conc (closeFdWith)
import Foundation.Internal.Base

import qualified Foundation.Network.Socket.Internal as I (close)
import Foundation.Network.Socket.Internal
    ( throwSocketError
    , eInterrupted
    , Fd(..)
    , Socket(..)
    )

data CloseError = CloseError

close :: Socket f -> IO ()
close (Socket mvar) = modifyMVarMasked_ mvar close_
  where
    close_ :: Fd -> IO Fd
    close_ fd
        | fd == Fd (-1)  = return fd
        | otherwise = do
            closeFdWith
              (const $ fix $ \retry -> do
                e <- I.close fd
                case e of
                    Left err | err /= eInterrupted -> throwSocketError err
                             | otherwise    -> retry
                    Right ()                -> return ()
              ) fd
            return (Fd (-1))
