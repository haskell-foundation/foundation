module Foundation.Network.Socket.Close
    ( close
    , CloseError(..)
    ) where

import Data.Function
import Control.Concurrent.MVar

import Foreign.C.Error hiding (throwErrno)

import GHC.Conc (closeFdWith)
import Foundation.Internal.Base

import qualified Foundation.Network.Socket.Internal as I
import Foundation.Network.Socket.Internal (Socket(..))

data CloseError = CloseError

close :: Socket f -> IO ()
close (Socket mvar) = modifyMVarMasked_ mvar close_
  where
    close_ :: I.Fd -> IO I.Fd
    close_ fd
        | fd == I.Fd (-1)  = return fd
        | otherwise = do
            closeFdWith
              (const $ fix $ \retry -> do
                e <- I.close fd
                case e of
                    Left err | err /= eINTR -> I.throwErrno err
                             | otherwise    -> retry
                    Right ()                -> return ()
              ) fd
            return (I.Fd (-1))
