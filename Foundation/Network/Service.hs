-- |
-- Module      : Foundation.Network.Service
-- License     : BSD-style
-- Maintainer  : Nicolas Di Prima <nicolas@primetype.co.uk>
-- Stability   : experimental
-- Portability : portable
--
-- Retrieve Network Services
--
module Foundation.Network.Service
    ( Service(..)
    , getServiceByName
    , getServiceByPort
    , Command(..)
    , foldServices
    ) where

import Foundation.Class.Storable
import Foundation.Internal.Base
import Foundation.String
import Foundation.Array
import Foundation.Collection
import Foundation.Primitive

import Foundation.System.Bindings.Posix2008

import Control.Monad ((=<<))
import Foreign.Ptr (nullPtr)
import Foreign.C.String (CString, withCString)

-- | Network Service available on the machine
--
-- Ordered by `servicePort` and `serviceProtocol`
--
data Service = Service
    { serviceName     :: !String
    , serviceAliases  :: !(Array String)
    , servicePort     :: !Word16
    , serviceProtocol :: !String
    } deriving (Eq, Typeable, Show)
instance Ord Service where
    compare s1 s2 = case servicePort s1 `compare` servicePort s2 of
        EQ -> serviceProtocol s1 `compare` serviceProtocol s2
        r  -> r

-- | get service by name
--
-- ```
--  print =<< getServiceByName "smtp" (Just "udp")
--  print =<< getServiceByName "http" Nothing
-- ```
--
getServiceByName :: String
                      -- ^ service name to lookup
                 -> Maybe String
                      -- ^ protocol
                      --
                      -- if `Nothing`, any protocol will be match
                      --
                 -> IO (Maybe Service)
getServiceByName name mProto =
    withCString (toList name) $ \cname ->
    withMCString mProto $ \cproto ->
      withServentDBConnected $ do
        cserv <- sysPosixGetServByName cname cproto
        peekService cserv

-- | get service by port
--
-- ```
--  print =<< getServiceByPort 25 (Just "udp")
--  print =<< getServiceByPort 80 Nothing
-- ```
--
getServiceByPort :: Word16
                      -- ^ port number to lookup
                 -> Maybe String
                      -- ^ protocol
                      --
                      -- if `Nothing`, any protocol will be match
                      --
                 -> IO (Maybe Service)
getServiceByPort port mProto =
    withMCString mProto $ \cproto ->
      withServentDBConnected $ do
        cserv <- sysPosixGetServByPort (toBE port) cproto
        peekService cserv

-- | command used for the @foldServices@ function
data Command a = More a | Stop a

-- | fold along the services available
--
-- For example, one could list all the services available with the following:
--
-- ```
-- accumulate :: Service -> [Service] -> Command [Service]
-- accumulate x xs = More (x:xs)
--
-- xs =<< foldServices accumulate []
-- forM_ print xs
-- ```
--
foldServices :: (Service -> a -> Command a) -> a -> IO a
foldServices f acc' = withServentDBConnected $ loop acc'
  where
    loop acc = do
      cserv <- sysPosixGetServEnt
      mserv <- peekService cserv
      case mserv of
        Nothing -> return acc
        Just s  -> case f s acc of
            Stop e -> return e
            More e -> loop e

withMCString :: Maybe String -> (CString -> IO a) -> IO a
withMCString Nothing f = f nullPtr
withMCString (Just l) f = withCString (toList l) f

withServentDBConnected :: IO a -> IO a
withServentDBConnected f = sysPosixSetServEnt 1 *> f <* sysPosixEndServEnt

peekService :: Ptr CServEnt -> IO (Maybe Service)
peekService ptr
    | ptr == nullPtr = return Nothing
    | otherwise = do
        offname <- peek =<< peek (cservent_name_ptr ptr)
        aliases' <- peekArrayEndedBy nullPtr =<< peek (castPtr $ cservent_aliases_ptr ptr)
        aliases <- forM aliases' peek
        port    <- fromBE <$> peek (cservent_port_ptr ptr)
        proto   <- peek =<< peek (cservent_proto_ptr ptr)
        return $ Just $ Service offname aliases port proto
