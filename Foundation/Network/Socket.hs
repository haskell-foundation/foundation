{-# LANGUAGE FlexibleContexts #-}

module Foundation.Network.Socket
    ( Socket

      -- * Family
    , Family(..)
    , SocketAddress (SocketAddressInet, inetAddress, inetPort)
    , Inet, InetPort(..), InetAddress(..)
    , inetAny, inetAddressFromTuple
      -- * Types
    , Type(..)
    , Datagram, Stream, Raw, SequentialPacket
      -- * Protocols
    , Protocol(..)
    , TCP, UDP

      -- * operations
    , socket
    , close
    , connect
    , bind
    , listen
    , accept
    , send
    ) where

import Prelude (fromIntegral)

import Data.Function
import Control.Concurrent.MVar
import Foreign.C.Error hiding (throwErrno)
import Foreign.C.Types
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.Conc (closeFdWith)

import Foundation.Collection
import Foundation.Array.Unboxed (withPtr, UArray)
import Foundation.Internal.Base
import Foundation.Internal.Types
import Foundation.Internal.Proxy
import Foundation.Primitive
import Foundation.Numerical
import Foundation.Class.Storable
import qualified Foundation.Network.Socket.Internal as I
import Foundation.Network.Socket.Internal
            ( Protocol(..), UDP, TCP
            , Type(..), Raw, SequentialPacket, Datagram, Stream
            , Family(..), Inet, InetPort(..), InetAddress(..), inetAny, inetAddressFromTuple
            , SocketAddress(..)
            )

newtype Socket f t p = Socket (MVar I.Fd)

socket :: (Family f, Type t, Protocol p)
       => IO (Socket f t p)
socket = socket_ undefined undefined undefined
  where
    socket_ :: (Family f, Type t, Protocol p)
            => f -> t -> p -> IO (Socket f t p)
    socket_ f t p = do
        efd <- I.socket (familyCode f) (typeCode t) (protocolCode p)
        case efd of
            Left errno -> I.throwErrno errno
            Right fd   -> do
                mvar <- newMVar fd
                _ <- mkWeakMVar mvar (close (Socket mvar))
                return $ Socket mvar

close :: Socket f t p -> IO ()
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

connect :: (Family f, StorableFixed (SocketAddress f))
        => Socket f t p
        -> SocketAddress f
        -> IO ()
connect (Socket mvar) addr =
    let (Size sz) = size (Just addr) in
    withMVar mvar $ \fd ->
        allocaBytes sz $ \addrptr -> do
            poke addrptr addr
            e <- I.connect fd (castPtr addrptr) (fromIntegral sz)
            case e of
                Left err | err == eWOULDBLOCK || err == eINPROGRESS -> do
                    -- TODO: wait for connection to happen
                    e' <- I.connect fd (castPtr addrptr) (fromIntegral sz)
                    case e' of
                        Right () -> return ()
                        Left err' | err' == eISCONN -> return ()
                                  | otherwise      -> I.throwErrno err'
                Left err -> I.throwErrno err
                Right () -> return ()

bind :: (Family f, StorableFixed (SocketAddress f))
     => Socket f t p
     -> SocketAddress f
     -> IO ()
bind (Socket mvar) addr =
    let (Size sz) = size (Just addr) in
    withMVar mvar $ \fd ->
        allocaBytes sz $ \addrptr -> do
            poke addrptr addr
            e <- I.bind fd (castPtr addrptr) (fromIntegral sz)
            case e of
                Left err -> I.throwErrno err
                Right () -> return ()

listen :: (Family f, StorableFixed (SocketAddress f))
     => Socket f t p
     -> Int
     -> IO ()
listen (Socket mvar) sz =
    withMVar mvar $ \fd -> do
        e <- I.listen fd (fromIntegral sz)
        case e of
            Left err -> I.throwErrno err
            Right () -> return ()

accept :: (Family f, StorableFixed (SocketAddress f))
       => Socket f t p
       -> IO (Socket f t p, SocketAddress f)
accept = undefined

send :: PrimType ty
     => Socket f t p
     -> UArray ty
     -> IO (Size ty)
send (Socket mvar) array = withMVar mvar $ \fd ->
    withPtr array $ \ptr -> do
        e <- I.send fd (castPtr ptr) (fromInteger $ toInteger $ num `scale` sz) 0 -- TODO add msg
        case e of
            Left err -> I.throwErrno err
            Right (CInt r) -> return $ Size $ fromInteger $ toInteger r
  where
    num :: Size ty
    num = Size $ length array
    sz = primSizeInBytes (toProxy array)
    toProxy :: PrimType ty => UArray ty -> Proxy ty
    toProxy _ = Proxy
