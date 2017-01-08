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
    , socket
    , close
    , connect
    , bind
    , listen
    , accept
    , send
    , recv
    ) where

import Prelude (fromIntegral)

import Data.Function
import Control.Concurrent.MVar
import Control.Monad (when)
import Foreign.C.Error hiding (throwErrno)
import Foreign.C.Types
import Foreign.Marshal.Alloc (allocaBytes, alloca)
import GHC.Conc (closeFdWith, threadWaitRead, threadWaitWrite)

import Foundation.Collection
import Foundation.Array.Unboxed (withPtr, UArray)
import Foundation.Array.Unboxed.Mutable (newPinned)
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
            , Family(..), Inet, InetPort(..)
            , SocketAddress(..)
            , Flag
            , flagWaitAll, flagOutOfBand, flagNoSignal, flagEndOfRecord
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
connect s addr =
    let (Size sz) = size (Just addr) :: Size Word8 in
    allocaBytes sz $ \addrptr -> do
        poke addrptr addr
        retryWith s [eAGAIN, eWOULDBLOCK, eINTR] threadWaitRead $ \fd -> do
            when (fd < I.Fd 0) (I.throwErrno eBADF)
            e <- I.connect fd (castPtr addrptr) (fromIntegral sz)
            return $ case e of
                Left e | e == eISCONN -> Right ()
                Left e                -> Left e
                Right a               -> Right a

bind :: (Family f, StorableFixed (SocketAddress f))
     => Socket f t p
     -> SocketAddress f
     -> IO ()
bind s addr =
    let (Size sz) = size (Just addr) in
    allocaBytes sz $ \addrptr -> do
        poke addrptr addr
        retryWith s [] (\_ -> return ()) $ \fd -> do
            when (fd < I.Fd 0) (I.throwErrno eBADF)
            I.bind fd (castPtr addrptr) (fromIntegral sz)

listen :: (Family f, StorableFixed (SocketAddress f))
     => Socket f t p
     -> Int
     -> IO ()
listen s sz =
    retryWith s [] (\_ -> return ()) $ \fd -> do
        when (fd < I.Fd 0) (I.throwErrno eBADF)
        I.listen fd (fromIntegral sz)

accept :: (Family f, StorableFixed (SocketAddress f))
       => Socket f t p
       -> IO (Socket f t p, SocketAddress f)
accept s =
    allocaAddr s Proxy $ \addrPtr (CInt sz) ->
      alloca $ \addrLenPtr -> do
        poke addrLenPtr sz
        fd' <- retryWith s [eWOULDBLOCK, eAGAIN] threadWaitRead $ \fd -> do
                when (fd < I.Fd 0) (I.throwErrno eBADF)
                I.accept fd (castPtr addrPtr) (castPtr addrLenPtr)
        addr <- peek addrPtr
        mvar <- newMVar fd'
        _ <- mkWeakMVar mvar (close (Socket mvar))
        return (Socket mvar, addr)
  where
    allocaAddr :: (Family f, StorableFixed (SocketAddress f))
               => Socket f t p
               -> Proxy (SocketAddress f)
               -> (Ptr (SocketAddress f) -> CInt -> IO a)
               -> IO a
    allocaAddr _ p f =
        let (Size sz) = size p
         in allocaBytes sz $ \ptr -> f (castPtr ptr) (fromIntegral sz)


send :: PrimType ty
     => Socket f t p
     -> Flag
     -> UArray ty
     -> IO (Size ty)
send s flag array = do
    (CInt i) <- withPtr array $ \ptr ->
        retryWith s [eAGAIN, eWOULDBLOCK] threadWaitWrite $ \fd -> do
            when (fd < I.Fd 0) (I.throwErrno eBADF)
            I.send fd (castPtr ptr) (fromInteger $ toInteger $ num `scale` sz) flag
    return $ Size $ fromInteger $ toInteger i
  where
    num :: Size ty
    num = Size $ length array
    sz = primSizeInBytes (toProxy array)
    toProxy :: PrimType ty => UArray ty -> Proxy ty
    toProxy _ = Proxy

recv :: (Show ty, PrimType ty)
     => Socket f t p
     -> Flag
     -> Size ty
     -> IO (UArray ty)
recv s flag num = do
    -- TODO use mutable ByteArray
    --      fix the bug in withMutableByteArrayPtr  which copies a temporary
    --      ByteArray#
    array <- newPinned num >>= unsafeFreeze
    CInt sz <- withPtr array $ \ptr ->
                 retryWith s [eAGAIN, eWOULDBLOCK] threadWaitRead $ \fd -> do
                    when (fd < I.Fd 0) (I.throwErrno eBADF)
                    let numBytes = fromInteger $ toInteger $ num `scale` primSizeInBytes (toProxy array)
                    I.recv fd (castPtr ptr) numBytes flag
    let sz' = fromInteger $ toInteger sz
    return $ take sz' array
  where
    toProxy :: PrimType ty => UArray ty -> Proxy ty
    toProxy _ = Proxy

retryWith :: Socket f t p -> [Errno] -> (I.Fd -> IO ()) -> (I.Fd -> IO (Either Errno a)) -> IO a
retryWith s@(Socket mvar) cases waitFunction action = do
    e <- withMVar mvar $ \fd -> do
            e <- action fd
            case e of
                Left err | err `elem` cases ->
                               return $ Left (waitFunction fd)
                         | otherwise -> I.throwErrno err
                Right a -> return $ Right a
    case e of
        Left wait -> wait >> retryWith s cases waitFunction action
        Right a   -> return a
