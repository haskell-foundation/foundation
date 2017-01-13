module Foundation.Conduit
    ( Conduit
    , ZipSink (..)
    , await
    , yield
    , yieldOr
    , leftover
    , runConduit
    , runConduitPure
    , fuse
    , (.|)
    , sourceHandle
    , sinkHandle
    , sinkList
    ) where

import Foundation.Conduit.Internal
import Foundation.Collection
import Foundation.IO
import Foundation.Internal.Base
import Foundation.Monad.Base
import Foundation.Array
import Foundation
import System.IO (Handle)


infixr 2 .|
-- | Operator version of 'fuse'.
(.|) :: Monad m => Conduit a b m () -> Conduit b c m r -> Conduit a c m r
(.|) = fuse
{-# INLINE (.|) #-}

sourceHandle :: MonadIO m
             => Handle
             -> Conduit i (UArray Word8) m ()
sourceHandle h =
    loop
  where
    defaultChunkSize :: Int
    defaultChunkSize = (32 :: Int) * 1000 - 16
    loop = do
        arr <- liftIO (hGet h defaultChunkSize)
        if null arr
            then return ()
            else yield arr >> loop

sinkHandle :: MonadIO m
           => Handle
           -> Conduit (UArray Word8) o m ()
sinkHandle h =
    loop
  where
    loop = await >>= maybe
        (return ())
        (\arr -> liftIO (hPut h arr) >> loop)

sinkList :: Monad m => Conduit i o m [i]
sinkList =
    loop id
  where
    loop front = await >>= maybe
        (return (front []))
        (\x -> loop (front . (x:)))
