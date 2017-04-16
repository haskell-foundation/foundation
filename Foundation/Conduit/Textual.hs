module Foundation.Conduit.Textual
    ( lines
    , fromBytes
    , toBytes
    ) where

import           Foundation.Internal.Base hiding (throw)
import           Foundation.Array.Unboxed
import           Foundation.String (String)
import qualified Foundation.String.UTF8 as S
import           Foundation.Conduit.Internal
import           Foundation.Monad

-- | Split conduit of string to its lines
--
-- This is very similar to Prelude lines except
-- it work directly on Conduit
lines :: Monad m => Conduit String String m ()
lines = await >>= maybe (finish mempty) (go mempty)
  where
    finish buf = if S.null buf then return () else yield buf

    go current nextBuf =
        case S.uncons next' of
            Just (_, rest') -> yield (current `mappend` line) >> go mempty rest'
            Nothing         ->
                let nextCurrent = current `mappend` nextBuf
                 in await >>= maybe (finish nextCurrent) (go nextCurrent)
      where (line, next') = S.breakElem '\n' nextBuf

fromBytes :: MonadThrow m => S.Encoding -> Conduit (UArray Word8) String m ()
fromBytes encoding = loop mempty
  where
    loop r = await >>= maybe (finish r) (go r)
    finish buf | null buf  = return ()
               | otherwise = case S.fromBytes encoding buf of
                                    (s, Nothing, _)  -> yield s
                                    (_, Just err, _) -> throw err
    go current nextBuf =
        case S.fromBytes encoding (current `mappend` nextBuf) of
            (s, Nothing           , r) -> yield s >> loop r
            (s, Just S.MissingByte, r) -> yield s >> loop r
            (_, Just err          , _) -> throw err

toBytes :: Monad m => S.Encoding -> Conduit String (UArray Word8) m ()
toBytes encoding = awaitForever $ \a -> pure (S.toBytes encoding a) >>= yield
