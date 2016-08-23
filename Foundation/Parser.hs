-- |
-- Module      : Foundation.Parser
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
-- Stability   : experimental
-- Portability : portable
--
-- The current implementation is mainly, if not copy/pasted, inspired from
-- `memory`'s Parser.
--
-- A very simple bytearray parser related to Parsec and Attoparsec
--
-- Simple example:
--
-- > > parse ((,,) <$> take 2 <*> element 0x20 <*> (elements "abc" *> anyElement)) "xx abctest"
-- > ParseOK "est" ("xx", 116)
--

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Foundation.Parser
    ( Parser
    , Result(..)
    -- * run the Parser
    , parse
    , parseFeed
    -- * Parser methods
    , hasMore
    , element
    , anyElement
    , elements
    , take
    , takeWhile
    , takeAll
    , skip
    , skipWhile
    , skipAll
    ) where

import           Control.Applicative (Alternative, empty, (<|>))
import           Control.Monad       (MonadPlus, mzero, mplus)
import           Foundation.Internal.Base
import           Foundation.Collection hiding (take)
import           Foundation.String
import           Foundation.Number

-- | use for convenience
type Reader a = Sequential a

data ParserError input
    = Expected
        { expectedInput :: !input
            -- ^ the expected input
        , receivedInput :: !input
           -- ^ but received this data
        }
    | NotEnough
        -- ^ not enough data to complete the parser
    | MonadFail String
        -- ^ only use in the event of Monad.fail function
  deriving (Show, Eq, Ord, Typeable)
instance (Show input, Typeable input) => Exception (ParserError input)

-- | Simple parsing result, that represent respectively:
--
-- * failure: with the error message
--
-- * continuation: that need for more input data
--
-- * success: the remaining unparsed data and the parser value
--
data Result input a =
      ParseFail (ParserError input)
    | ParseMore (Maybe input -> Result input a)
    | ParseOK   input a

instance (Show ba, Show a) => Show (Result ba a) where
    show (ParseFail err) = "ParseFailure: " <> show err
    show (ParseMore _)   = "ParseMore _"
    show (ParseOK b a)   = "ParseOK " <> show a <> " " <> show b

-- | The continuation of the current buffer, and the error string
type Failure input r = input -> ParserError input -> Result input r

-- | The continuation of the next buffer value, and the parsed value
type Success input a r = input -> a -> Result input r

-- | Simple parser structure
newtype Parser input a = Parser
    { runParser :: forall r . input
                           -> Failure input r
                           -> Success input a r
                           -> Result input r }

instance Functor (Parser input) where
    fmap f p = Parser $ \buf err ok ->
       runParser p buf err (\b a -> ok b (f a))
instance Applicative (Parser input) where
    pure      = return
    (<*>) d e = d >>= \b -> e >>= \a -> return (b a)
instance Monad (Parser input) where
    fail errorMsg = Parser $ \buf err _ -> err buf (MonadFail $ fromList errorMsg)
    return v      = Parser $ \buf _ ok -> ok buf v
    m >>= k       = Parser $ \buf err ok ->
        runParser m buf err (\buf' a -> runParser (k a) buf' err ok)
instance MonadPlus (Parser input) where
    mzero = fail "MonadPlus.mzero"
    mplus f g = Parser $ \buf err ok ->
        -- rewrite the err callback of @f to call @g
        runParser f buf (\_ _ -> runParser g buf err ok) ok
instance Alternative (Parser input) where
    empty = fail "Alternative.empty"
    (<|>) = mplus

-- | Run a parser on an @initial input.
--
-- If the Parser need more data than available, the @feeder function
-- is automatically called and fed to the More continuation.
parseFeed :: (Reader input, Monad m)
          => m (Maybe input)
          -> Parser input a
          -> input
          -> m (Result input a)
parseFeed feeder p initial = loop $ parse p initial
  where loop (ParseMore k) = feeder >>= (loop . k)
        loop r             = return r

-- | Run a Parser on a ByteString and return a 'Result'
parse :: Reader input
      => Parser input a -> input -> Result input a
parse p s = runParser p s (\_ msg -> ParseFail msg) ParseOK

-- When needing more data, getMore append the next data
-- to the current buffer. if no further data, then
-- the err callback is called.
getMore :: Reader input => Parser input ()
getMore = Parser $ \buf err ok -> ParseMore $ \nextChunk ->
    case nextChunk of
        Nothing -> err buf NotEnough
        Just nc
            | null nc   -> runParser getMore buf err ok
            | otherwise -> ok (mappend buf nc) ()

--
-- Only used by takeAll, which accumulate all the remaining data
-- until ParseMore is fed a Nothing value.
--
-- getAll cannot fail.
getAll :: Reader input => Parser input ()
getAll = Parser $ \buf err ok -> ParseMore $ \nextChunk ->
    case nextChunk of
        Nothing -> ok buf ()
        Just nc -> runParser getAll (mappend buf nc) err ok

-- Only used by skipAll, which flush all the remaining data
-- until ParseMore is fed a Nothing value.
--
-- flushAll cannot fail.
flushAll :: Reader input => Parser input ()
flushAll = Parser $ \buf err ok -> ParseMore $ \nextChunk ->
    case nextChunk of
        Nothing -> ok buf ()
        Just _  -> runParser flushAll mempty err ok

hasMore :: Reader input => Parser input Bool
hasMore = Parser $ \buf err ok ->
    if null buf
        then ParseMore $ \nextChunk ->
            case nextChunk of
                Nothing -> ok buf False
                Just nc -> runParser hasMore nc err ok
        else ok buf True

-- | Get the next `Element input` from the parser
anyElement :: Reader input => Parser input (Element input)
anyElement = Parser $ \buf err ok ->
    case uncons buf of
        Nothing      -> runParser (getMore >> anyElement) buf err ok
        Just (c1,b2) -> ok b2 c1

-- | Parse a specific `Element input` at current position
--
-- if the `Element input` is different than the expected one,
-- this parser will raise a failure.
element :: (Reader input, Eq (Element input))
        => Element input -> Parser input ()
element w = Parser $ \buf err ok ->
    case uncons buf of
        Nothing      -> runParser (getMore >> element w) buf err ok
        Just (c1,b2) | c1 == w   -> ok b2 ()
                     | otherwise -> err buf (Expected (singleton w) (singleton c1))

-- | Parse a sequence of elements from current position
--
-- if the following `Element input` don't match the expected
-- `input` completely, the parser will raise a failure
elements :: (Show input, Eq input, Reader input) => input -> Parser input ()
elements allExpected = consumeEq allExpected
  where
    -- partially consume as much as possible or raise an error.
    consumeEq expected = Parser $ \actual err ok ->
        let eLen = length expected in
         if length actual >= eLen
             then    -- enough data for doing a full match
                let (aMatch,aRem) = splitAt eLen actual
                 in if aMatch == expected
                     then ok aRem ()
                     else err actual (Expected expected aMatch)
             else    -- not enough data, match as much as we have, and then recurse.
                let (eMatch, eRem) = splitAt (length actual) expected
                 in if actual == eMatch
                     then runParser (getMore >> consumeEq eRem) mempty err ok
                     else err actual (Expected expected eMatch)

-- | Take @n elements from the current position in the stream
take :: Reader input => Int -> Parser input input
take n = Parser $ \buf err ok ->
    if length buf >= n
        then let (b1,b2) = splitAt n buf in ok b2 b1
        else runParser (getMore >> take n) buf err ok

-- | Take elements while the @predicate hold from the current position in the
-- stream
takeWhile :: Reader input => (Element input -> Bool) -> Parser input input
takeWhile predicate = Parser $ \buf err ok ->
    let (b1, b2) = span predicate buf
     in if null b2
            then runParser (getMore >> takeWhile predicate) buf err ok
            else ok b2 b1

-- | Take the remaining elements from the current position in the stream
takeAll :: Reader input => Parser input input
takeAll = Parser $ \buf err ok ->
    runParser (getAll >> returnBuffer) buf err ok
  where
    returnBuffer = Parser $ \buf _ ok -> ok mempty buf

-- | Skip @n elements from the current position in the stream
skip :: Reader input => Int -> Parser input ()
skip n = Parser $ \buf err ok ->
    if length buf >= n
        then ok (drop n buf) ()
        else runParser (getMore >> skip (n - length buf)) mempty err ok

-- | Skip `Element input` while the @predicate hold from the current position
-- in the stream
skipWhile :: Reader input => (Element input -> Bool) -> Parser input ()
skipWhile p = Parser $ \buf err ok ->
    let (_, b2) = span p buf
     in if null b2
            then runParser (getMore >> skipWhile p) mempty err ok
            else ok b2 ()

-- | Skip all the remaining `Element input` from the current position in the
-- stream
skipAll :: Reader input => Parser input ()
skipAll = Parser $ \buf err ok -> runParser flushAll buf err ok
