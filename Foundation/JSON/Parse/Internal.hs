-- |
-- Module      : Foundation.JSON.Parse.Internal
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
--
-- JSON Parser internal
-- {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE Rank2Types                 #-}
module Foundation.JSON.Parse.Internal
    ( Parser(..)
    , Result(..)
    , pGo
    ) where

import           GHC.Types
import           GHC.Word
import           Foundation.Numerical
import           Foundation.Bits
import           Foundation.Primitive.Imports
import           Foundation.Primitive.IntegralConv
import           Foundation.JSON.Parse.Types
import           Foundation.JSON.Types
import           Data.List (reverse)
import           Foundation.String.UTF8 (String)
import qualified Foundation.String.UTF8 as S
import           Foundation.Array.Unboxed (UArray)
import qualified Foundation.Array.Unboxed as A

-- from 8.0 we'll be able to use PatternSynonym, sadly for now stuck with GHC
#define CH_BS     0x08
#define CH_TAB    0x09
#define CH_FF     0x0c
#define CH_CR     0x0d
#define CH_LF     0x0a
#define CH_SP     0x20
#define CH_LBRACE 0x7b
#define CH_RBRACE 0x7d
#define CH_LSQR   0x5b
#define CH_RSQR   0x5d
#define CH_SLASH  0x2f
#define CH_BACKS  0x5c
#define CH_HASH   0x23
#define CH_STAR   0x2a
#define CH_COMMA  0x2c
#define CH_QUOTE  0x22
#define CH_COLON  0x3a
#define CH_DOT    0x2e
#define CH_PLUS   0x2b
#define CH_MINUS  0x2d
#define CH_0      0x30
#define CH_9      0x39
#define CH_E      0x45
#define CH_a      0x61
#define CH_b      0x62
#define CH_e      0x65
#define CH_f      0x66
#define CH_l      0x6c
#define CH_n      0x6e
#define CH_r      0x72
#define CH_s      0x73
#define CH_t      0x74
#define CH_u      0x75

data Result r =
      ParseFail JsonParseError
    | ParseMore [JsonEvent] (UArray Word8 -> Result r)
    | ParseOK   (UArray Word8) [JsonEvent] r

doMore :: [JsonEvent] -> (UArray Word8 -> Result r) -> Result r
doMore evs = ParseMore (reverse evs)

type Next a r = JsonParseContext -> UArray Word8 -> [JsonEvent] -> a -> Result r

newtype Parser a = Parser
    { runParser :: forall r .
                   JsonParseContext
                -> UArray Word8
                -> [JsonEvent]
                -> Next a r
                -> Result r }

instance Functor Parser where
    fmap f fa = Parser $ \pc s evs next ->
        runParser fa pc s evs (\pc' s' evs' a -> next pc' s' evs' $ f a)

instance Applicative Parser where
    pure a = Parser $ \pc s evs next -> next pc s evs a
    fab <*> fa = Parser $ \pc s evs next ->
        runParser fab pc s evs    $ \pc1 s1 evs1 ab ->
        runParser fa pc1 s1 evs1  $ \pc2 s2 evs2 a  ->
            next pc2 s2 evs2 (ab a)

instance Monad Parser where
    return    = pure
    ma >>= mb = Parser $ \pc s evs next ->
        runParser ma pc s evs $ \pc' s' evs' a ->
        runParser (mb a) pc' s' evs' next

pGo :: Parser ()
pGo = skipSpaces >> nextByte >>= \c ->
        case c of
            CH_LBRACE -> pObjectBegin
            CH_LSQR   -> pArrayBegin
            CH_SLASH  -> pCommentC >> pGo
            CH_HASH   -> pCommentYaml >> pGo
            _         -> yieldError "go" (UnexpectedChar c)

pOk :: Parser ()
pOk = skipSpaces >> nextByte >>= \c ->
    case c of
        CH_RBRACE -> pObjectEnd
        CH_RSQR   -> pArrayEnd
        CH_COMMA  -> pCommaSep
        CH_SLASH  -> pCommentC >> pOk
        CH_HASH   -> pCommentYaml >> pOk
        _         -> yieldError "ok" (UnexpectedChar c)

pObject :: Parser ()
pObject = skipSpaces >> nextByte >>= \c ->
    case c of
        CH_RBRACE -> pObjectEnd
        CH_QUOTE  -> pKeyString
        CH_SLASH  -> pCommentC >> pObject
        CH_HASH   -> pCommentYaml >> pObject
        _         -> yieldError "object" (UnexpectedChar c)

pKey :: Parser ()
pKey = skipSpaces >> nextByte >>= \c ->
    case c of
        CH_QUOTE  -> pKeyString
        CH_SLASH  -> pCommentC >> pKey
        CH_HASH   -> pCommentYaml >> pKey
        _         -> yieldError "key" (UnexpectedChar c)

pValue :: Parser ()
pValue = skipSpaces >> nextByte >>= \c ->
    case c of
        CH_LBRACE     -> pObjectBegin
        CH_LSQR       -> pArrayBegin
        CH_QUOTE      -> pString
        CH_SLASH      -> pCommentC >> pValue
        CH_MINUS      -> pMinus
        CH_0          -> pLeadingZero
        CH_f          -> pFalse
        CH_n          -> pNull
        CH_t          -> pTrue
        CH_HASH       -> pCommentYaml >> pValue
        _ | isDigit c -> pIntegral c
          | otherwise -> yieldError "value" (UnexpectedChar c)

pArray :: Parser ()
pArray = skipSpaces >> nextByte >>= \c ->
    case c of
        CH_LBRACE     -> pObjectBegin
        CH_LSQR       -> pArrayBegin
        CH_RSQR       -> pArrayEnd
        CH_QUOTE      -> pString
        CH_SLASH      -> pCommentC >> pArray
        CH_MINUS      -> pMinus
        CH_0          -> pLeadingZero
        CH_f          -> pFalse
        CH_n          -> pNull
        CH_t          -> pTrue
        CH_HASH       -> pCommentYaml >> pArray
        _ | isDigit c -> pIntegral c
          | otherwise -> yieldError "array" (UnexpectedChar c)

pColon :: Parser ()
pColon = skipSpaces >> nextByte >>= \c ->
    case c of
        CH_COLON -> pKeySep
        CH_SLASH -> pCommentC >> pColon
        CH_HASH  -> pCommentYaml >> pColon
        _        -> yieldError "colon" (UnexpectedChar c)

pTrue, pFalse, pNull :: Parser ()
pTrue = expect [CH_r,CH_u,CH_e] >> yield JsonTrue >> pOk
pFalse = expect [CH_a,CH_l,CH_s,CH_e] >> yield JsonFalse >> pOk
pNull = expect [CH_u,CH_l,CH_l] >> yield JsonNull >> pOk

pKeySep :: Parser ()
pKeySep = pValue

pCommaSep :: Parser ()
pCommaSep = stackEither pValue pKey

pObjectBegin, pObjectEnd, pArrayBegin, pArrayEnd :: Parser ()
pObjectBegin = yield JsonObjectBegin >> pushStack ContextObject >> pObject
pObjectEnd = popStackObject >> yield JsonObjectEnd >> pOk
pArrayBegin = yield JsonArrayBegin >> pushStack ContextArray >> pArray
pArrayEnd = popStackArray >> yield JsonArrayEnd >> pOk

pString :: Parser ()
pString = pStringVal (JsonString . JString) >> pOk

pKeyString :: Parser ()
pKeyString = pStringVal (JsonKey . JKey) >> pColon

pStringVal :: (String -> JsonEvent)
           -> Parser ()
pStringVal toEvent = loop []
  where
    loop acc = nextByte >>= \c ->
        case c of
            CH_QUOTE -> yield (toEvent $ S.fromBytesUnsafe $ A.reverse $ fromList $ acc)
            CH_BACKS -> escape acc
            v        -> loop (v:acc)

    escape acc = nextByte >>= \c -> do
        let parseMulti u = do
                expect [CH_BACKS, CH_u]
                uLow <- parseUnicodeEscapeValue
                if not (isLowSurrogate uLow)
                    then yieldError "escape" (UnicodeMissingLowSurrogate)
                    else
                         let v  = 0x10000 + ((u .&. 0x3ff) .<<. 10) + (uLow .&. 0x3ff)
                             v1 = integralDownsize (v .>>. 18)            .|. 0xf0
                             v2 = integralDownsize ((v .>>. 12) .&. 0x3f) .|. 0x80
                             v3 = integralDownsize ((v .>>. 6)  .&. 0x3f) .|. 0x80
                             v4 = integralDownsize (v .&. 0x3f)           .|. 0x80
                          in loop (v1:v2:v3:v4:acc)

            write2 uVal =do
                let v1 = integralDownsize (uVal .>>. 6)   .|. 0xc0
                    v2 = integralDownsize (uVal .&. 0x3f) .|. 0x80
                 in loop (v1 : v2 : acc)
            write3 uVal = do
                let v1 = integralDownsize (uVal .>>. 12)           .|. 0xe0
                    v2 = integralDownsize ((uVal .>>. 6) .&. 0x3f) .|. 0x80
                    v3 = integralDownsize (uVal .&. 0x3f)          .|. 0x80
                 in loop (v1 : v2 : v3 : acc)

        case c of
            CH_u     -> do
                u <- parseUnicodeEscapeValue
                if      u < 0x80          then loop (integralDownsize u : acc)
                else if isLowSurrogate u  then yieldError "escape" (UnicodeUnexpectedLowSurrogate)
                else if isHighSurrogate u then parseMulti u
                else if u < 0x800         then write2 u
                else                           write3 u
            CH_t     -> loop (CH_TAB:acc)
            CH_r     -> loop (CH_CR:acc)
            CH_b     -> loop (CH_BS:acc)
            CH_f     -> loop (CH_FF:acc)
            CH_n     -> loop (CH_LF:acc)
            CH_QUOTE -> loop (CH_QUOTE:acc)
            CH_BACKS -> loop (CH_BACKS:acc)
            CH_SLASH -> loop (CH_SLASH:acc)
            _        -> yieldError "escape" (UnexpectedChar c)

    parseUnicodeEscapeValue :: Parser Word32
    parseUnicodeEscapeValue = toUni <$> hexDigit <*> hexDigit <*> hexDigit <*> hexDigit

    isLowSurrogate v = (v .&. 0xfc00) == 0xdc00
    isHighSurrogate v = (v .&. 0xfc00) == 0xd800

    toUni a b c d = (integralUpsize a .<<. 24) + (integralUpsize b .<<. 16) + (integralUpsize c .<<. 8) + integralUpsize d

    -- only accept digits '0' .. '9' and lower case 'a' .. 'f'
    hexDigit = nextByte >>= \c ->
        if      isDigit c              then pure (c - CH_0)
        else if c >= CH_a && c <= CH_f then pure (10 + c - CH_a)
        else                                yieldError "hexDigit" (UnexpectedChar c)

-- | Parse C style comment starting with /* and finishing with */
pCommentC :: Parser ()
pCommentC = isCommentCAllowed >> expect [CH_STAR] >> loop
  where
    loop = skipUntil CH_STAR >> isEnd
    isEnd = nextByte >>= \c ->
        case c of
            CH_SLASH -> pure ()
            CH_STAR  -> isEnd
            _        -> loop
    isCommentCAllowed = do
        cfg <- getConfig
        if allowCommentC cfg then pure () else yieldError "comment-c" CommentNotAllowed

-- | Parse YAML style comment starting with # and finishing at end of line
pCommentYaml :: Parser ()
pCommentYaml = isCommentYamlAllowed >> skipUntilNewline
  where
    isCommentYamlAllowed = do
        cfg <- getConfig
        if allowCommentYaml cfg then pure () else yieldError "comment-yaml" CommentNotAllowed

-- | parser for a number after a minus
pMinus :: Parser ()
pMinus = nextByte >>= \c ->
    if c >= CH_0 && c <= CH_9
        then (if c == CH_0 then pLeadingZero else pIntegral c)
        else yieldError "MX" (UnexpectedChar c)

-- | parser after a leading zero
--
-- we either accept that it's a integral zero, or
-- a floating number.
pLeadingZero :: Parser ()
pLeadingZero = nextByte >>= \c ->
    if c == CH_DOT
        then pFloatingNumber "0"
        else do
            yield (JsonInt $ JInt "0")
            case c of
                CH_RSQR       -> pArrayEnd
                CH_RBRACE     -> pObjectEnd
                CH_COMMA      -> pCommaSep
                CH_SLASH      -> pCommentC >> pOk
                CH_HASH       -> pCommentYaml >> pOk
                _ | isWhite c -> pOk
                  | otherwise -> yieldError "LeadingZero" (UnexpectedChar c)

pIntegral :: Word8 -> Parser ()
pIntegral x = getDigits0 x end
  where
    end acc c
        | c == CH_DOT            = pFloatingNumber acc
        | c == CH_e || c == CH_E = pFloatingExponant acc ""
        | otherwise              = do
            yield (JsonInt $ JInt acc)
            case c of
                CH_RSQR       -> pArrayEnd
                CH_RBRACE     -> pObjectEnd
                CH_COMMA      -> pCommaSep
                CH_SLASH      -> pCommentC >> pOk
                CH_HASH       -> pCommentYaml >> pOk
                _ | isWhite c -> pOk
                  | otherwise -> yieldError "pIntegral" (UnexpectedChar c)

-- parse the floating part of a number
--
-- it need to contains at least 1 digit followed by
-- an optional
pFloatingNumber :: String -> Parser ()
pFloatingNumber integralAcc = getDigits1 $ \floatingAcc c ->
    if c == CH_e || c == CH_E
        then pFloatingExponant integralAcc floatingAcc
        else do
            yield (finalizeFloat floatingAcc)
            case c of
                CH_RSQR   -> pArrayEnd
                CH_RBRACE -> pObjectEnd
                CH_COMMA  -> pCommaSep
                CH_SLASH  -> pCommentC >> pOk
                CH_HASH   -> pCommentYaml >> pOk
                _         -> yieldError "pFloatingNumber" (UnexpectedChar c)
  where
    finalizeFloat floats = JsonFloat $ JFloat $ integralAcc <> "." <> floats

-- parse floating number exponant
--
-- [+-] followed by one to many digits
pFloatingExponant :: String -> String -> Parser ()
pFloatingExponant integralAcc floatingAcc = nextByte >>= \c ->
    case c of
        CH_PLUS       -> getDigits1 $ end True
        CH_MINUS      -> getDigits1 $ end False
        _ | isDigit c -> getDigits0 c $ end True
          | otherwise -> yieldError "pFloatingExponant" (UnexpectedChar c)
  where
    end positive acc c = do
        yield $ JsonFloat $ JFloat $ integralAcc <> "." <> floatingAcc <> "e" <> (if positive then "" else "-") <> acc
        case c of
            CH_RSQR   -> pArrayEnd
            CH_RBRACE -> pObjectEnd
            CH_COMMA  -> pCommaSep
            CH_SLASH  -> pCommentC >> pOk
            CH_HASH   -> pCommentYaml >> pOk
            _         -> yieldError "pFloatingExponant" (UnexpectedChar c)

skipSpaces :: Parser ()
skipSpaces = Parser $ \pcs s evs next ->
    let s' = snd $ A.span isWhite s
     in if A.null s'
            then doMore evs (\x -> runParser skipSpaces pcs x [] next)
            else next pcs s' evs ()

skipUntilNewline :: Parser ()
skipUntilNewline = skipUntil CH_LF

skipUntil :: Word8 -> Parser ()
skipUntil v = Parser $ \pcs s evs next ->
    let s' = snd $ A.span (\x -> x /= v) s
     in if A.null s'
            then doMore evs (\x -> runParser skipUntilNewline pcs x [] next)
            else next pcs (A.drop 1 s') evs ()


nextByte :: Parser Word8
nextByte = Parser $ \pcs s evs next ->
    case A.uncons s of
        Nothing     -> doMore evs (\x -> runParser nextByte pcs x [] next)
        Just (c,s') -> next pcs s' evs c

getDigits1 :: (String -> Word8 -> Parser a) -> Parser a
getDigits1 f =
    nextByte >>= \c ->
        if isDigit c
            then getDigits0 c f
            else yieldError "getDigits1" (UnexpectedChar c)

getDigits0 :: Word8 -> (String -> Word8 -> Parser a) -> Parser a
getDigits0 initAcc f = loop [initAcc]
  where
    loop acc = nextByte >>= \c ->
        if isDigit c
            then loop (c : acc)
            else f (reverseAsciiString acc) c
    reverseAsciiString l = S.fromBytesUnsafe $ A.reverse $ fromList l

expect :: [Word8] -> Parser ()
expect l = loop l
  where
    loop []     = pure ()
    loop (x:xs) = nextByte >>= \c ->
        if c == x then loop xs else yieldError "expect" (UnexpectedChar x)

yield :: JsonEvent -> Parser ()
yield ev = Parser $ \pcs s evs next -> next pcs s (ev : evs) ()

pushStack :: (Context -> Context) -> Parser ()
pushStack b = Parser $ \pcs s evs next ->
    -- TODO check nesting < limits
    let newContext = pcs { contextStack = b (contextStack pcs), contextStackSize = succ (contextStackSize pcs) }
     in next newContext s evs ()

getConfig :: Parser JsonParseConfiguration
getConfig = Parser $ \pcs s evs next -> next pcs s evs (contextConfig pcs)

popStackArray :: Parser ()
popStackArray =  Parser $ \pcs s evs next ->
    case contextStack pcs of
        ContextEmpty    -> ParseFail PopEmpty
        ContextObject _ -> ParseFail PopUnexpectedMode
        ContextArray xs -> next (pcs { contextStack = xs, contextStackSize = pred (contextStackSize pcs) }) s evs ()

popStackObject :: Parser ()
popStackObject =  Parser $ \pcs s evs next ->
    case contextStack pcs of
        ContextEmpty     -> ParseFail PopEmpty
        ContextArray _   -> ParseFail PopUnexpectedMode
        ContextObject xs -> next (pcs { contextStack = xs, contextStackSize = pred (contextStackSize pcs) }) s evs ()

stackEither :: Parser a -> Parser a -> Parser a
stackEither arrayMode objectMode = Parser $ \pcs s evs next ->
    case contextStack pcs of
        ContextEmpty    -> ParseFail CommaOutOfStructure
        ContextArray _  -> runParser arrayMode pcs s evs next
        ContextObject _ -> runParser objectMode pcs s evs next

yieldError :: String -> JsonParseError -> Parser a
yieldError _ err = Parser $ \_ _ _ _ -> ParseFail err

-- white '\n' '\r' '\t' ' '
isWhite :: Word8 -> Bool
isWhite w = w == CH_LF || w == CH_TAB || w == CH_CR || w == CH_SP

isDigit :: Word8 -> Bool
isDigit w = w >= CH_0 && w <= CH_9
