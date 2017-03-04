{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Foundation.Network.IPv6
    ( IPv6
    , fromString, toString
    , fromTuple, toTuple
    ) where

import Prelude (fromIntegral)
import qualified Text.Printf as Base
import Data.Char (isSeparator, isHexDigit)
import Numeric (readHex)

import Foundation.Class.Storable
import Foundation.Hashing.Hashable
import Foundation.Numerical.Additive (scale)
import Foundation.Internal.Base
import Foundation.Internal.Proxy
import Foundation.Primitive
import Foundation.Collection (intercalate, span)
import Foundation.String (String)
import Foundation.Bits

type LString = [Char]

-- | IPv6 data type
data IPv6 = IPv6 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  deriving (Eq, Ord, Typeable)
instance Hashable IPv6 where
    hashMix (IPv6 w1 w2) = hashMix (w1, w2)
instance Show IPv6 where
    show = toLString
instance IsString IPv6 where
    fromString = fromLString
instance Storable IPv6 where
    peek ptr = fromTuple <$>
        (   (,,,,,,,)
        <$> (fromBE <$> peekOff ptr' 1)
        <*> (fromBE <$> peekOff ptr' 2)
        <*> (fromBE <$> peekOff ptr' 3)
        <*> (fromBE <$> peekOff ptr' 4)
        <*> (fromBE <$> peekOff ptr' 5)
        <*> (fromBE <$> peekOff ptr' 6)
        <*> (fromBE <$> peekOff ptr' 7)
        <*> (fromBE <$> peekOff ptr' 8)
        )
      where
        ptr' :: Ptr (BE Word16)
        ptr' = castPtr ptr
    poke ptr ipv6 = do
        let (i1,i2,i3,i4,i5,i6,i7,i8) = toTuple ipv6
         in pokeOff ptr' 1 (toBE i1)
         >> pokeOff ptr' 2 (toBE i2)
         >> pokeOff ptr' 3 (toBE i3)
         >> pokeOff ptr' 4 (toBE i4)
         >> pokeOff ptr' 5 (toBE i5)
         >> pokeOff ptr' 6 (toBE i6)
         >> pokeOff ptr' 7 (toBE i7)
         >> pokeOff ptr' 8 (toBE i8)
      where
        ptr' :: Ptr (BE Word16)
        ptr' = castPtr ptr
instance StorableFixed IPv6 where
    size      _ = (size      (Proxy :: Proxy Word64)) `scale` 2
    alignment _ = alignment (Proxy :: Proxy Word64)

toString :: IPv6 -> String
toString = fromList . toLString

toLString :: IPv6 -> [Char]
toLString ipv4 =
    let (i1,i2,i3,i4,i5,i6,i7,i8) = toTuple ipv4
     in intercalate ":" $ showHex4 <$> [i1,i2,i3,i4,i5,i6,i7,i8]

showHex4 :: Word16 -> [Char]
showHex4 c
  | c < 0x10   = '0':'0':'0':showHex c
  | c < 0x100  = '0':'0':showHex c
  | c < 0x1000 = '0':showHex c
  | otherwise  = showHex c

showHex :: Word16 -> [Char]
showHex = Base.printf "%03xh"

fromLString :: [Char] -> IPv6
fromLString s =
    case parseInt' (parseHex $ snd $ span isSeparator s) [] of
        Left err        -> error err
        Right (addr, _) -> addr
    where
        parseInt' :: Either LString (Word16, LString)
                  -> [Word16]
                  -> Either LString (IPv6, LString)
        parseInt' (Left err)           _          = Left err
        parseInt' (Right (w8, xs))     [w7,w6,w5,w4,w3,w2,w1] = Right (fromTuple (w1, w2, w3, w4, w5, w6, w7, w8), xs)
        parseInt' (Right (_,  []))     _          = Left "Not an ipv6 addr"
        parseInt' (Right (w,  ':':xs)) acc        = parseInt' (parseHex xs) (w:acc)
        parseInt' (Right (_,    c:_ )) _          = Left $ "Not an ipv6 addr: unexpected char '" <> [c] <> "'"

        parseHex :: LString
                 -> Either LString (Word16, LString)
        parseHex buf =
            case span isHexDigit buf of
                ([], x:xs) -> case x of
                                ':' -> Right (0, x:xs)
                                _   -> Left $ "Not an ipv6 addr: unexpected char '" <> [x] <> "'"
                (l , xs)   -> let lhs = readHex l :: [(Word16, LString)]
                              in  case lhs of
                                    [(w, [])] -> Right (w, xs)
                                    _ -> Left "can't fall here"



fromTuple :: (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16)
          -> IPv6
fromTuple (i1, i2, i3, i4, i5, i6, i7, i8) = IPv6 hi low
  where
    f :: Word16 -> Word64
    f = fromIntegral
    hi, low :: Word64
    hi =    (f i1 .<<. 48) .&. 0xFFFF000000000000
        .|. (f i2 .<<. 32) .&. 0x0000FFFF00000000
        .|. (f i3 .<<. 16) .&. 0x00000000FFFF0000
        .|. (f i4        ) .&. 0x000000000000FFFF
    low =   (f i5 .<<. 48) .&. 0xFFFF000000000000
        .|. (f i6 .<<. 32) .&. 0x0000FFFF00000000
        .|. (f i7 .<<. 16) .&. 0x00000000FFFF0000
        .|. (f i8        ) .&. 0x000000000000FFFF

toTuple :: IPv6 -> (Word16,Word16,Word16,Word16,Word16,Word16,Word16,Word16)
toTuple (IPv6 hi low) =
    (f w1, f w2, f w3, f w4, f w5, f w6, f w7, f w8)
  where
    f :: Word64 -> Word16
    f = fromIntegral
    w1, w2, w3, w4, w5, w6, w7, w8 :: Word64
    w1 = hi  .>>. 48 .&. 0x0000FFFF
    w2 = hi  .>>. 32 .&. 0x0000FFFF
    w3 = hi  .>>. 16 .&. 0x0000FFFF
    w4 = hi          .&. 0x0000FFFF
    w5 = low .>>. 48 .&. 0x0000FFFF
    w6 = low .>>. 32 .&. 0x0000FFFF
    w7 = low .>>. 16 .&. 0x0000FFFF
    w8 = low         .&. 0x0000FFFF
