-- |
-- Module      : Foundation.String.UTF8
-- License     : BSD-style
-- Maintainer  : Foundation
--
-- A String type backed by a UTF8 encoded byte array and all the necessary
-- functions to manipulate the string.
--
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE CPP                        #-}
module Foundation.Primitive.UTF8.Base
    where

import           GHC.ST (ST, runST)
import           GHC.Int
import           GHC.Types
import           GHC.Word
import           GHC.Prim
import           Foundation.Internal.Base
import           Foundation.Internal.Primitive
import           Foundation.Numerical
import           Foundation.Bits
import           Foundation.Primitive.NormalForm
import           Foundation.Primitive.Types.OffsetSize
import           Foundation.Primitive.Monad
import           Foundation.Primitive.UTF8.Table
import           Foundation.Primitive.UTF8.Helper
import           Foundation.Array.Unboxed           (UArray)
import qualified Foundation.Array.Unboxed           as Vec
import qualified Foundation.Array.Unboxed           as C
import           Foundation.Array.Unboxed.ByteArray (MutableByteArray)
import qualified Foundation.Array.Unboxed.Mutable   as MVec
import           Foundation.String.ModifiedUTF8     (fromModified)
import           GHC.CString                        (unpackCString#, unpackCStringUtf8#)

import           Data.Data
import           Foundation.Boot.List as List

-- | Opaque packed array of characters in the UTF8 encoding
newtype String = String (UArray Word8)
    deriving (Typeable, Monoid, Eq, Ord)

-- | Mutable String Buffer.
--
-- Use as an *append* buffer, as UTF8 variable encoding
-- doesn't really allow to change previously written
-- character without potentially shifting bytes.
newtype MutableString st = MutableString (MutableByteArray st)
    deriving (Typeable)

instance Show String where
    show = show . sToList
instance IsString String where
    fromString = sFromList
instance IsList String where
    type Item String = Char
    fromList = sFromList
    toList = sToList

instance Data String where
    toConstr s   = mkConstr stringType (show s) [] Prefix
    dataTypeOf _ = stringType
    gunfold _ _  = error "gunfold"

instance NormalForm String where
    toNormalForm (String ba) = toNormalForm ba

stringType :: DataType
stringType = mkNoRepType "Foundation.String"

-- | size in bytes.
--
-- this size is available in o(1)
size :: String -> CountOf Word8
size (String ba) = Vec.length ba

-- | Convert a String to a list of characters
--
-- The list is lazily created as evaluation needed
sToList :: String -> [Char]
sToList s = loop 0
  where
    !nbBytes = size s
    loop idx
        | idx .==# nbBytes = []
        | otherwise        =
            let (# c , idx' #) = next s idx in c : loop idx'

{-# RULES
"String sFromList" forall s .
  sFromList (unpackCString# s) = String $ fromModified s
  #-}
{-# RULES
"String sFromList" forall s .
  sFromList (unpackCStringUtf8# s) = String $ fromModified s
  #-}

-- | Create a new String from a list of characters
--
-- The list is strictly and fully evaluated before
-- creating the new String, as the size need to be
-- computed before filling.
sFromList :: [Char] -> String
sFromList l = runST (new bytes >>= startCopy)
  where
    -- count how many bytes
    !bytes = List.sum $ fmap (charToBytes . fromEnum) l

    startCopy :: MutableString (PrimState (ST st)) -> ST st String
    startCopy ms = loop 0 l
      where
        loop _   []     = freeze ms
        loop idx (c:xs) = write ms idx c >>= \idx' -> loop idx' xs
{-# INLINE [0] sFromList #-}

next :: String -> Offset8 -> (# Char, Offset8 #)
next (String ba) n =
    case getNbBytes# h of
        0# -> (# toChar h, n + 1 #)
        1# -> (# toChar (decode2 (Vec.unsafeIndex ba (n + 1))) , n + 2 #)
        2# -> (# toChar (decode3 (Vec.unsafeIndex ba (n + 1))
                                 (Vec.unsafeIndex ba (n + 2))) , n + 3 #)
        3# -> (# toChar (decode4 (Vec.unsafeIndex ba (n + 1))
                                 (Vec.unsafeIndex ba (n + 2))
                                 (Vec.unsafeIndex ba (n + 3))) , n + 4 #)
        r -> error ("next: internal error: invalid input: offset=" <> show n <> " table=" <> show (I# r) <> " h=" <> show (W# h))
  where
    !(W8# h) = Vec.unsafeIndex ba n

    toChar :: Word# -> Char
    toChar w = C# (chr# (word2Int# w))

    decode2 :: Word8 -> Word#
    decode2 (W8# c1) =
        or# (uncheckedShiftL# (and# h 0x1f##) 6#)
            (and# c1 0x3f##)

    decode3 :: Word8 -> Word8 -> Word#
    decode3 (W8# c1) (W8# c2) =
        or# (uncheckedShiftL# (and# h 0xf##) 12#)
            (or# (uncheckedShiftL# (and# c1 0x3f##) 6#)
                 (and# c2 0x3f##))

    decode4 :: Word8 -> Word8 -> Word8 -> Word#
    decode4 (W8# c1) (W8# c2) (W8# c3) =
        or# (uncheckedShiftL# (and# h 0x7##) 18#)
            (or# (uncheckedShiftL# (and# c1 0x3f##) 12#)
                (or# (uncheckedShiftL# (and# c2 0x3f##) 6#)
                    (and# c3 0x3f##))
            )

-- A variant of 'next' when you want the next character
-- to be ASCII only. if Bool is False, then it's not ascii,
-- otherwise it is and the return Word8 is valid.
nextAscii :: String -> Offset8 -> (# Word8, Bool #)
nextAscii (String ba) n = (# w, not (testBit w 7) #)
  where
    !w = Vec.unsafeIndex ba n

expectAscii :: String -> Offset8 -> Word8 -> Bool
expectAscii (String ba) n v = Vec.unsafeIndex ba n == v
{-# INLINE expectAscii #-}

write :: PrimMonad prim => MutableString (PrimState prim) -> Offset8 -> Char -> prim Offset8
write (MutableString mba) i c =
    if      bool# (ltWord# x 0x80##   ) then encode1
    else if bool# (ltWord# x 0x800##  ) then encode2
    else if bool# (ltWord# x 0x10000##) then encode3
    else                                     encode4
  where
    !(I# xi) = fromEnum c
    !x       = int2Word# xi

    encode1 = Vec.unsafeWrite mba i (W8# x) >> return (i + 1)

    encode2 = do
        let x1  = or# (uncheckedShiftRL# x 6#) 0xc0##
            x2  = toContinuation x
        Vec.unsafeWrite mba i     (W8# x1)
        Vec.unsafeWrite mba (i+1) (W8# x2)
        return (i + 2)

    encode3 = do
        let x1  = or# (uncheckedShiftRL# x 12#) 0xe0##
            x2  = toContinuation (uncheckedShiftRL# x 6#)
            x3  = toContinuation x
        Vec.unsafeWrite mba i     (W8# x1)
        Vec.unsafeWrite mba (i+1) (W8# x2)
        Vec.unsafeWrite mba (i+2) (W8# x3)
        return (i + 3)

    encode4 = do
        let x1  = or# (uncheckedShiftRL# x 18#) 0xf0##
            x2  = toContinuation (uncheckedShiftRL# x 12#)
            x3  = toContinuation (uncheckedShiftRL# x 6#)
            x4  = toContinuation x
        Vec.unsafeWrite mba i     (W8# x1)
        Vec.unsafeWrite mba (i+1) (W8# x2)
        Vec.unsafeWrite mba (i+2) (W8# x3)
        Vec.unsafeWrite mba (i+3) (W8# x4)
        return (i + 4)

    toContinuation :: Word# -> Word#
    toContinuation w = or# (and# w 0x3f##) 0x80##
{-# INLINE write #-}

-- | Allocate a MutableString of a specific size in bytes.
new :: PrimMonad prim
    => Size8 -- ^ in number of bytes, not of elements.
    -> prim (MutableString (PrimState prim))
new n = MutableString `fmap` MVec.new n

freeze :: PrimMonad prim => MutableString (PrimState prim) -> prim String
freeze (MutableString mba) = String `fmap` C.unsafeFreeze mba
{-# INLINE freeze #-}
