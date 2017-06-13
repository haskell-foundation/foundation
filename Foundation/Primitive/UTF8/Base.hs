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
import           Foundation.Numerical
import           Foundation.Bits
import           Foundation.Class.Bifunctor
import           Foundation.Primitive.NormalForm
import           Foundation.Primitive.Types.OffsetSize
import           Foundation.Primitive.Monad
import           Foundation.Primitive.Types
import           Foundation.Primitive.FinalPtr
import           Foundation.Primitive.UTF8.Table
import           Foundation.Primitive.UTF8.Helper
import qualified Foundation.Primitive.UTF8.BA       as PrimBA
import qualified Foundation.Primitive.UTF8.Addr     as PrimAddr
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
        0# -> (# toChar# h, n + 1 #)
        1# -> (# toChar# (decode2 (Vec.unsafeIndex ba (n + 1))) , n + 2 #)
        2# -> (# toChar# (decode3 (Vec.unsafeIndex ba (n + 1))
                                 (Vec.unsafeIndex ba (n + 2))) , n + 3 #)
        3# -> (# toChar# (decode4 (Vec.unsafeIndex ba (n + 1))
                                 (Vec.unsafeIndex ba (n + 2))
                                 (Vec.unsafeIndex ba (n + 3))) , n + 4 #)
        r -> error ("next: internal error: invalid input: offset=" <> show n <> " table=" <> show (I# r) <> " h=" <> show (W# h))
  where
    !(W8# h) = Vec.unsafeIndex ba n

    decode2 :: Word8 -> Word#
    decode2 (W8# c1) =
        or# (uncheckedShiftL# (maskHeader2# h) 6#)
            (maskContinuation# c1)

    decode3 :: Word8 -> Word8 -> Word#
    decode3 (W8# c1) (W8# c2) =
        or3# (uncheckedShiftL# (maskHeader3# h) 12#)
             (uncheckedShiftL# (maskContinuation# c1) 6#)
             (maskContinuation# c2)

    decode4 :: Word8 -> Word8 -> Word8 -> Word#
    decode4 (W8# c1) (W8# c2) (W8# c3) =
        or4# (uncheckedShiftL# (maskHeader4# h) 18#)
             (uncheckedShiftL# (maskContinuation# c1) 12#)
             (uncheckedShiftL# (maskContinuation# c2) 6#)
             (maskContinuation# c3)

-- Given a non null offset, give the previous character and the offset of this character
-- will fail bad if apply at the beginning of string or an empty string.
prevBA :: ByteArray# -> Offset8 -> (# Char, Offset8 #)
prevBA ba offset =
    case primBaIndex ba prevOfs1 of
        (W8# v1) | isContinuation# v1 -> atLeast2 (maskContinuation# v1)
                 | otherwise          -> (# toChar# v1, prevOfs1 #)
  where
    sz1 = CountOf 1
    !prevOfs1 = offset `offsetMinusE` sz1
    prevOfs2 = prevOfs1 `offsetMinusE` sz1
    prevOfs3 = prevOfs2 `offsetMinusE` sz1
    prevOfs4 = prevOfs3 `offsetMinusE` sz1
    atLeast2 !v  =
        case primBaIndex ba prevOfs2 of
            (W8# v2) | isContinuation# v2 -> atLeast3 (or# (uncheckedShiftL# (maskContinuation# v2) 6#) v)
                     | otherwise          -> (# toChar# (or# (uncheckedShiftL# (maskHeader2# v2) 6#) v), prevOfs2 #)
    atLeast3 !v =
        case primBaIndex ba prevOfs3 of
            (W8# v3) | isContinuation# v3 -> atLeast4 (or# (uncheckedShiftL# (maskContinuation# v3) 12#) v)
                     | otherwise          -> (# toChar# (or# (uncheckedShiftL# (maskHeader3# v3) 12#) v), prevOfs2 #)
    atLeast4 !v =
        case primBaIndex ba prevOfs4 of
            (W8# v4) -> (# toChar# (or# (uncheckedShiftL# (maskHeader4# v4) 18#) v), prevOfs4 #)

-- Given a non null offset, give the previous character and the offset of this character
-- will fail bad if apply at the beginning of string or an empty string.
prevAddr :: Addr# -> Offset8 -> (# Char, Offset8 #)
prevAddr ba offset =
    case primAddrIndex ba prevOfs1 of
        (W8# v1) | isContinuation# v1 -> atLeast2 (maskContinuation# v1)
                 | otherwise          -> (# toChar# v1, prevOfs1 #)
  where
    sz1 = CountOf 1
    !prevOfs1 = offset `offsetMinusE` sz1
    prevOfs2 = prevOfs1 `offsetMinusE` sz1
    prevOfs3 = prevOfs2 `offsetMinusE` sz1
    prevOfs4 = prevOfs3 `offsetMinusE` sz1
    atLeast2 !v  =
        case primAddrIndex ba prevOfs2 of
            (W8# v2) | isContinuation# v2 -> atLeast3 (or# (uncheckedShiftL# (maskContinuation# v2) 6#) v)
                     | otherwise          -> (# toChar# (or# (uncheckedShiftL# (maskHeader2# v2) 6#) v), prevOfs2 #)
    atLeast3 !v =
        case primAddrIndex ba prevOfs3 of
            (W8# v3) | isContinuation# v3 -> atLeast4 (or# (uncheckedShiftL# (maskContinuation# v3) 12#) v)
                     | otherwise          -> (# toChar# (or# (uncheckedShiftL# (maskHeader3# v3) 12#) v), prevOfs2 #)
    atLeast4 !v =
        case primAddrIndex ba prevOfs4 of
            (W8# v4) -> (# toChar# (or# (uncheckedShiftL# (maskHeader4# v4) 18#) v), prevOfs4 #)

prevSkipBA :: ByteArray# -> Offset8 -> Offset8
prevSkipBA ba offset = loop (offset `offsetMinusE` sz1)
  where
    sz1 = CountOf 1
    loop o
        | isContinuation (primBaIndex ba o) = loop (o `offsetMinusE` sz1)
        | otherwise                         = o

prevSkipAddr :: Addr# -> Offset8 -> Offset8
prevSkipAddr addr offset = loop (offset `offsetMinusE` sz1)
  where
    sz1 = CountOf 1
    loop o
        | isContinuation (primAddrIndex addr o) = loop (o `offsetMinusE` sz1)
        | otherwise                             = o

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
write (MutableString marray) ofs c =
    case marray of
        MVec.MUVecMA start _ _ mba  -> PrimBA.write mba (start + ofs) c
        MVec.MUVecAddr start _ fptr -> withFinalPtr fptr $ \(Ptr ptr) -> PrimAddr.write ptr (start + ofs) c

-- | Allocate a MutableString of a specific size in bytes.
new :: PrimMonad prim
    => Size8 -- ^ in number of bytes, not of elements.
    -> prim (MutableString (PrimState prim))
new n = MutableString `fmap` MVec.new n

newNative :: PrimMonad prim
          => CountOf Word8 -- ^ in number of bytes, not of elements.
          -> (MutableByteArray# (PrimState prim) -> prim a)
          -> prim (a, MutableString (PrimState prim))
newNative n f = second MutableString `fmap` MVec.newNative n f

freeze :: PrimMonad prim => MutableString (PrimState prim) -> prim String
freeze (MutableString mba) = String `fmap` C.unsafeFreeze mba
{-# INLINE freeze #-}

freezeShrink :: PrimMonad prim
             => CountOf Word8
             -> MutableString (PrimState prim)
             -> prim String
freezeShrink n (MutableString mba) = String `fmap` C.unsafeFreezeShrink mba n
