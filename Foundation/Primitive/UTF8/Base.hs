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
import           Foundation.Primitive.FinalPtr
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
next (String array) n =
    case array of
        Vec.UVecBA start _ _ ba   -> PrimBA.next ba (start + n)
        Vec.UVecAddr start _ fptr -> unt2 $ withUnsafeFinalPtr fptr $ \(Ptr ptr) -> pureST $ t2 (PrimAddr.next ptr (start + n))
  where
    pureST :: a -> ST s a
    pureST = pure
    unt2 (a,b) = (# a, b #)
    t2 (# a, b #) = (a, b)

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
