-- |
-- Module      : Core.String.ModifiedUTF8
-- License     : BSD-style
-- Maintainer  : foundation
-- Stability   : experimental
-- Portability : portable
--
-- A String type backed by a Modified UTF8 encoded byte array and all the
-- necessary functions to manipulate the string.
--
-- You can think of String as a specialization of a byte array that
-- have element of type Char.
--
-- The String data must contain UTF8 valid data.
--

{-# LANGUAGE MagicHash #-}
module Core.String.ModifiedUTF8
    ( fromModified
    ) where

import           GHC.ST (runST, ST)
import           GHC.Prim (Addr#)
import           GHC.Ptr (Ptr(..))
import qualified Control.Monad (mapM)

import           Core.Internal.Base
import           Core.Internal.Types
import qualified Core.Array.Unboxed as Vec
import           Core.Array.Unboxed (UArray)
import           Core.Number
import           Core.Array.Unboxed.Builder
import           Core.Primitive.FinalPtr
import           Core.String.UTF8Table

-- offset of size one
aone :: Offset Word8
aone = Offset 1

-- helper function to read some bytes from the given byte reader
accessBytes :: Offset Word8 -> (Offset Word8 -> Word8) -> ([Word8], Offset Word8)
accessBytes offset getAtIdx = (loop offset, pastEnd)
  where
    nbytes :: Size Word8
    nbytes = Size $ getNbBytes $ getAtIdx offset
    pastEnd :: Offset Word8
    pastEnd = aone + (offset `offsetPlusE` nbytes)
    loop :: Offset Word8 -> [Word8]
    loop off
        | off == pastEnd = []
        | otherwise      = getAtIdx off : loop (off + aone)

buildByteArray :: Addr# -> ST st (UArray Word8)
buildByteArray addr = Vec.UVecAddr (Offset 0) (Size 100000) `fmap`
    toFinalPtr (Ptr addr) (\_ -> return ())

-- | assuming the given ByteArray is a valid modified UTF-8 sequence of bytes
--
-- We only modify the given Unicode Null-character (0xC080) into a null bytes
--
-- FIXME: need to evaluate the kind of modified UTF8 GHC is actually expecting
-- it is plausible they only handle the Null Bytes, which this function actually
-- does.
fromModified :: Addr# -> UArray Word8
fromModified addr = runST $ do
    ba <- buildByteArray addr
    Vec.unsafeIndexer ba buildWithBytes
  where
    buildWithBytes getAt = build 64 $ loopBuilder getAt (Offset 0)
    loopBuilder getAt offset =
        let (bs, noffset) = accessBytes offset getAt
         in case bs of
              [] -> internalError "ModifiedUTF8.fromModified"
              [0x00] -> return ()
              [b1,b2] | b1 == 0xC0 && b2 == 0x80 -> appendTy 0x00 >> loopBuilder getAt noffset
              _ -> Control.Monad.mapM appendTy bs >> loopBuilder getAt noffset

{-
toModified :: ByteArray -> ByteArray
toModified = undefined
-}
