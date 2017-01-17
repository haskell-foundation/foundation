-- |
-- Module      : Foundation.String.ModifiedUTF8
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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash        #-}
module Foundation.String.ModifiedUTF8
    ( fromModified
    ) where

import           GHC.ST (runST, ST)
import           GHC.Prim (Addr#)
import           Control.Monad (mapM_)

import           Foundation.Internal.Base
import           Foundation.Internal.Types
import qualified Foundation.Array.Unboxed as Vec
import           Foundation.Array.Unboxed (UArray)
import           Foundation.Numerical
import           Foundation.Primitive.FinalPtr
import           Foundation.String.UTF8Table

-- helper function to read some bytes from the given byte reader
accessBytes :: Offset Word8 -> (Offset Word8 -> Word8) -> ([Word8], Offset Word8)
accessBytes offset getAtIdx = (loop offset, pastEnd)
  where
    nbytes :: Size Word8
    nbytes = Size $ getNbBytes $ getAtIdx offset
    pastEnd :: Offset Word8
    pastEnd = 1 + (offset `offsetPlusE` nbytes)
    loop :: Offset Word8 -> [Word8]
    loop off
        | off == pastEnd = []
        | otherwise      = getAtIdx off : loop (off + 1)

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
    buildWithBytes getAt = Vec.builderBuild 64 $ loopBuilder getAt (Offset 0)
    loopBuilder getAt offset =
        case bs of
            [] -> internalError "ModifiedUTF8.fromModified"
            [0x00] -> return ()
            [b1,b2] | b1 == 0xC0 && b2 == 0x80 -> Vec.builderAppend 0x00 >> loopBuilder getAt noffset
            _ -> mapM_ Vec.builderAppend bs >> loopBuilder getAt noffset
      where
        (bs, noffset) = accessBytes offset getAt

{-
toModified :: ByteArray -> ByteArray
toModified = undefined
-}
