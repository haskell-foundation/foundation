{-# OPTIONS_GHC -O2 -Wall #-}
{-# LANGUAGE BangPatterns, OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS
import Criterion.Main
import Data.ByteString (ByteString, unpack)
import Data.ByteString.Internal (toForeignPtr, unsafeCreate, memcpy)
import qualified Foundation as F
import Foundation.Array
import Foundation.Array.Internal (withPtr, fromForeignPtr, copyToPtr)
import qualified Foundation.Primitive.Block as BLK

fromByteString :: ByteString -> F.UArray F.Word8
fromByteString = fromForeignPtr . toForeignPtr

fromByteString2 :: ByteString -> F.UArray F.Word8
fromByteString2 = fromForeignPtr . toForeignPtr


toByteString :: F.UArray F.Word8 -> ByteString
toByteString v = unsafeCreate len $ \dst -> withPtr v $ \src -> memcpy dst src len
  where !len = F.length v

toByteString2 :: F.UArray F.Word8 -> ByteString
toByteString2 v = unsafeCreate len $ copyToPtr v
  where !len = F.length v

toByteStringBlock :: BLK.Block F.Word8 -> ByteString
toByteStringBlock blk = unsafeCreate len $ BLK.unsafeCopyToPtr blk
  where !len = BLK.length blk

bs = "foundation is the future" :: BS.ByteString
str = fromByteString bs :: UArray F.Word8
str2 = F.fromList (unpack bs) :: UArray F.Word8

blk = BLK.fromList (unpack bs) :: BLK.Block F.Word8

main = defaultMain
  [ bench "toByteString-copyPtr" $ whnf toByteString2 str
  , bench "toByteString-withPtr" $ whnf toByteString str
  , bench "toByteString-native-copyPtr" $ whnf toByteString2 str2
  , bench "toByteString-native-withPtr" $ whnf toByteString str2
  , bench "toByteString-block-copyPtr" $ whnf toByteStringBlock blk
  , bench "BS.copy"  $ whnf BS.copy bs
  ]

