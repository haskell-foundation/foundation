module Main where

import Common

import qualified Foundation as F
import qualified Foundation.Collection as F

import qualified Data.ByteString as B

dat = [0..255]

abench = bench "bytestring"

main = defaultMain
    [ bgroup "break"
        [ bgroup "#word8-start"
            [ fbench $ whnf (fst . F.breakElem 0) (F.fromList dat :: F.UArray F.Word8)
            , abench $ nf (fst . B.break (== 0)) (B.pack dat)
            ]
        , bgroup "#word8-middle"
            [ fbench $ whnf (fst . F.breakElem 120) (F.fromList dat :: F.UArray F.Word8)
            , abench $ nf (fst . B.break (== 120)) (B.pack dat)
            ]
        , bgroup "#word8-end"
            [ fbench $ whnf (fst . F.breakElem 255) (F.fromList dat :: F.UArray F.Word8)
            , abench $ nf (fst . B.break (== 255)) (B.pack dat)
            ]
{-
        , bgroup "#japanese"
            [ bench "foundation" $ whnf (fst . F.breakElem '帝') (F.fromList textJapanese :: F.String)
            , bench "text" $ whnf (fst . T.break (== '帝')) (T.pack textJapanese)
            ]
-}
        ]
    ]
