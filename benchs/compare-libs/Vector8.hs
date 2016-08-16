module Main where

import BenchUtil.Common


import qualified Foundation as F
import qualified Foundation.Collection as F

import qualified Data.Vector.Unboxed as V

dat = [0..255]

fdat :: F.UArray F.Word8
fdat = F.fromList dat

vdat :: V.Vector F.Word8
vdat = V.fromList dat

abench = bench "vector-unboxed"

main = defaultMain
    [ bgroup "break"
        [ bgroup "#word8-start"
            [ fbench $ whnf (fst . F.breakElem 0) fdat
            , abench $ nf (fst . V.break (== 0)) vdat
            ]
        , bgroup "#word8-middle"
            [ fbench $ whnf (fst . F.breakElem 120) fdat
            , abench $ nf (fst . V.break (== 120)) vdat
            ]
        , bgroup "#word8-end"
            [ fbench $ whnf (fst . F.breakElem 255) fdat
            , abench $ nf (fst . V.break (== 255)) vdat
            ]
{-
        , bgroup "#japanese"
            [ bench "foundation" $ whnf (fst . F.breakElem '帝') (F.fromList textJapanese :: F.String)
            , bench "text" $ whnf (fst . T.break (== '帝')) (T.pack textJapanese)
            ]
-}
        ]
    ]
