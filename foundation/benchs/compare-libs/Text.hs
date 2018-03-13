#!/usr/bin/env stack -- stack --install-ghc --resolver lts-5.13 runghc --package text --package foundation
module Main where

import qualified Foundation as F
import qualified Foundation.Collection as F
import Criterion.Main

import qualified Data.Text as T

textEnglish = "Set in the year 0 F.E. (\"Foundation Era\"), The Psychohistorians opens on Trantor, the capital of the 12,000-year-old Galactic Empire. Though the empire appears stable and powerful, it is slowly decaying in ways that parallel the decline of the Western Roman Empire. Hari Seldon, a mathematician and psychologist, has developed psychohistory, a new field of science and psychology that equates all possibilities in large societies to mathematics, allowing for the prediction of future events."

textJapanese = "数学者ハリ・セルダンは、膨大な集団の行動を予測する心理歴史学を作りあげ発展させることで、銀河帝国が近いうちに崩壊することを予言する[1]。セルダンは、帝国崩壊後に3万年続くはずの暗黒時代を、あらゆる知識を保存することで千年に縮めようとし、知識の集大成となる銀河百科事典 (Encyclopedia Galactica) を編纂するグループ「ファウンデーション」をつくったが、帝国崩壊を公言し平和を乱したという罪で裁判にかけられ、グループは銀河系辺縁部にある資源の乏しい無人惑星ターミナスへ追放されることになった。しかし、この追放劇すらもセルダンの計画に予定されていた事柄であった。病で死期をさとっていたセルダンは、己の仕事が終わったことを確信する。"

main = defaultMain
    [ bgroup "break"
        [ bgroup "#english-start"
            [ bench "foundation" $ whnf (fst . F.breakElem 'S') (F.fromList textEnglish :: F.String)
            , bench "text" $ nf (fst . T.break (== 'S')) (T.pack textEnglish)
            ]
        , bgroup "#english-middle"
            [ bench "foundation" $ whnf (fst . F.breakElem '2') (F.fromList textEnglish :: F.String)
            , bench "text" $ nf (fst . T.break (== '2')) (T.pack textEnglish)
            ]
        , bgroup "#english-notfound"
            [ bench "foundation" $ whnf (fst . F.breakElem 'z') (F.fromList textEnglish :: F.String)
            , bench "text" $ nf (fst . T.break (== 'z')) (T.pack textEnglish)
            ]
{-
        , bgroup "#japanese"
            [ bench "foundation" $ whnf (fst . F.breakElem '帝') (F.fromList textJapanese :: F.String)
            , bench "text" $ whnf (fst . T.break (== '帝')) (T.pack textJapanese)
            ]
-}
        ]
    ]
