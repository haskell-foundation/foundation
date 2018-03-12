{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Foundation as F
import qualified Foundation.Collection as F
import qualified Foundation.String as F
import           Gauge.Main

s :: F.String
s = "Sed ut perspiciatis, unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam eaque ipsa, quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt, explicabo. Nemo enim ipsam voluptatem, quia voluptas sit, aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos, qui ratione voluptatem sequi nesciunt, neque porro quisquam est, qui dolorem ipsum, quia dolor sit amet consectetur adipisci[ng] velit, sed quia non numquam [do] eius modi tempora inci[di]dunt, ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit, qui in ea voluptate velit esse, quam nihil molestiae consequatur, vel illum, qui dolorem eum fugiat, quo voluptas nulla pariatur?"

c :: Char
c = '?'

l :: Int
l = F.length s - 1

cAscii :: F.Word8
cAscii = fromIntegral $ fromEnum c 

main = defaultMain
    [ bgroup "splitAt" [ bgroup "String"
        [ bench "splitAt 10" $ whnf (F.splitAt 10) s
        , bench "splitAt 100" $ whnf (F.splitAt 100) s
        , bench ("splitAt " ++ show l) $ whnf (F.splitAt l) s
        ]
    ]]
