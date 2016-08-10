{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Foundation as F
import qualified Foundation.Collection as F
import qualified Foundation.String as F
import           Criterion.Main

s :: F.String
s = "Sed ut perspiciatis, unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam eaque ipsa, quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt, explicabo. Nemo enim ipsam voluptatem, quia voluptas sit, aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos, qui ratione voluptatem sequi nesciunt, neque porro quisquam est, qui dolorem ipsum, quia dolor sit amet consectetur adipisci[ng] velit, sed quia non numquam [do] eius modi tempora inci[di]dunt, ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit, qui in ea voluptate velit esse, quam nihil molestiae consequatur, vel illum, qui dolorem eum fugiat, quo voluptas nulla pariatur?"

c :: Char
c = '?'

cAscii :: F.Word8
cAscii = fromIntegral $ fromEnum c 

main = defaultMain
    [ bgroup "break"
        [ bench "string" $ whnf (fst . F.breakElem c) s
        , bench "uvec" $ whnf (fst . F.breakElem cAscii) (F.toBytes F.UTF8 s)
        ]
    ]
