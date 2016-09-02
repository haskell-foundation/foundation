{-# LANGUAGE TypeFamilies #-}
module Main (main) where

import           Criterion.Main
import           Foundation
import qualified Foundation.Collection as C
import           GHC.ST
import qualified Prelude

main :: IO ()
main = defaultMain
    [ bgroup "String" $
        benches (Proxy :: Proxy ([Char]    -> String))        '€' <$> [100000, 1000000, 10000000]
    , bgroup "UArray Word32" $
        benches (Proxy :: Proxy ([Word32]  -> UArray Word32)) 128 <$> [100000, 1000000, 10000000]
    , bgroup "Array Integer" $
        benches (Proxy :: Proxy ([Integer] -> Array Integer)) 128 <$> [100000, 1000000, 10000000]
    ]
  where
    input n e = C.take n (Prelude.repeat e)

    builder es = runST $ C.build 65536 $ Prelude.mapM_ C.append es

    benches proxy c n = bgroup (show n) $
        let !chars = input n c
        in  [ bench "Buildable" $ whnf ( builder  `asProxyTypeOf` proxy ) chars
            , bench "fromList"  $ whnf ( fromList `asProxyTypeOf` proxy ) chars
            ]
