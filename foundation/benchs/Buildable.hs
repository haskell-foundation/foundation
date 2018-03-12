{-# LANGUAGE TypeFamilies #-}
module Main (main) where

import           Gauge.Main
import           Foundation
import qualified Foundation.Collection as C
import           GHC.ST
import qualified Prelude

main :: IO ()
main = defaultMain
    [ bgroup "String" $
        benches (Proxy :: Proxy ([Char]    -> String))       'a' '€' <$> [100000, 1000000, 10000000]
    , bgroup "UArray Word32" $
        benches (Proxy :: Proxy ([Word32]  -> UArray Word32)) 1  128 <$> [100000, 1000000, 10000000]
    , bgroup "Array Integer" $
        benches (Proxy :: Proxy ([Integer] -> Array Integer)) 1  128 <$> [100000, 1000000, 10000000]
    ]
  where
    input n e = C.take n (Prelude.repeat e)

    builder es = runST $ C.build 65536 $ Prelude.mapM_ C.append es

    benches proxy e toE n = bgroup (show n) $
        let !chars = input n e
        in  [ bench "Buildable" $ whnf ( (builder . fmap (const toE)) `asProxyTypeOf` proxy ) chars
            , bench "fromList"  $ whnf ( (fromList . fmap (const toE)) `asProxyTypeOf` proxy ) chars
            ]
