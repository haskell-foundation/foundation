{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Utils.Foreign
    ( Ptr
    , Storable
    , createPtr
    , free
    ) where

import           Foreign.Marshal.Alloc

import           Foreign.Ptr
import           Foundation
import           Prelude (zip)
import           Control.Monad (forM_)

import           Foundation.Foreign
import           Foundation.Class.Storable

createPtr :: forall e . StorableFixed e => [e] -> IO (FinalPtr e)
createPtr l
    | null l    = toFinalPtr nullPtr (\_ -> return ())
    | otherwise = do
        let (CountOf szElem) = size (Proxy :: Proxy e)
            nbBytes = szElem * (let (CountOf c) = length l in c)
        ptr <- mallocBytes nbBytes
        forM_ (zip [0..] l) $ uncurry (pokeOff ptr)
        toFinalPtr ptr free
