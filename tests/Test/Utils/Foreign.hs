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
import           Foreign.Storable
import           Foundation
import           Prelude (zip)
import           Control.Monad (forM_)

import           Foundation.Foreign

createPtr :: forall e . Storable e => [e] -> IO (FinalPtr e)
createPtr l
    | null l    = toFinalPtr nullPtr (\_ -> return ())
    | otherwise = do
        let szElem = sizeOf (undefined :: e)
            nbBytes = szElem * length l
        ptr <- mallocBytes nbBytes
        forM_ (zip [0..] l) $ \(o, e) -> pokeElemOff ptr o e
        toFinalPtr ptr (\p -> free p)
