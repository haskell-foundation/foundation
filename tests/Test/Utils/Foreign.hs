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
import           Prelude (head, zip)
import           Control.Monad (forM_)

import           Foundation.Foreign
import           Foundation

createPtr :: Storable e => [e] -> IO (FinalPtr e)
createPtr l
    | null l    = toFinalPtr nullPtr (\_ -> return ())
    | otherwise = do
        let szElem = sizeOf (head l)
            nbBytes = szElem * length l
        ptr <- mallocBytes nbBytes
        forM_ (zip [0..] l) $ \(o, e) -> pokeElemOff ptr o e
        toFinalPtr ptr (\p -> free p)
