module ForeignUtils
    ( Ptr
    , Storable
    , createPtr
    , free
    ) where

import           Foreign.Marshal.Alloc

import           Foreign.Ptr
import           Foreign.Storable
import           Core
import           Prelude (length, head, zip, null)
import           Control.Monad (forM_)

import           Core.Foreign

createPtr :: Storable e => [e] -> IO (FinalPtr e)
createPtr l
    | null l    = toFinalPtr nullPtr (\_ -> return ())
    | otherwise = do
        let szElem = sizeOf (head l)
            nbBytes = szElem * length l
        ptr <- mallocBytes nbBytes
        forM_ (zip [0..] l) $ \(o, e) -> pokeElemOff ptr o e
        toFinalPtr ptr (\p -> free p)
