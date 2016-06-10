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

createPtr :: Storable e => [e] -> IO (Ptr e)
createPtr l
    | null l    = return nullPtr
    | otherwise = do
        let szElem = sizeOf (head l)
            nbBytes = szElem * length l
        ptr <- mallocBytes nbBytes
        forM_ (zip [0..] l) $ \(o, e) -> pokeElemOff ptr o e
        return ptr
