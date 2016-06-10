-- |
-- Module      : Core.Primitive.FinalPtr
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- A smaller ForeignPtr reimplementation that work in any prim monad.
--
-- Here be dragon.
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}
module Core.Primitive.FinalPtr
    ( FinalPtr
    , toFinalPtr
    , withFinalPtr
    , withUnsafeFinalPtr
    ) where

import GHC.Ptr
import GHC.IO
import Core.Primitive.Monad
import Core.Internal.Primitive
import Core.Internal.Base (return)

-- | Create a pointer with an associated finalizer
data FinalPtr a = FinalPtr (Ptr a)

-- | create a new FinalPtr from a Pointer
toFinalPtr :: PrimMonad prim => Ptr a -> (Ptr a -> IO ()) -> prim (FinalPtr a)
toFinalPtr ptr finalizer = unsafePrimFromIO (primitive makeWithFinalizer)
  where
    makeWithFinalizer s =
        case compatMkWeak# ptr () (finalizer ptr) s of { (# s2, _ #) -> (# s2, FinalPtr ptr #) }

-- | Looks at the raw pointer inside a FinalPtr, making sure the
-- data pointed by the pointer is not finalized during the call to 'f'
withFinalPtr :: PrimMonad prim => FinalPtr p -> (Ptr p -> prim a) -> prim a
withFinalPtr (FinalPtr ptr) f = do
    r <- f ptr
    primTouch ptr
    return r

-- | Unsafe version of 'withFinalPtr'
withUnsafeFinalPtr :: PrimMonad prim => FinalPtr p -> (Ptr p -> prim a) -> a
withUnsafeFinalPtr fptr f = unsafePerformIO (unsafePrimToIO (withFinalPtr fptr f))
