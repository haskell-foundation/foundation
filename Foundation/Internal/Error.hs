{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP #-}
module Foundation.Internal.Error
    ( error
    ) where

import           GHC.Prim
import           GHC.Types (RuntimeRep)
import           Foundation.String.UTF8
import           Foundation.Internal.CallStack

#if MIN_VERSION_base(4,9,0)

import           GHC.Exception (errorCallWithCallStackException)

-- | stop execution and displays an error message
error :: forall (r :: RuntimeRep) . forall (a :: TYPE r) . HasCallStack => String -> a
error s = raise# (errorCallWithCallStackException (sToList s) ?callstack)

#else

import           GHC.Exception (errorCallException)

error :: String -> a
error s = raise# (errorCallException (sToList s))

#endif
