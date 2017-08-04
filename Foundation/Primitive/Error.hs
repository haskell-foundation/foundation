{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP #-}
module Foundation.Primitive.Error
    ( error
    ) where

import           GHC.Prim
import           Foundation.Primitive.UTF8.Base
import           Foundation.Primitive.Compat.CallStack

#if MIN_VERSION_base(4,9,0)

import           GHC.Types (RuntimeRep)
import           GHC.Exception (errorCallWithCallStackException)

-- | stop execution and displays an error message
error :: forall (r :: RuntimeRep) . forall (a :: TYPE r) . HasCallStack => String -> a
error s = raise# (errorCallWithCallStackException (sToList s) ?callstack)

#elif MIN_VERSION_base(4,7,0)

import           GHC.Exception (errorCallException)

error :: String -> a
error s = raise# (errorCallException (sToList s))

#else

import           GHC.Types
import           GHC.Exception

error :: String -> a
error s = throw (ErrorCall (sToList s))

#endif
