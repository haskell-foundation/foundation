{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ExistentialQuantification #-}
module Foundation.Internal.Error
    ( error
    ) where

import           GHC.Exception( errorCallWithCallStackException )
import           GHC.Prim
import           GHC.Types (RuntimeRep)
import           Foundation.String.UTF8
import           Foundation.Internal.CallStack

-- | stop execution and displays an error message
error :: forall (r :: RuntimeRep) . forall (a :: TYPE r) . HasCallStack => String -> a
error s = raise# (errorCallWithCallStackException (sToList s) ?callstack)
