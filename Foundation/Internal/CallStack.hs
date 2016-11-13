{-# LANGUAGE CPP #-}
module Foundation.Internal.CallStack
    ( HasCallStack
    ) where

#if MIN_VERSION_base(4,9,0)

import GHC.Stack (HasCallStack)

#elif MIN_VERSION_base(4,8,0)

import qualified GHC.Stack

type HasCallStack = (?callStack :: GHC.Stack.CallStack)

#else

import GHC.Exts (Constraint)

type HasCallStack = (() :: Constraint)

#endif
