module Foundation.Collection.Copy
    ( Copy(..)
    ) where

import qualified Foundation.Array.Unboxed as UA
import qualified Foundation.Array.Boxed as BA

class Copy a where
    copy :: a -> a
instance Copy [ty] where
    copy a = a
instance UA.PrimType ty => Copy (UA.UArray ty) where
    copy = UA.copy
instance Copy (BA.Array ty) where
    copy = BA.copy
