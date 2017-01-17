module Foundation.Collection.Copy
    ( Copy(..)
    ) where

import qualified Foundation.Array.Unboxed as UA

class Copy a where
    copy :: a -> a
instance Copy [ty] where
    copy a = a
instance UA.PrimType ty => Copy (UA.UArray ty) where
    copy = UA.copy
