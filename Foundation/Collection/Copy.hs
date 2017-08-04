module Foundation.Collection.Copy
    ( Copy(..)
    ) where

import           GHC.ST (runST)
import           Basement.Compat.Base ((>>=))
import qualified Basement.Block as BLK
import qualified Basement.UArray as UA
import qualified Basement.BoxedArray as BA
import qualified Basement.String as S

class Copy a where
    copy :: a -> a
instance Copy [ty] where
    copy a = a
instance UA.PrimType ty => Copy (BLK.Block ty) where
    copy blk = runST (BLK.thaw blk >>= BLK.unsafeFreeze)
instance UA.PrimType ty => Copy (UA.UArray ty) where
    copy = UA.copy
instance Copy (BA.Array ty) where
    copy = BA.copy
instance Copy S.String where
    copy = S.copy
