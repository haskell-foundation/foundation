{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ConstraintKinds       #-}
module Basement.From
    ( From(..)
    , Into
    , TryFrom(..)
    , TryInto
    , into
    , tryInto
    ) where

import Basement.Compat.Base
import Basement.IntegralConv

-- | Class of things that can be converted from a to b
class From a b where
    from :: a -> b

type Into b a = From a b

-- | Same as from but reverse the type variable so that the destination type can be specified first
--
-- e.g. converting:
--
-- from @_ @Word (10 :: Int)
--
-- into @Word (10 :: Int)
--
into :: Into b a => a -> b
into = from

-- | Class of things that can mostly be converted from a to b, but with possible error cases.
class TryFrom a b where
    tryFrom :: a -> Maybe b

type TryInto b a = TryFrom a b

-- | same as tryFrom but reversed
tryInto :: TryInto b a => a -> Maybe b
tryInto = tryFrom

instance From a a where
    from = id

instance From Int Word where
    from = integralCast
instance From Word Int where
    from = integralCast
