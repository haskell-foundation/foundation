module Fake.Vector
    ( Vector
    , fromList
    , length
    , splitAt
    , take
    , takeWhile
    , break
    , reverse
    , filter
    , foldl'
    , foldr
    , and
    , all
    , any
    ) where

data Vector ty = Vector

fromList _  = Vector
length      = undefined
splitAt _ _ = (undefined, undefined)
take        = undefined
break   _ _ = (undefined, undefined)
takeWhile _ _ = undefined
reverse     = undefined
filter _    = undefined
foldl' _ _ _ = undefined
foldr _ _ _ = undefined
and _ _ = undefined
all _ _ = undefined
any _ _ = undefined
