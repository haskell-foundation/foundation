module Fake.ByteString
    ( ByteString
    , pack
    , length
    , splitAt
    , take
    , break
    , reverse
    , filter
    , readInt
    , readInteger
    ) where

import Prelude (undefined, Maybe(..))

data ByteString = ByteString

pack _      = ByteString
length      = undefined
splitAt _ _ = (undefined, undefined)
take        = undefined
break   _ _ = (undefined, undefined)
reverse     = undefined
filter _    = undefined

readInt :: ByteString -> Maybe (a,b)
readInt _    = undefined
readInteger :: ByteString -> Maybe (a,b)
readInteger _ = undefined
