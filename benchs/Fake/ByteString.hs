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

import Prelude (undefined)

data ByteString = ByteString

pack _      = ByteString
length      = undefined
splitAt _ _ = (undefined, undefined)
take        = undefined
break   _ _ = (undefined, undefined)
reverse     = undefined
filter _    = undefined
readInt _    = undefined
readInteger _ = undefined
