-- this is a scratchpad for idea
-- not meant to be exported, or be compiled as part of foundation
module Foundation.Idea where

import Foundation.Collection.Element

class X c where
    xmap :: (Element c -> Element d) -> c -> d

--isLength :: (Int -> Bool) -> c -> Bool
--isLength (> 5) []
--isLength (== 0) []
