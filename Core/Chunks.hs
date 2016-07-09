-- |
-- Module      : Core.Chunks
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- Chunks of collections that itself is a collection
--
-- Can be used as lazy stream.
--
module Core.Chunks
    ( Chunks(..)
    , reChunk
    ) where

import           Core.Internal.Base
import qualified Core.Collection as C
import           Control.Monad (mplus)
import           Core.Number

-- | Chunks of data structure
--
-- Represent a 'lazy' stream of data.
data Chunks s =
      End
    | Chunk !s (Chunks s)

--newtype Chunks a = Chunks [a]

type instance C.Element (Chunks a) = C.Element a

instance IsList s => IsList (Chunks s) where
    type Item (Chunks s) = Item s
    toList End         = []
    toList (Chunk s r) = toList s <> toList r
    fromList l         = Chunk (fromList l) End

instance Monoid (Chunks s) where
    mempty  = End
    mappend = append
    mconcat = concat

instance (IsList s, C.InnerFunctor s) => C.InnerFunctor (Chunks s) where
    imap _ End         = End
    imap f (Chunk s r) = Chunk (C.imap f s) (C.imap f r)

instance (IsList s, C.InnerFunctor s, C.Sequential s)
        => C.Sequential (Chunks s) where
    null = null
    take = take
    drop = drop
    splitAt = splitAt
    splitOn = splitOn
    break = break
    span = span
    reverse = reverse
    filter = filter
    snoc = snoc
    cons = cons
    find = find
    sortBy = sortBy
    length = length
    singleton x = Chunk (C.singleton x) End

append :: Chunks s -> Chunks s -> Chunks s
append s1  End = s1
append End s2  = s2
append s1  s2  = appendEnd s1
  where appendEnd (Chunk r s) = Chunk r (appendEnd s)
        appendEnd End         = s2

concat :: [Chunks s] -> Chunks s
concat []      = End
concat (s1:sl) = appendEnd s1 sl
  where appendEnd (Chunk r s) l      = Chunk r (appendEnd s l)
        appendEnd End         []     = End
        appendEnd End         (x:xs) = appendEnd x xs

length :: C.Sequential s => Chunks s -> Int
length = sumLength 0
  where
    sumLength !sum End  = sum
    sumLength !sum (Chunk s k) = sumLength (sum + C.length s) k

snoc :: C.Sequential s
     => Chunks s -> C.Element (Chunks s) -> Chunks s
snoc End         el = Chunk (C.singleton el) End
snoc (Chunk s r) el = Chunk s (snoc r el)

cons :: C.Sequential s
     => C.Element (Chunks s) -> Chunks s -> Chunks s
cons e = Chunk (C.singleton e)

find :: C.Sequential s
     => (C.Element (Chunks s) -> Bool) -> Chunks s -> Maybe (C.Element (Chunks s))
find _ End         = Nothing
find f (Chunk s r) = C.find f s `mplus` find f r

sortBy :: C.Sequential s
       => (C.Element (Chunks s) -> C.Element (Chunks s) -> Ordering)
       -> Chunks s
       -> Chunks s
sortBy = undefined

null :: C.Sequential s => Chunks s -> Bool
null End           = True
null (Chunk s End) = C.null s
null (Chunk s l)   = C.null s && null l

take :: C.Sequential s => Int -> Chunks s -> Chunks s
take n l
    | n <= 0    = End
    | otherwise =
        case l of
            End       -> End
            Chunk s r ->
                let len = C.length s
                 in if len >= n
                        then Chunk (C.take n s) End
                        else Chunk s (take (n - len) r)

drop :: C.Sequential s => Int -> Chunks s -> Chunks s
drop n l
    | n <= 0    = l
    | otherwise =
        case l of
            End        -> End
            Chunk s r  ->
                let len = C.length s
                 in if len >= n
                        then Chunk (C.drop n s) r
                        else drop (n - len) l

splitAt :: C.Sequential s => Int -> Chunks s -> (Chunks s, Chunks s)
splitAt _ End = (End, End)
splitAt nbElements chunks
    | nbElements <= 0 = (End, chunks)
    | otherwise       = loop nbElements id chunks
  where
    loop _ toChunks End = (toChunks End, End)
    loop n toChunks (Chunk s r)
        | len >= n  = let (s1, s2) = C.splitAt n s in (toChunks (Chunk s1 End), Chunk s2 r)
        | otherwise = loop (n - len) (\x -> toChunks (Chunk s x)) r
      where len = C.length s

splitOn :: (C.Element (Chunks s) ~ Item s, C.Sequential s) => (C.Element (Chunks s) -> Bool) -> Chunks s -> [Chunks s]
splitOn predicate = fmap fromList . C.splitOn predicate . toList
    {-
    loop id
  where
    loop toChunk (Chunk s r) =
        case C.splitOn predicate s of
            []   -> loop toChunk r
            [x]  -> loop
            x:xs -> undefined
    loop toChunk End = toChunk End
    -}

break :: (C.Element (Chunks s) ~ Item s, C.Sequential s) => (C.Element (Chunks s) -> Bool) -> Chunks s -> (Chunks s, Chunks s)
break predicate = loop id
  where
    loop toChunk (Chunk s r) =
        let (s1,s2) = C.break predicate s
         in if C.null s2
            then loop (\x -> toChunk (Chunk s x)) r
            else (toChunk (Chunk s1 End), Chunk s2 r)
    loop toChunk End = (toChunk End, End)

span :: (C.Element (Chunks s) ~ Item s, C.Sequential s) => (C.Element (Chunks s) -> Bool) -> Chunks s -> (Chunks s, Chunks s)
span predicate = break (not . predicate)

reverse :: C.Sequential s => Chunks s -> Chunks s
reverse = loop End
  where
    loop stream End         = stream
    loop stream (Chunk s r) = loop (Chunk (C.reverse s) stream) r

filter :: (C.Element (Chunks s) ~ Item s, C.Sequential s) => (C.Element (Chunks s) -> Bool) -> Chunks s -> Chunks s
filter f = mapChunks (C.filter f)

mapChunks :: (s -> s) -> Chunks s -> Chunks s
mapChunks _ End         = End
mapChunks f (Chunk s r) = Chunk (f s) (mapChunks f r)

{-
foldElementChunks :: (e -> C.Element (Chunks s) -> e) -> e -> Chunks s -> e
foldElementChunks f initialAcc = loop initialAcc
  where
    loop !e End         = e
    loop !e (Chunk s r) = loop (f e s) r
-}

foldChunks :: (e -> s -> e) -> e -> Chunks s -> e
foldChunks f initialAcc = loop initialAcc
  where
    loop !e End         = e
    loop !e (Chunk s r) = loop (f e s) r

-- | Recreate chunks of specific size from a stream of chunks of
-- each unknown and arbitrary size
reChunk :: C.Sequential s => Int -> Chunks s -> Chunks s
reChunk n c =
    let (s1, s2) = splitAt n c
        s' = mconcat $ C.reverse $ foldChunks (flip (:)) [] s1
     in Chunk s' (reChunk n s2)
