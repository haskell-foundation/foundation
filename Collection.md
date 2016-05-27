
Various functions:

```haskell
prepending and appending only work for implicit int indexing

snoc :: c -> Element c -> c
cons :: Element c -> c -> c
```

Basic manipulation of collection

```haskell
insert :: Key c -> Element c -> c -> c
update :: (Element c -> Element c) -> Key c -> c -> c
delete :: Key c -> c -> (c, Bool)
alter :: (Maybe (Element c) -> Maybe (Element c)) -> Key c -> c -> (c, Ternary: Created/Deleted/Modified)
```

Creation

```haskell
singleton :: Element c -> c
singletonKey :: Key c -> Element c -> c

-- this mean that you can create keys implicitely from 0 .. N -> contiguity
replicate :: Int -> Element c -> c

create :: Int -- ^ size
       -> (Int -> Element c) -> c

-- supply a list of key to populate: solve previous problem but not ideal for collection with contiguous indexing
replicateKeys :: [Key c] -> Element c -> c
```

```haskell
find :: (Element c -> Bool) -> c -> Maybe (Element c)
findKey :: (Key c -> Bool) -> c -> Maybe (Key c)
findWithKey :: (Key c -> Element c -> Bool) -> c -> Maybe (Key c, Element c)
```

Others:

```haskell
sortBy :: (Element c -> Element c -> Ordering) -> c -> c
```

```haskell
unionWith :: (Key c -> Element c -> Element d -> Element e)
          -> c
          -> d
          -> e
```

```haskell
map :: Key c ~ Key d => (Element c -> Element d) -> c -> d
mapWithKey :: Key c ~ Key d => (Key c -> Element c -> Element d) -> c -> d

-- interesting development:
fromListToMap :: [a] -> Map Int a
fromListToMap = map id

-- what about Map Int a -> [a] .. contiguity ?

```

Collection properties:

|                      | String     | [a]        | Vector a   | Set v | Map k v | HashSet v | HashMap k v |
|----------------------|------------|------------|------------|-------|---------|-----------|-------------|
| Dimension            | 1+Pos      | 1+Pos      | 1+Pos      | 1     | 2       | 1         |             |
| Indexing             | Int (impl) | Int (impl) | Int (impl) | v     | k       | v         | k           |
| Element              | Char       | a          | a          | ()    | v       | ()        | v           |
| Length               | o(1)       | o(n)       | o(1)       |       |         |           |             |
| Ordered              | ✔          | ✔          | ✔          | ✔     | ✔       |           |             |
| Sorted               |            |            |            | ✔     | ✔       |           |             |
| Order Preserving     | ✔          | ✔          | ✔          |       |         |           |             |
| Duplicate Allowed    | ✔          | ✔          | ✔          |       |         |           |             |

Operation supported by different collections:

|                      | String     | [a]        | Vector a   | Set v | Map k v | HashSet v | HashMap k v |
|----------------------|------------|------------|------------|-------|---------|-----------|-------------|
| snoc                 | ✔          | ✔          | ✔          |       |         |           |             |
| cons                 | ✔          | ✔          | ✔          |       |         |           |             |
| singleton            | ✔          | ✔          | ✔          |       |         |           |             |
| replicate            | ✔          | ✔          | ✔          |       |         |           |             |
| concat, append       | ✔          | ✔          | ✔          | ✔     | ✔       | ✔         | ✔           |
| take,drop,break,span | ✔          | ✔          | ✔          |       |         |           |             |
| splitAt,splitOn      | ✔          | ✔          | ✔          |       |         |           |             |
| empty, null          | ✔          | ✔          | ✔          | ✔     | ✔       | ✔         | ✔           |
| foldl, foldr         | ✔          | ✔          | ✔          | ✔     | ✔       | ✔         | ✔           |
| foldlKey, foldrKey   | ✔          | ✔          | ✔          |       | ✔       |           |             |
| map                  | ✔          | ✔          | ✔          | ✔     | ✔       | ✔         | ✔           |
| mapWithKey           | ✔          | ✔          | ✔          |       | ✔       |           |             |
| lookup               | ✔          | ✔          | ✔          |       | ✔       | ✔         | ✔           |
| elem,find            | ✔          | ✔          | ✔          | ✔     | ✔       | ✔         | ✔           |
| filter               | ✔          | ✔          | ✔          | ✔     | ✔       | ✔         | ✔           |
| partition            | ✔          | ✔          | ✔          | ✔     | ✔       | ✔         | ✔           |
| reverse              | ✔          | ✔          | ✔          | ⹂     | ⹂       | ⹂         | ⹂           |
| union,intersect      |            |            |            | ✔     | ✔       | ✔         | ✔           |
| keys                 | ✔          | ✔          | ✔          | ✔     | ✔       |           |             |
| values               |            |            |            | ⹂     | ✔       | ✔         | ✔           |
| zip, zipWith         | ✔          | ✔          | ✔          | ✔     | ✔       | ✔         | ✔           |
| delete               | ✔          | ✔          | ✔          | ✔     | ✔       | ✔         | ✔           |
| insert               | ✔          | ✔          | ✔          | ✔     | ✔       | ✔         | ✔           |
| update               | ✔          | ✔          | ✔          | ✔     | ✔       | ✔         | ✔           |
| sort, sortWith       | ✔          | ✔          | ✔          | ⹂     | ⹂       | ⹂         | ⹂           |
