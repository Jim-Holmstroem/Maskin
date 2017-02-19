module Map
    ( Map
    , get
    , set
    , empty
    ) where


data Map k v = Map [(k, v)]
    deriving (Show, Eq)


empty :: (Eq k) => Map k v
empty = Map []

get :: (Eq k) => k -> Map k v -> Maybe v
get k (Map []) = Nothing
get k (Map ((k', v):kvs))
        | k == k' = Just v
        | otherwise = get k (Map kvs)

set :: (Eq k) => (k, v) -> Map k v -> Map k v
set (k, v) (Map kvs) = Map $ set' (k, v) kvs
    where set' (k, v) [] = [(k, v)]
          set' (k, v) ((k', v'):kvs)
              | k == k' = (k', v'):kvs
              | otherwise = set' (k, v) kvs
