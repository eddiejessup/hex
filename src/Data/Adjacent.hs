module Data.Adjacent where

data Adjacency a = Adjacency
    { pre :: !(Maybe a)
    , v :: !a
    , post :: !(Maybe a)
    } deriving (Show)

toAdjacents :: [a] -> [Adjacency a]
toAdjacents = go Nothing
  where
    go _ [] =
       []
    go _pre [_v] =
       [Adjacency _pre _v Nothing]
    go _pre (_v : xs@(_post : _)) =
       Adjacency _pre _v (Just _post) : go (Just _v) xs

fromAdjacency :: Adjacency a -> a
fromAdjacency = v

fromAdjacencies :: [Adjacency a] -> [a]
fromAdjacencies = fmap fromAdjacency
