module Data.Adjacent where

data Adjacency a =
  Adjacency { pre :: Maybe a
            , v :: a
            , post :: Maybe a}
  deriving (Show)

toAdjacents :: [a] -> [Adjacency a]
toAdjacents = inner Nothing
 where
  inner _    []  = []
  inner _pre [_v] = [Adjacency _pre _v Nothing]
  inner _pre (_v : xs@(_post : _)) =
    Adjacency _pre _v (Just _post) : inner (Just _v) xs

fromAdjacency :: Adjacency a -> a
fromAdjacency = v

fromAdjacencies :: [Adjacency a] -> [a]
fromAdjacencies = fmap fromAdjacency
