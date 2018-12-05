module Data.Adjacent where

newtype Adjacency a =
  Adjacency (Maybe a, a, Maybe a)
  deriving (Show)

toAdjacents :: [a] -> [Adjacency a]
toAdjacents = inner Nothing
  where
    inner _       []     = []
    inner _before [this] = [Adjacency (_before, this, Nothing)]
    inner _before (this : _after : rest) =
        Adjacency (_before, this, Just _after)
            : inner (Just this) (_after : rest)

fromAdjacent :: Adjacency a -> a
fromAdjacent (Adjacency (_, a, _)) = a

fromAdjacents :: [Adjacency a] -> [a]
fromAdjacents = fmap fromAdjacent
