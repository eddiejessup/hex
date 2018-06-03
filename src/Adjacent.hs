module Adjacent where

newtype Adjacency a = Adjacency (Maybe a, a, Maybe a) deriving Show

toAdjacentsInner :: Maybe a -> [a] -> [Adjacency a]
toAdjacentsInner _ [] = []
toAdjacentsInner _before [this] = [Adjacency (_before, this, Nothing)]
toAdjacentsInner _before (this:_after:rest) =
  Adjacency (_before, this, Just _after):toAdjacentsInner (Just this) (_after:rest)

toAdjacents :: [a] -> [Adjacency a]
toAdjacents = toAdjacentsInner Nothing

fromAdjacent :: Adjacency a -> a
fromAdjacent (Adjacency (_, a, _)) = a

fromAdjacents :: [Adjacency a] -> [a]
fromAdjacents = fmap fromAdjacent
