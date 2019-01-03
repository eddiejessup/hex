module Data.Adjacent where

data Adj a = Adj
    { pre :: !(Maybe a)
    , val :: !a
    , post :: !(Maybe a)
    } deriving (Show)

toAdjacents :: [a] -> [Adj a]
toAdjacents = go Nothing
  where
    go _pre xs = case xs of
        []   -> []
        [_v] -> [Adj _pre _v Nothing]
        (_v : ys@(_post : _)) -> (Adj _pre _v (Just _post)) : go (Just _v) ys

fromAdjacency :: Adj a -> a
fromAdjacency = val

fromAdjacencies :: [Adj a] -> [a]
fromAdjacencies = fmap fromAdjacency
