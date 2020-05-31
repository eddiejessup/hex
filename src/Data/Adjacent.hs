module Data.Adjacent where

import qualified Data.Sequence as Seq
import Hexlude

data Adj a = Adj {adjPre :: Maybe a, adjVal :: a, adjPost :: Maybe a}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data ToAdjState a
  = VeryStart
  | GotGoing a (Seq (Adj a))

toAdjacents :: Foldable t => t a -> Seq (Adj a)
toAdjacents xs = case foldl' f VeryStart xs of
  VeryStart -> Empty
  GotGoing onlyElem Empty -> Seq.singleton (Adj Nothing onlyElem Nothing)
  GotGoing finalElem acc@(_ :|> Adj _ left _) -> acc |> Adj (Just left) finalElem Nothing
  where
    f fState right =
      GotGoing right $ case fState of
        VeryStart -> Empty
        GotGoing v acc ->
          let maybeLeft = case acc of
                Empty -> Nothing
                (_ :|> Adj _ left _) -> Just left
          in acc |> Adj maybeLeft v (Just right)

fromAdjacency :: Adj a -> a
fromAdjacency = adjVal

fromAdjacencies :: Functor f => f (Adj a) -> f a
fromAdjacencies = fmap fromAdjacency
