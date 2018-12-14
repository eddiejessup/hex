module HeX.BreakList.BreakList where

import           Data.Maybe                    ( mapMaybe )

import qualified Data.Adjacent                 as A

import qualified HeX.Box                       as B
import           HeX.Dimensioned                ( Dimensioned(..) )
import           HeX.BreakList.Glue

data BreakItem
  = GlueBreak Glue
  | KernBreak B.Kern
  | PenaltyBreak Penalty
  | NoBreak
  deriving (Show)

breakPenalty :: BreakItem -> Int
breakPenalty (GlueBreak _) = 0
breakPenalty (KernBreak _) = 0
breakPenalty NoBreak = 0
breakPenalty (PenaltyBreak (Penalty p)) = p

newtype Penalty = Penalty Int

instance Show Penalty where
  show (Penalty p) = "|p" ++ show p ++ "|"

class BreakableListElem a where
  toGlue :: a -> Maybe Glue

  isDiscardable :: a -> Bool

  isBox :: a -> Bool

  toBreakItem :: A.Adjacency a -> Maybe BreakItem

  naturalLength :: a -> Int

  naturalListLength :: [a] -> Int
  naturalListLength = sum . fmap naturalLength

  glues :: [a] -> [Glue]
  glues = mapMaybe toGlue

  totalGlue :: [a] -> Glue
  totalGlue = mconcat . glues
