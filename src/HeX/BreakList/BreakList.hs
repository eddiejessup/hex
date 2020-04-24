module Hex.BreakList.BreakList where

import qualified Data.Adjacent as A
import qualified Hex.Box as B
import Hex.BreakList.Glue
import Hex.Quantity
import Hexlude

data BreakItem
  = GlueBreak (Glue Length)
  | KernBreak B.Kern
  | PenaltyBreak Penalty
  | NoBreak
  deriving stock Show

breakPenalty :: BreakItem -> TeXInt
breakPenalty (PenaltyBreak (Penalty p)) = p
breakPenalty (GlueBreak _) = 0
breakPenalty (KernBreak _) = 0
breakPenalty NoBreak = 0

newtype Penalty = Penalty TeXInt
  deriving stock Show

instance Describe Penalty where

  describe (Penalty p) = singleLine $ "Penalty " <> quote (show p)

class BreakableListElem a where

  toGlue :: a -> Maybe (Glue Length)

  isDiscardable :: a -> Bool

  isBox :: a -> Bool

  toBreakItem :: A.Adj a -> Maybe BreakItem
