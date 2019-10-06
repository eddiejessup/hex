module HeX.BreakList.BreakList where

import           HeXlude

import qualified Data.Adjacent      as A

import qualified HeX.Box            as B
import           HeX.BreakList.Glue
import           HeX.Quantity

data BreakItem =
    GlueBreak (Glue Length) | KernBreak B.Kern | PenaltyBreak Penalty | NoBreak
    deriving ( Show )

breakPenalty :: BreakItem -> TeXInt
breakPenalty (PenaltyBreak (Penalty p)) = p
breakPenalty (GlueBreak _)              = 0
breakPenalty (KernBreak _)              = 0
breakPenalty NoBreak                    = 0

newtype Penalty = Penalty TeXInt
    deriving ( Show )

instance Readable Penalty where
    describe (Penalty p) = "|p" <> show p <> "|"

class BreakableListElem a where
    toGlue :: a -> Maybe (Glue Length)

    isDiscardable :: a -> Bool

    isBox :: a -> Bool

    toBreakItem :: A.Adj a -> Maybe BreakItem
