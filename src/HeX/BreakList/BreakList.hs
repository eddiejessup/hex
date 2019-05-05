module HeX.BreakList.BreakList where

import           HeXlude

import qualified Data.Adjacent      as A

import qualified HeX.Box            as B
import           HeX.BreakList.Glue

data BreakItem =
    GlueBreak Glue | KernBreak B.Kern | PenaltyBreak Penalty | NoBreak
    deriving ( Show )

breakPenalty :: BreakItem -> Int
breakPenalty (PenaltyBreak (Penalty p)) = p
breakPenalty (GlueBreak _) = 0
breakPenalty (KernBreak _) = 0
breakPenalty NoBreak = 0

newtype Penalty = Penalty Int
    deriving ( Show )

instance Readable Penalty where
    describe (Penalty p) = "|p" <> show p <> "|"

class BreakableListElem a where
    toGlue :: a -> Maybe Glue

    isDiscardable :: a -> Bool

    isBox :: a -> Bool

    toBreakItem :: A.Adj a -> Maybe BreakItem

    naturalSpan :: a -> Int

naturalListSpan :: (Functor f, Foldable f) => BreakableListElem a => f a -> Int
naturalListSpan = sum . fmap naturalSpan

glues :: (Filterable f, BreakableListElem a) => f a -> f Glue
glues = mapMaybe toGlue

totalGlue :: (Filterable f, BreakableListElem a, Foldable f) => f a -> Glue
totalGlue = mconcat . glues
