module HeX.BreakList.Elem where

import qualified Adjacent as A

import Data.Maybe
import qualified HeX.Box as B
import HeX.Box (Dimensioned, naturalWidth)

import HeX.BreakList.Glue
import HeX.BreakList.Judge

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

newtype Penalty =
  Penalty Int

instance Show Penalty where
  show (Penalty p) = "|p" ++ show p ++ "|"

class Show a => BreakableListElem a where
  toGlue :: a -> Maybe Glue
  isDiscardable :: a -> Bool
  isBox :: a -> Bool
  toBreakItem :: A.Adjacency a -> Maybe BreakItem
  naturalLength :: a -> Int

naturalListLength :: BreakableListElem a => [a] -> Int
naturalListLength = sum . fmap naturalLength

-- TODO: WhatsIt, Leaders, Mark, Insertion
-- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust
data BreakableHListElem
  = HListBox B.Box
  | HRule B.Rule
  | HGlue Glue
  | HKern B.Kern
  | HPenalty Penalty
  | HFontDefinition B.FontDefinition
  | HFontSelection B.FontSelection
  | HCharacter B.Character

data BreakableVListElem
  = VListBox B.Box
  | VRule B.Rule
  | VGlue Glue
  | VKern B.Kern
  | VPenalty Penalty
  | VFontDefinition B.FontDefinition
  | VFontSelection B.FontSelection

-- Show.

instance Show BreakableHListElem where
  show (HListBox e) = show e
  show (HRule e) = show e
  show (HGlue e) = show e
  show (HKern e) = show e
  show (HPenalty e) = show e
  show (HFontDefinition e) = show e
  show (HFontSelection e) = show e
  show (HCharacter e) = show e
  showList a = (show (foldr append [] a) ++)
    where
      append (HCharacter B.Character {B.char = c}) [] = [Sentence [c]]
      append x [] = [NonSentence x]
      append y r@(x:xs)
        | HCharacter B.Character {B.char = c} <- y
        , (Sentence cs) <- x = Sentence (c : cs) : xs
        | HCharacter B.Character {B.char = c} <- y = Sentence [c] : r
        | otherwise = NonSentence y : r

-- Just for the purposes of showing the list more compactly.
data CondensedHListElem
  = Sentence String
  | NonSentence BreakableHListElem

instance Show CondensedHListElem where
  show (Sentence s) = s
  show (NonSentence x) = show x

instance Show BreakableVListElem where
  show (VListBox e) = show e
  show (VRule e) = show e
  show (VGlue e) = show e
  show (VKern e) = show e
  show (VPenalty e) = show e
  show (VFontDefinition e) = show e
  show (VFontSelection e) = show e

-- Dimensioned.

instance Dimensioned BreakableHListElem where
  naturalWidth (HListBox b) = B.naturalWidth b
  naturalWidth (HRule r) = B.naturalWidth r
  naturalWidth (HGlue g) = dimen g
  naturalWidth (HKern k) = B.kernDimen k
  naturalWidth (HPenalty _) = 0
  naturalWidth (HFontDefinition _) = 0
  naturalWidth (HFontSelection _) = 0
  naturalWidth (HCharacter c) = naturalWidth c
  naturalHeight (HListBox b) = B.naturalHeight b
  naturalHeight (HRule r) = B.naturalHeight r
  naturalHeight (HGlue _) = 0
  naturalHeight (HKern _) = 0
  naturalHeight (HPenalty _) = 0
  naturalHeight (HFontDefinition _) = 0
  naturalHeight (HFontSelection _) = 0
  naturalHeight (HCharacter c) = B.naturalHeight c
  naturalDepth (HListBox b) = B.naturalDepth b
  naturalDepth (HRule r) = B.naturalDepth r
  naturalDepth (HGlue _) = 0
  naturalDepth (HKern _) = 0
  naturalDepth (HPenalty _) = 0
  naturalDepth (HFontDefinition _) = 0
  naturalDepth (HFontSelection _) = 0
  naturalDepth (HCharacter c) = B.naturalDepth c

instance Dimensioned BreakableVListElem where
  naturalWidth (VListBox b) = B.naturalWidth b
  naturalWidth (VRule r) = B.naturalWidth r
  naturalWidth (VGlue _) = 0
  naturalWidth (VKern _) = 0
  naturalWidth (VPenalty _) = 0
  naturalWidth (VFontDefinition _) = 0
  naturalWidth (VFontSelection _) = 0
  naturalHeight (VListBox b) = B.naturalHeight b
  naturalHeight (VRule r) = B.naturalHeight r
  naturalHeight (VGlue g) = dimen g
  naturalHeight (VKern k) = B.kernDimen k
  naturalHeight (VPenalty _) = 0
  naturalHeight (VFontDefinition _) = 0
  naturalHeight (VFontSelection _) = 0
  naturalDepth (VListBox b) = B.naturalDepth b
  naturalDepth (VRule r) = B.naturalDepth r
  naturalDepth (VGlue _) = 0
  naturalDepth (VKern _) = 0
  naturalDepth (VPenalty _) = 0
  naturalDepth (VFontDefinition _) = 0
  naturalDepth (VFontSelection _) = 0

-- BreakableListElem.

instance BreakableListElem BreakableHListElem where
  toGlue (HGlue g) = Just g
  toGlue _ = Nothing
  isDiscardable (HGlue _) = True
  isDiscardable (HKern _) = True
  isDiscardable (HPenalty _) = True
  isDiscardable _ = False
  isBox (HListBox _) = True
  isBox (HRule _) = True
  isBox (HCharacter _) = True
  isBox _ = False
    -- TODO: Add math formula conditions.
  toBreakItem (A.Adjacency a) =
    case a of
      (Just x, HGlue g, _) ->
        if isDiscardable x
          then Nothing
          else Just $ GlueBreak g
      (_, HKern k, Just (HGlue _)) -> Just $ KernBreak k
      (_, HPenalty p, _) -> Just $ PenaltyBreak p
    -- TODO: Discretionary break and Math-off.
      _ -> Nothing
  naturalLength = B.naturalWidth

instance BreakableListElem BreakableVListElem where
  toGlue (VGlue g) = Just g
  toGlue _ = Nothing
  isDiscardable (VGlue _) = True
  isDiscardable (VKern _) = True
  isDiscardable (VPenalty _) = True
  isDiscardable _ = False
  isBox (VListBox _) = True
  isBox (VRule _) = True
  isBox _ = False
  toBreakItem (A.Adjacency a) =
    case a of
      (Just x, VGlue g, _) ->
        if isDiscardable x
          then Nothing
          else Just $ GlueBreak g
      (_, VKern k, Just VGlue {}) -> Just $ KernBreak k
      (_, VPenalty p, _) -> Just $ PenaltyBreak p
      _ -> Nothing
  naturalLength = B.naturalHeight

listGlueStatus :: BreakableListElem a => Int -> [a] -> GlueStatus
listGlueStatus desiredLength cs =
  let totGlue = mconcat $ mapMaybe toGlue cs
  in glueStatus (naturalListLength cs - desiredLength) totGlue
