module HeX.BreakList.Elem where

import           Data.Adjacent

import qualified HeX.Box                       as B
import           HeX.Dimensioned                ( Dimensioned(..) )
import           HeX.BreakList.Glue
import           HeX.BreakList.BreakList        ( BreakableListElem(..), Penalty, BreakItem(..) )

-- Vertical list.

data BreakableVListElem
  = ListBox B.Box
  | ListRule B.Rule
  | ListGlue Glue
  | ListKern B.Kern
  | ListPenalty Penalty
  | ListFontDefinition B.FontDefinition
  | ListFontSelection B.FontSelection
  deriving (Show)

instance BreakableListElem BreakableVListElem where
  toGlue (ListGlue g) = Just g
  toGlue _ = Nothing

  isDiscardable (ListGlue _) = True
  isDiscardable (ListKern _) = True
  isDiscardable (ListPenalty _) = True
  isDiscardable _ = False

  isBox (ListBox _) = True
  isBox (ListRule _) = True
  isBox _ = False

  toBreakItem Adj { pre = Just x, val = ListGlue g }
    = if isDiscardable x then Nothing else Just $ GlueBreak g
  toBreakItem Adj { val = ListKern k, post = Just (ListGlue _) }
    = Just $ KernBreak k
  toBreakItem Adj { val = ListPenalty p }
    = Just $ PenaltyBreak p
  toBreakItem _ = Nothing

  naturalLength = naturalHeight

instance Dimensioned BreakableVListElem where
  naturalWidth (ListBox b) = naturalWidth b
  naturalWidth (ListRule r) = naturalWidth r
  naturalWidth (ListGlue _) = 0
  naturalWidth (ListKern _) = 0
  naturalWidth (ListPenalty _) = 0
  naturalWidth (ListFontDefinition _) = 0
  naturalWidth (ListFontSelection _) = 0

  naturalHeight (ListBox b) = naturalHeight b
  naturalHeight (ListRule r) = naturalHeight r
  naturalHeight (ListGlue g) = dimen g
  naturalHeight (ListKern k) = B.kernDimen k
  naturalHeight (ListPenalty _) = 0
  naturalHeight (ListFontDefinition _) = 0
  naturalHeight (ListFontSelection _) = 0

  naturalDepth (ListBox b) = naturalDepth b
  naturalDepth (ListRule r) = naturalDepth r
  naturalDepth (ListGlue _) = 0
  naturalDepth (ListKern _) = 0
  naturalDepth (ListPenalty _) = 0
  naturalDepth (ListFontDefinition _) = 0
  naturalDepth (ListFontSelection _) = 0

-- Horizontal list.

-- TODO: WhatsIt, Leaders, Mark, Insertion
-- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust
data BreakableHListElem
  = HVListElem BreakableVListElem
  | ListCharacter B.Character
  deriving (Show)

instance BreakableListElem BreakableHListElem where
  toGlue (HVListElem    e) = toGlue e
  toGlue (ListCharacter _) = Nothing

  isDiscardable (HVListElem    e) = isDiscardable e
  isDiscardable (ListCharacter _) = False

  isBox (HVListElem    e) = isBox e
  isBox (ListCharacter _) = True

  -- TODO: Add math formula conditions.
  -- TODO: Discretionary break and Math-off.
  toBreakItem Adj { pre = Just x, val = HVListElem (ListGlue g) }
    = if isDiscardable x then Nothing else Just $ GlueBreak g
  toBreakItem Adj { val = HVListElem (ListKern k), post = Just (HVListElem (ListGlue _)) }
    = Just $ KernBreak k
  toBreakItem Adj { val = HVListElem (ListPenalty p) }
    = Just $ PenaltyBreak p
  toBreakItem _ = Nothing

  naturalLength = naturalWidth

instance Dimensioned BreakableHListElem where
  naturalWidth (HVListElem (ListBox b)) = naturalWidth b
  naturalWidth (HVListElem (ListRule r)) = naturalWidth r
  naturalWidth (HVListElem (ListGlue g)) = dimen g
  naturalWidth (HVListElem (ListKern k)) = B.kernDimen k
  naturalWidth (HVListElem (ListPenalty _)) = 0
  naturalWidth (HVListElem (ListFontDefinition _)) = 0
  naturalWidth (HVListElem (ListFontSelection _)) = 0
  naturalWidth (ListCharacter c) = naturalWidth c

  naturalHeight (HVListElem (ListBox b)) = naturalHeight b
  naturalHeight (HVListElem (ListRule r)) = naturalHeight r
  naturalHeight (HVListElem (ListGlue _)) = 0
  naturalHeight (HVListElem (ListKern _)) = 0
  naturalHeight (HVListElem (ListPenalty _)) = 0
  naturalHeight (HVListElem (ListFontDefinition _)) = 0
  naturalHeight (HVListElem (ListFontSelection _)) = 0
  naturalHeight (ListCharacter c) = naturalHeight c

  naturalDepth (HVListElem (ListBox b)) = naturalDepth b
  naturalDepth (HVListElem (ListRule r)) = naturalDepth r
  naturalDepth (HVListElem (ListGlue _)) = 0
  naturalDepth (HVListElem (ListKern _)) = 0
  naturalDepth (HVListElem (ListPenalty _)) = 0
  naturalDepth (HVListElem (ListFontDefinition _)) = 0
  naturalDepth (HVListElem (ListFontSelection _)) = 0
  naturalDepth (ListCharacter c) = naturalDepth c

-- Display.

-- Just used to show an HList more compactly.
data CondensedHListElem
  = Sentence String
  | NonSentence BreakableHListElem
  deriving (Show)

condenseHList :: [BreakableHListElem] -> [CondensedHListElem]
condenseHList = foldr append []
  where
    append (ListCharacter B.Character {B.char = c}) [] = [Sentence [c]]
    append x [] = [NonSentence x]
    append y r@(x:xs)
      | ListCharacter B.Character {B.char = c} <- y
      , (Sentence cs) <- x = Sentence (c : cs) : xs
      | ListCharacter B.Character {B.char = c} <- y = Sentence [c] : r
      | otherwise = NonSentence y : r
