module HeX.BreakList.Elem where

import qualified Data.Adjacent                 as A
import           Data.Maybe

import qualified HeX.Box                       as B
import           HeX.Box                        ( Dimensioned
                                                , naturalWidth
                                                )
import           HeX.BreakList.Glue
import           HeX.BreakList.Judge

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
  = HVListElem BreakableVListElem
  | ListCharacter B.Character

data BreakableVListElem
  = ListBox B.Box
  | ListRule B.Rule
  | ListGlue Glue
  | ListKern B.Kern
  | ListPenalty Penalty
  | ListFontDefinition B.FontDefinition
  | ListFontSelection B.FontSelection

-- Show.

instance Show BreakableHListElem where
  show (HVListElem e) = show e
  show (ListCharacter e) = show e

  showList a = (show (foldr append [] a) ++)
    where
      append (ListCharacter B.Character {B.char = c}) [] = [Sentence [c]]
      append x [] = [NonSentence x]
      append y r@(x:xs)
        | ListCharacter B.Character {B.char = c} <- y
        , (Sentence cs) <- x = Sentence (c : cs) : xs
        | ListCharacter B.Character {B.char = c} <- y = Sentence [c] : r
        | otherwise = NonSentence y : r

-- Just for the purposes of showing the list more compactly.
data CondensedHListElem
  = Sentence String
  | NonSentence BreakableHListElem

instance Show CondensedHListElem where
  show (Sentence s) = s
  show (NonSentence x) = show x

instance Show BreakableVListElem where
  show (ListBox e) = show e
  show (ListRule e) = show e
  show (ListGlue e) = show e
  show (ListKern e) = show e
  show (ListPenalty e) = show e
  show (ListFontDefinition e) = show e
  show (ListFontSelection e) = show e

-- Dimensioned.

instance Dimensioned BreakableHListElem where
  naturalWidth (HVListElem (ListBox b)) = B.naturalWidth b
  naturalWidth (HVListElem (ListRule r)) = B.naturalWidth r
  naturalWidth (HVListElem (ListGlue g)) = dimen g
  naturalWidth (HVListElem (ListKern k)) = B.kernDimen k
  naturalWidth (HVListElem (ListPenalty _)) = 0
  naturalWidth (HVListElem (ListFontDefinition _)) = 0
  naturalWidth (HVListElem (ListFontSelection _)) = 0
  naturalWidth (ListCharacter c) = naturalWidth c

  naturalHeight (HVListElem (ListBox b)) = B.naturalHeight b
  naturalHeight (HVListElem (ListRule r)) = B.naturalHeight r
  naturalHeight (HVListElem (ListGlue _)) = 0
  naturalHeight (HVListElem (ListKern _)) = 0
  naturalHeight (HVListElem (ListPenalty _)) = 0
  naturalHeight (HVListElem (ListFontDefinition _)) = 0
  naturalHeight (HVListElem (ListFontSelection _)) = 0
  naturalHeight (ListCharacter c) = B.naturalHeight c

  naturalDepth (HVListElem (ListBox b)) = B.naturalDepth b
  naturalDepth (HVListElem (ListRule r)) = B.naturalDepth r
  naturalDepth (HVListElem (ListGlue _)) = 0
  naturalDepth (HVListElem (ListKern _)) = 0
  naturalDepth (HVListElem (ListPenalty _)) = 0
  naturalDepth (HVListElem (ListFontDefinition _)) = 0
  naturalDepth (HVListElem (ListFontSelection _)) = 0
  naturalDepth (ListCharacter c) = B.naturalDepth c

instance Dimensioned BreakableVListElem where
  naturalWidth (ListBox b) = B.naturalWidth b
  naturalWidth (ListRule r) = B.naturalWidth r
  naturalWidth (ListGlue _) = 0
  naturalWidth (ListKern _) = 0
  naturalWidth (ListPenalty _) = 0
  naturalWidth (ListFontDefinition _) = 0
  naturalWidth (ListFontSelection _) = 0

  naturalHeight (ListBox b) = B.naturalHeight b
  naturalHeight (ListRule r) = B.naturalHeight r
  naturalHeight (ListGlue g) = dimen g
  naturalHeight (ListKern k) = B.kernDimen k
  naturalHeight (ListPenalty _) = 0
  naturalHeight (ListFontDefinition _) = 0
  naturalHeight (ListFontSelection _) = 0

  naturalDepth (ListBox b) = B.naturalDepth b
  naturalDepth (ListRule r) = B.naturalDepth r
  naturalDepth (ListGlue _) = 0
  naturalDepth (ListKern _) = 0
  naturalDepth (ListPenalty _) = 0
  naturalDepth (ListFontDefinition _) = 0
  naturalDepth (ListFontSelection _) = 0

-- BreakableListElem.

instance BreakableListElem BreakableHListElem where
  toGlue (HVListElem e) = toGlue e
  toGlue (ListCharacter _) = Nothing

  isDiscardable (HVListElem e) = isDiscardable e
  isDiscardable (ListCharacter _) = False

  isBox (HVListElem e) = isBox e
  isBox (ListCharacter _) = True

  -- TODO: Add math formula conditions.
  -- TODO: Discretionary break and Math-off.

  toBreakItem (A.Adjacency a) = case a of
    (Just x, HVListElem (ListGlue g), _) ->
      if isDiscardable x then Nothing else Just $ GlueBreak g
    (_, HVListElem (ListKern k), Just (HVListElem (ListGlue _))) ->
      Just $ KernBreak k
    (_, HVListElem (ListPenalty p), _) -> Just $ PenaltyBreak p
    _ -> Nothing

  naturalLength = B.naturalWidth

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

  toBreakItem (A.Adjacency a) = case a of
    (Just x, ListGlue g, _) ->
      if isDiscardable x then Nothing else Just $ GlueBreak g
    (_, ListKern k, Just ListGlue {}) ->
      Just $ KernBreak k
    (_, ListPenalty p, _) -> Just $ PenaltyBreak p
    _ -> Nothing

  naturalLength = B.naturalHeight

listGlueStatus :: BreakableListElem a => Int -> [a] -> GlueStatus
listGlueStatus desiredLength cs =
  let totGlue = mconcat $ mapMaybe toGlue cs
  in glueStatus (naturalListLength cs - desiredLength) totGlue
