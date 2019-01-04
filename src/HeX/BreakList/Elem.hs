module HeX.BreakList.Elem where

import           Data.Adjacent
import           Data.Concept

import qualified HeX.Box                       as B
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

    toBreakItem adj = case adj of
        Adj (Just x) (ListGlue g) _ | not (isDiscardable x) ->
            Just $ GlueBreak g
        Adj _ (ListKern k) (Just (ListGlue _)) ->
            Just $ KernBreak k
        Adj _ (ListPenalty p) _ ->
            Just $ PenaltyBreak p
        _ ->
            Nothing

    naturalLength = naturalHeight

instance Dimensioned BreakableVListElem where
    naturalWidth e = case e of
        (ListBox b)            -> naturalWidth b
        (ListRule r)           -> naturalWidth r
        (ListGlue _)           -> 0
        (ListKern _)           -> 0
        (ListPenalty _)        -> 0
        (ListFontDefinition _) -> 0
        (ListFontSelection _)  -> 0

    naturalHeight e = case e of
        (ListBox b)            -> naturalHeight b
        (ListRule r)           -> naturalHeight r
        (ListGlue g)           -> dimen g
        (ListKern k)           -> B.kernDimen k
        (ListPenalty _)        -> 0
        (ListFontDefinition _) -> 0
        (ListFontSelection _)  -> 0

    naturalDepth e = case e of
        (ListBox b)            -> naturalDepth b
        (ListRule r)           -> naturalDepth r
        (ListGlue _)           -> 0
        (ListKern _)           -> 0
        (ListPenalty _)        -> 0
        (ListFontDefinition _) -> 0
        (ListFontSelection _)  -> 0

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
    toBreakItem adj = case adj of
        Adj (Just x) (HVListElem (ListGlue g)) _ | not (isDiscardable x) ->
            Just $ GlueBreak g
        Adj _ (HVListElem (ListKern k)) (Just (HVListElem (ListGlue _))) ->
            Just $ KernBreak k
        Adj _ (HVListElem (ListPenalty p)) _ ->
            Just $ PenaltyBreak p
        _ ->
            Nothing

    naturalLength = naturalWidth

instance Dimensioned BreakableHListElem where
    naturalWidth e = case e of
        ListCharacter c                   -> naturalWidth c
        HVListElem (ListBox b)            -> naturalWidth b
        HVListElem (ListRule r)           -> naturalWidth r
        HVListElem (ListGlue g)           -> dimen g
        HVListElem (ListKern k)           -> B.kernDimen k
        HVListElem (ListPenalty _)        -> 0
        HVListElem (ListFontDefinition _) -> 0
        HVListElem (ListFontSelection _)  -> 0

    naturalHeight e = case e of
        ListCharacter c                   -> naturalHeight c
        HVListElem (ListBox b)            -> naturalHeight b
        HVListElem (ListRule r)           -> naturalHeight r
        HVListElem (ListGlue _)           -> 0
        HVListElem (ListKern _)           -> 0
        HVListElem (ListPenalty _)        -> 0
        HVListElem (ListFontDefinition _) -> 0
        HVListElem (ListFontSelection _)  -> 0

    naturalDepth e = case e of
        HVListElem (ListBox b)            -> naturalDepth b
        HVListElem (ListRule r)           -> naturalDepth r
        HVListElem (ListGlue _)           -> 0
        HVListElem (ListKern _)           -> 0
        HVListElem (ListPenalty _)        -> 0
        HVListElem (ListFontDefinition _) -> 0
        HVListElem (ListFontSelection _)  -> 0
        ListCharacter c                   -> naturalDepth c

-- Display.

-- Just used to show an HList more compactly.
data CondensedHListElem
    = Sentence String
    | NonSentence BreakableHListElem
    deriving (Show)

condenseHList :: [BreakableHListElem] -> [CondensedHListElem]
condenseHList =
    foldr append []
  where
    append (ListCharacter B.Character {B.char = c}) [] = [Sentence [c]]
    append x [] = [NonSentence x]
    append y r@(x:xs)
        | ListCharacter B.Character {B.char = c} <- y, (Sentence cs) <- x =
            Sentence (c : cs) : xs
        | ListCharacter B.Character {B.char = c} <- y =
            Sentence [c] : r
        | otherwise =
            NonSentence y : r
