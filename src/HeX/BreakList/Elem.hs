{-# LANGUAGE RecordWildCards #-}

module HeX.BreakList.Elem where

import           Data.Adjacent

import           HeX.Type
import           HeX.Box
import           HeX.BreakList.Glue
import           HeX.BreakList.BreakList        ( BreakableListElem(..), Penalty, BreakItem(..) )

-- Vertical list.

data BreakableVListElem
    = VListBaseElem BaseElem
    | ListGlue Glue
    | ListPenalty Penalty
    deriving (Show)

instance BreakableListElem BreakableVListElem where
    toGlue (ListGlue g) = Just g
    toGlue _            = Nothing

    isDiscardable e = case e of
        ListGlue _                 -> True
        ListPenalty _              -> True
        VListBaseElem (ElemKern _) -> True
        _                          -> False

    isBox e = case e of
        VListBaseElem (ElemBox _)  -> True
        VListBaseElem (ElemRule _) -> True
        _                          -> False

    toBreakItem adj = case adj of
        Adj (Just x) (ListGlue g) _ | not (isDiscardable x) ->
            Just $ GlueBreak g
        Adj _ (VListBaseElem (ElemKern k)) (Just (ListGlue _)) ->
            Just $ KernBreak k
        Adj _ (ListPenalty p) _ ->
            Just $ PenaltyBreak p
        _ ->
            Nothing

    naturalSpan = naturalHeight

axisVListElemNaturalLength :: Axis -> TypoDim -> BreakableVListElem -> Int
axisVListElemNaturalLength ax dim e = case e of
    VListBaseElem be -> axisBaseElemNaturalLength ax dim be
    ListGlue g       -> spacerNaturalLength ax dim $ dimen g
    ListPenalty _    -> 0

instance Dimensioned BreakableVListElem where
    naturalLength = axisVListElemNaturalLength Vertical

-- Horizontal list.

-- TODO: WhatsIt, Leaders, Mark, Insertion
-- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust
data BreakableHListElem
    = HVListElem BreakableVListElem
    | HListHBaseElem HBaseElem
    deriving (Show)

instance BreakableListElem BreakableHListElem where
    toGlue (HVListElem    e) = toGlue e
    toGlue _                 = Nothing

    isDiscardable (HVListElem    e) = isDiscardable e
    isDiscardable (HListHBaseElem (ElemCharacter _)) = False

    isBox (HVListElem    e) = isBox e
    isBox (HListHBaseElem (ElemCharacter _)) = True

    -- TODO: Add math formula conditions.
    -- TODO: Discretionary break and Math-off.
    toBreakItem adj = case adj of
        Adj (Just x) (HVListElem (ListGlue g)) _ | not (isDiscardable x) ->
            Just $ GlueBreak g
        Adj _ (HVListElem (VListBaseElem (ElemKern k))) (Just (HVListElem (ListGlue _))) ->
            Just $ KernBreak k
        Adj _ (HVListElem (ListPenalty p)) _ ->
            Just $ PenaltyBreak p
        _ ->
            Nothing

    naturalSpan = naturalWidth

instance Dimensioned BreakableHListElem where
    naturalLength dim (HVListElem e) = axisVListElemNaturalLength Horizontal dim e
    naturalLength dim (HListHBaseElem e) = naturalLength dim e

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
    append (HListHBaseElem (ElemCharacter Character {..})) [] =
        [Sentence [char]]
    append e [] =
        [NonSentence e]
    append e r@(x:xs) = case (x, e) of
        (Sentence cs, HListHBaseElem (ElemCharacter Character {..})) ->
            Sentence (char : cs) : xs
        (_, HListHBaseElem (ElemCharacter Character {..})) ->
            Sentence [char] : r
        _ ->
            NonSentence e : r
