module Hex.BreakList.Set where

import           Hexlude

import           Hex.Box
import           Hex.BreakList.Elem
import           Hex.BreakList.Judge
import           Hex.Quantity

setVListElem :: GlueStatus -> VListElem -> Maybe VBoxElem
setVListElem st = \case
    ListGlue g       -> Just $ BoxGlue $ setGlue st g
    VListBaseElem be -> Just $ VBoxBaseElem be
    ListPenalty _    -> Nothing

setHListElem :: GlueStatus -> HListElem -> Maybe HBoxElem
setHListElem st = \case
    HVListElem ve     -> HVBoxElem <$> setVListElem st ve
    HListHBaseElem be -> Just $ HBoxHBaseElem be

rawSetHList :: GlueStatus -> HList -> HBox
rawSetHList status (HList elemList) = HBox (seqMapMaybe (setHListElem status) elemList)

setHList :: HList -> LazyTargetLength -> Box HBox
setHList hList lazyStatus =
    let
        TargetLength status width = case lazyStatus of
            ComputedTargetLength tgtLength ->
                tgtLength
            UncomputedTargetLength desiredLength ->
                listGlueStatusAbstractTarget desiredLength hList
    in Box
        { contents = rawSetHList status hList
        , boxWidth = width
        , boxHeight = naturalHeight hList
        , boxDepth = naturalDepth hList
        }

rawSetVList :: GlueStatus -> VList -> VBox
rawSetVList status (VList elemList) = VBox (seqMapMaybe (setVListElem status) elemList)

setVList :: VList -> DesiredLength -> VBoxAlignType -> Box VBox
setVList vList desiredLength alignType =
    let
        (TargetLength status height, depth) = case alignType of
            DefaultAlign ->
                (listGlueStatusAbstractTarget desiredLength vList, naturalDepth vList)
            TopAlign ->
                panic "vtop not implemented"
    in Box
        { contents = rawSetVList status vList
        , boxWidth = naturalWidth vList
        , boxHeight = height
        , boxDepth = depth }
