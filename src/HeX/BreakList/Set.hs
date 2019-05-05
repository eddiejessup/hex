module HeX.BreakList.Set where

import           HeXlude

import           HeX.Box
import           HeX.BreakList.Elem
import           HeX.BreakList.Judge

setVListElem :: GlueStatus -> VListElem -> Maybe VBoxElem
setVListElem st e = case e of
    ListGlue g       -> Just $ BoxGlue $ setGlue st g
    VListBaseElem be -> Just $ VBoxBaseElem be
    ListPenalty _    -> Nothing

setHListElem :: GlueStatus -> HListElem -> Maybe HBoxElem
setHListElem st e = case e of
    HVListElem ve     -> HVBoxElem <$> setVListElem st ve
    HListHBaseElem be -> Just $ HBoxHBaseElem be

setHList :: GlueStatus -> HList -> [HBoxElem]
setHList st elemList = toList $ mapMaybe (setHListElem st) elemList

setVList :: GlueStatus -> VList -> [VBoxElem]
setVList st elemList = toList $ mapMaybe (setVListElem st) elemList
