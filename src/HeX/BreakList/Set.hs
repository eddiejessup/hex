module HeX.BreakList.Set where

import           Data.Maybe                     ( mapMaybe )

import           HeX.Box
import           HeX.BreakList.Elem
import           HeX.BreakList.Judge

setVListElem :: GlueStatus -> BreakableVListElem -> Maybe VBoxElem
setVListElem st e = case e of
    ListGlue g       -> Just $ BoxGlue $ setGlue st g
    VListBaseElem be -> Just $ VBoxBaseElem be
    ListPenalty _    -> Nothing

setHListElem :: GlueStatus -> BreakableHListElem -> Maybe HBoxElem
setHListElem st e = case e of
    HVListElem ve     -> HVBoxElem <$> setVListElem st ve
    HListHBaseElem be -> Just $ HBoxHBaseElem be

setHList :: GlueStatus -> [BreakableHListElem] -> [HBoxElem]
setHList st = mapMaybe (setHListElem st)

setVList :: GlueStatus -> [BreakableVListElem] -> [VBoxElem]
setVList st = mapMaybe (setVListElem st)
