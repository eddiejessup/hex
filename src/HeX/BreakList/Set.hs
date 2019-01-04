{-# LANGUAGE DuplicateRecordFields #-}

module HeX.BreakList.Set where

import           Data.Maybe                     ( mapMaybe )

import           HeX.Box
import           HeX.BreakList.Elem
import           HeX.BreakList.Glue
import           HeX.BreakList.Judge

setHListElem :: GlueStatus -> BreakableHListElem -> Maybe HBoxElem
setHListElem st e = case e of
    HVListElem ve     -> HVBoxElem <$> setVListElem st ve
    HListHBaseElem be -> Just $ HBoxHBaseElem be

setVListElem :: GlueStatus -> BreakableVListElem -> Maybe VBoxElem
setVListElem st e = case e of
    ListGlue g -> Just $ BoxGlue $ setGlue st g
    ListPenalty _ -> Nothing
    VListBaseElem be -> Just $ VBoxBaseElem be

-- Set a list of list elements by concatenating the result of setting each
-- value.
setHList :: GlueStatus -> [BreakableHListElem] -> [HBoxElem]
setHList st = mapMaybe (setHListElem st)
setVList :: GlueStatus -> [BreakableVListElem] -> [VBoxElem]
setVList st = mapMaybe (setVListElem st)

setGlue :: GlueStatus -> Glue -> SetGlue
setGlue st g@Glue {dimen = d} = SetGlue $ d + glueDiff st g

-- Suppose the line order is i, and the glue has natural width u, and
-- flexibility f_j, corresponding to its amount and order of stretch or shrink
-- as appropriate.
-- The glue's width is u, plus: rf if j = i; otherwise 0.
glueDiff :: GlueStatus -> Glue -> Int
glueDiff NaturallyGood _ = 0
-- Note: I made this logic up. Take a 'do your best' approach.
glueDiff (NaturallyBad Full Unfixable) Glue {shrink = GlueFlex {factor = f}} =
    -(round f)
glueDiff (NaturallyBad Bare Unfixable) Glue {stretch = GlueFlex {factor = f}} =
    round f
glueDiff (NaturallyBad a Fixable {ratio = r, order = setOrder}) Glue { shrink = shr
                                                                     , stretch = str
                                                                     }
    | Full <- a = -(scaleFactor shr)
    | Bare <- a = scaleFactor str
  where
    scaleFactor GlueFlex {factor = f, order = glueOrder} =
      if setOrder == glueOrder
        then round (r * f)
        else 0
