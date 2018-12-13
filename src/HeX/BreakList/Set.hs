{-# LANGUAGE DuplicateRecordFields #-}

module HeX.BreakList.Set where

import Data.Maybe (mapMaybe)

import qualified HeX.Box                       as B
import           HeX.BreakList.Elem
import           HeX.BreakList.Glue
import           HeX.BreakList.Judge

setHListElem :: GlueStatus -> BreakableHListElem -> Maybe B.HBoxElem
setHListElem st (HVListElem    e) = B.HVBoxElem <$> setVListElem st e
setHListElem _  (ListCharacter a) = Just $ B.BoxCharacter a

setVListElem :: GlueStatus -> BreakableVListElem -> Maybe B.VBoxElem
setVListElem st (ListGlue           g) = Just $ B.BoxGlue $ setGlue st g
setVListElem _  (ListPenalty        _) = Nothing
setVListElem _  (ListBox            b) = Just $ B.BoxChild b
setVListElem _  (ListRule           a) = Just $ B.BoxRule a
setVListElem _  (ListKern           a) = Just $ B.BoxKern a
setVListElem _  (ListFontDefinition a) = Just $ B.BoxFontDefinition a
setVListElem _  (ListFontSelection  a) = Just $ B.BoxFontSelection a

-- Set a list of list elements by concatenating the result of setting each
-- value.
setHList :: GlueStatus -> [BreakableHListElem] -> [B.HBoxElem]
setHList st = mapMaybe (setHListElem st)
setVList :: GlueStatus -> [BreakableVListElem] -> [B.VBoxElem]
setVList st = mapMaybe (setVListElem st)

setGlue :: GlueStatus -> Glue -> B.SetGlue
setGlue st g@Glue {dimen = d} = B.SetGlue $ d + glueDiff st g

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
