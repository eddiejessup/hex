{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module HeX.BreakList.Set where

import qualified HeX.Box                       as B

import           HeX.BreakList.Elem
import           HeX.BreakList.Glue
import           HeX.BreakList.Judge

class Settable a where
  type Result a :: *
  set :: GlueStatus -> a -> [Result a]

instance Settable BreakableHListElem where
  type Result BreakableHListElem = B.HBoxElem
  set ls (HVListElem    (ListGlue           g)) = B.HGlue <$> set ls g
  set _  (HVListElem    (ListPenalty        _)) = []
  set _  (HVListElem    (ListBox            b)) = [B.HChild b]
  set _  (HVListElem    (ListRule           a)) = [B.HRule a]
  set _  (HVListElem    (ListKern           a)) = [B.HKern a]
  set _  (HVListElem    (ListFontDefinition a)) = [B.HFontDefinition a]
  set _  (HVListElem    (ListFontSelection  a)) = [B.HFontSelection a]
  set _  (ListCharacter a                     ) = [B.HCharacter a]

instance Settable BreakableVListElem where
  type Result BreakableVListElem = B.VBoxElem
  set ls (ListGlue           g) = B.VGlue <$> set ls g
  set _  (ListPenalty        _) = []
  set _  (ListBox            b) = [B.VChild b]
  set _  (ListRule           a) = [B.VRule a]
  set _  (ListKern           a) = [B.VKern a]
  set _  (ListFontDefinition a) = [B.VFontDefinition a]
  set _  (ListFontSelection  a) = [B.VFontSelection a]

-- We can set a list of settable values by concatenating the result of setting
-- each value.
instance Settable a => Settable [a] where
  type Result [a] = Result a
  set = concatMap . set

instance Settable Glue where
  type Result Glue = B.SetGlue
  set ls g@Glue {dimen = d} = [B.SetGlue $ d + glueDiff ls g]

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
