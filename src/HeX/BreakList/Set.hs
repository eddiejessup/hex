{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HeX.BreakList.Set where

import qualified HeX.Box as B

import HeX.BreakList.Elem
import HeX.BreakList.Judge
import HeX.BreakList.Glue

class Settable a b where
  set :: GlueStatus -> a -> [b]

instance Settable BreakableHListElem B.HBoxElem where
  set ls (HGlue g) = B.HGlue <$> set ls g
  set _ (HPenalty _) = []
  set _ (HListBox b) = [B.HChild b]
  set _ (HRule a) = [B.HRule a]
  set _ (HKern a) = [B.HKern a]
  set _ (HFontDefinition a) = [B.HFontDefinition a]
  set _ (HFontSelection a) = [B.HFontSelection a]
  set _ (HCharacter a) = [B.HCharacter a]

instance Settable BreakableVListElem B.VBoxElem where
  set ls (VGlue g) = B.VGlue <$> set ls g
  set _ (VPenalty _) = []
  set _ (VListBox b) = [B.VChild b]
  set _ (VRule a) = [B.VRule a]
  set _ (VKern a) = [B.VKern a]
  set _ (VFontDefinition a) = [B.VFontDefinition a]
  set _ (VFontSelection a) = [B.VFontSelection a]

-- We can set a list of settable values by concatenating the result of setting
-- each value.
instance (Settable a b) => Settable [a] b where
  set _status = concatMap (set _status)

instance Settable Glue B.SetGlue where
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
