{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module HeX.Box.Draw where

import qualified DVI.Encode as DVIE

import HeX.Box

class DVIAble a where
  toDVI :: a -> [DVIE.Instruction]

instance DVIAble Box where
  toDVI Box{contents=HBoxContents cs} = concatMap toDVI cs
  toDVI Box{contents=VBoxContents cs} = concatMap toDVI cs

-- TODO: Don't know how to handle depth.
instance DVIAble Rule where
  toDVI Rule{width=w, height=h, depth=d} = [DVIE.Rule{height=h + d, width=w, move=True}]

instance DVIAble SetGlue where
  toDVI SetGlue {} = []

instance DVIAble Kern where
  toDVI Kern {} = []

instance DVIAble FontDefinition where
  toDVI FontDefinition { fontNr = fNr
                       , fontPath = path
                       , fontName = name
                       , fontInfo = info
                       , scaleFactorRatio = scale
                       }
   = [ DVIE.DefineFont
        { fontInfo = info
        , fontPath = name
        , fontNr = fNr
        , scaleFactorRatio = scale
        }
      ]

instance DVIAble FontSelection where
  toDVI FontSelection {fontNr = fNr} = [DVIE.SelectFont fNr]

instance DVIAble Character where
  toDVI Character {code = c} = [DVIE.Character {charNr = c, move = True}]

instance DVIAble HBoxElem where
  toDVI (HChild b) =
    [DVIE.PushStack] ++ toDVI b ++ [DVIE.PopStack, DVIE.MoveRight {distance = naturalWidth b}]
    -- TODO: Rule.
  toDVI (HGlue g) = [DVIE.MoveRight {distance = glueDimen g}]
  toDVI (HKern k) = [DVIE.MoveRight {distance = kernDimen k}]
  toDVI (HRule r) =
    [DVIE.PushStack] ++ toDVI r ++ [DVIE.PopStack, DVIE.MoveRight {distance = naturalWidth r}]
  toDVI (HFontDefinition e) = toDVI e
  toDVI (HFontSelection e) = toDVI e
  toDVI (HCharacter e) = toDVI e

instance DVIAble VBoxElem where
  toDVI (VChild b) =
    [DVIE.PushStack] ++ toDVI b ++ [DVIE.PopStack, DVIE.MoveDown {distance = naturalHeight b + naturalDepth b}]
    -- TODO: Rule.
  toDVI (VGlue g) = [DVIE.MoveDown {distance = glueDimen g}]
  toDVI (VKern k) = [DVIE.MoveDown {distance = kernDimen k}]
  toDVI (VRule r) =
    [DVIE.PushStack] ++ toDVI r ++ [DVIE.PopStack, DVIE.MoveDown {distance = naturalHeight r + naturalDepth r}]
  toDVI (VFontDefinition e) = toDVI e
  toDVI (VFontSelection e) = toDVI e

instance DVIAble Page where
  toDVI (Page vs) = DVIE.BeginNewPage:concatMap toDVI vs

instance DVIAble [Page] where
  toDVI = concatMap toDVI
