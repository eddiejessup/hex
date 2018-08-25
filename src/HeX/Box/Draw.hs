{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module HeX.Box.Draw where

import qualified DVI.Document as DVID

import HeX.Box

class DVIAble a where
  toDVI :: a -> [DVID.Instruction]

instance DVIAble Box where
  toDVI Box{contents=HBoxContents cs} = concatMap toDVI cs
  toDVI Box{contents=VBoxContents cs} = concatMap toDVI cs

-- TODO: Don't know how to handle depth.
instance DVIAble Rule where
  toDVI Rule{width=w, height=h, depth=d} = [DVID.Rule{height=h + d, width=w, move=True}]

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
   = [ DVID.DefineFont
        { fontInfo = info
        , fontPath = name
        , fontNr = fNr
        , scaleFactorRatio = scale
        }
      ]

instance DVIAble FontSelection where
  toDVI FontSelection {fontNr = fNr} = [DVID.SelectFont fNr]

instance DVIAble Character where
  toDVI Character {code = c} = [DVID.Character {charNr = c, move = True}]

instance DVIAble HBoxElem where
  toDVI (HChild b) =
    [DVID.PushStack] ++ toDVI b ++ [DVID.PopStack, DVID.MoveRight $ naturalWidth b]
    -- TODO: Rule.
  toDVI (HGlue g) = [DVID.MoveRight $ glueDimen g]
  toDVI (HKern k) = [DVID.MoveRight $ kernDimen k]
  toDVI (HRule r) =
    [DVID.PushStack] ++ toDVI r ++ [DVID.PopStack, DVID.MoveRight $ naturalWidth r]
  toDVI (HFontDefinition e) = toDVI e
  toDVI (HFontSelection e) = toDVI e
  toDVI (HCharacter e) = toDVI e

instance DVIAble VBoxElem where
  toDVI (VChild b) =
    [DVID.PushStack] ++ toDVI b ++ [DVID.PopStack, DVID.MoveDown $ naturalHeight b + naturalDepth b]
    -- TODO: Rule.
  toDVI (VGlue g) = [DVID.MoveDown $ glueDimen g]
  toDVI (VKern k) = [DVID.MoveDown $ kernDimen k]
  toDVI (VRule r) =
    [DVID.PushStack] ++ toDVI r ++ [DVID.PopStack, DVID.MoveDown $ naturalHeight r + naturalDepth r]
  toDVI (VFontDefinition e) = toDVI e
  toDVI (VFontSelection e) = toDVI e

instance DVIAble Page where
  toDVI (Page vs) = DVID.BeginNewPage:concatMap toDVI vs

instance DVIAble [Page] where
  toDVI = concatMap toDVI
