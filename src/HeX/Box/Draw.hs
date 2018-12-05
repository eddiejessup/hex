{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module HeX.Box.Draw where

import qualified DVI.Instruction               as DI
import qualified DVI.Document                  as D

import           HeX.Box

class DVIAble a where
  toDVI :: a -> [D.Instruction]

instance DVIAble Box where
  toDVI Box {contents = HBoxContents cs} = concatMap toDVI cs
  toDVI Box {contents = VBoxContents cs} = concatMap toDVI cs

instance DVIAble Rule where
  toDVI Rule {width = w, height = h, depth = d} =
    [D.Rule {height = h + d, width = w, move = DI.Set}]

instance DVIAble SetGlue where
  toDVI SetGlue {} = []

instance DVIAble Kern where
  toDVI Kern {} = []

instance DVIAble FontDefinition where
  -- TODO: Improve mapping of name and path.
  toDVI FontDefinition { fontNr = fNr
                       , fontPath = path
                       , fontName = name
                       , fontInfo = info
                       , scaleFactorRatio = scale
                       } =
    [ D.DefineFont
      {fontInfo = info, fontPath = name, fontNr = fNr, scaleFactorRatio = scale}
    ]

instance DVIAble FontSelection where
  toDVI FontSelection {fontNr = fNr} = [D.SelectFont fNr]

instance DVIAble Character where
  toDVI Character {char = c} = [D.Character (fromEnum c) DI.Set]

instance DVIAble HBoxElem where
  toDVI (HChild b) =
    [D.PushStack] ++
    toDVI b ++ [D.PopStack, D.MoveRight $ naturalWidth b]
    -- TODO: Rule.
  toDVI (HGlue g) = [D.MoveRight $ glueDimen g]
  toDVI (HKern k) = [D.MoveRight $ kernDimen k]
  toDVI (HRule r) =
    [D.PushStack] ++
    toDVI r ++ [D.PopStack, D.MoveRight $ naturalWidth r]
  toDVI (HFontDefinition e) = toDVI e
  toDVI (HFontSelection e) = toDVI e
  toDVI (HCharacter e) = toDVI e

instance DVIAble VBoxElem where
  toDVI (VChild b) =
    [D.PushStack] ++
    toDVI b ++ [D.PopStack, D.MoveDown $ naturalHeight b + naturalDepth b]
    -- TODO: Rule.
  toDVI (VGlue g) = [D.MoveDown $ glueDimen g]
  toDVI (VKern k) = [D.MoveDown $ kernDimen k]
  toDVI (VRule r) =
    [D.PushStack] ++
    toDVI r ++ [D.PopStack, D.MoveDown $ naturalHeight r + naturalDepth r]
  toDVI (VFontDefinition e) = toDVI e
  toDVI (VFontSelection e) = toDVI e

instance DVIAble Page where
  toDVI (Page vs) = D.BeginNewPage : concatMap toDVI vs

instance DVIAble [Page] where
  toDVI = concatMap toDVI
