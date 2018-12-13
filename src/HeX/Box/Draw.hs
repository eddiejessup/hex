{-# LANGUAGE DuplicateRecordFields #-}

module HeX.Box.Draw where

import qualified DVI.Instruction               as DI
import qualified DVI.Document                  as D

import           HeX.Dimensioned              ( Dimensioned(..) )
import           HeX.Box

boxContentsToDVI :: BoxContents -> [D.Instruction]
boxContentsToDVI (HBoxContents cs) = concatMap hBoxElemToDVI cs
boxContentsToDVI (VBoxContents cs) = concatMap vBoxElemToDVI cs

boxToDVI :: Box -> [D.Instruction]
boxToDVI Box {contents = cs} = boxContentsToDVI cs

ruleToDVI :: Rule -> D.Instruction
ruleToDVI Rule {width = w, height = h, depth = d}
  = D.Rule {height = h + d, width = w, move = DI.Set}

-- TODO: Improve mapping of name and path.
fontDefToDVI :: FontDefinition -> D.Instruction
fontDefToDVI FontDefinition { fontNr = fNr
                            , fontPath = _
                            , fontName = name
                            , fontInfo = info
                            , scaleFactorRatio = scale
                            } =
  D.DefineFont {fontInfo = info, fontPath = name, fontNr = fNr, scaleFactorRatio = scale}

fontSelToDVI :: FontSelection -> D.Instruction
fontSelToDVI FontSelection {fontNr = fNr} = D.SelectFont fNr

charToDVI :: Character -> D.Instruction
charToDVI Character {char = c} = D.Character (fromEnum c) DI.Set

hBoxElemToDVI :: HBoxElem -> [D.Instruction]
hBoxElemToDVI (HVBoxElem (BoxChild b)) =
  [D.PushStack] ++ boxToDVI b ++ [D.PopStack, D.MoveRight $ naturalWidth b]
hBoxElemToDVI (HVBoxElem (BoxGlue g)) = [D.MoveRight $ glueDimen g]
hBoxElemToDVI (HVBoxElem (BoxKern k)) = [D.MoveRight $ kernDimen k]
hBoxElemToDVI (HVBoxElem (BoxRule r)) =
  [D.PushStack] ++ [ruleToDVI r] ++ [D.PopStack, D.MoveRight $ naturalWidth r]
hBoxElemToDVI (HVBoxElem e@(BoxFontDefinition _)) = vBoxElemToDVI e
hBoxElemToDVI (HVBoxElem e@(BoxFontSelection _)) = vBoxElemToDVI e
hBoxElemToDVI (BoxCharacter e) = [charToDVI e]

vBoxElemToDVI :: VBoxElem -> [D.Instruction]
vBoxElemToDVI (BoxChild b) =
  [D.PushStack] ++ boxToDVI b ++ [D.PopStack, D.MoveDown $ naturalHeight b + naturalDepth b]
  -- TODO: Rule.
vBoxElemToDVI (BoxGlue g) = [D.MoveDown $ glueDimen g]
vBoxElemToDVI (BoxKern k) = [D.MoveDown $ kernDimen k]
vBoxElemToDVI (BoxRule r) =
  [D.PushStack] ++ [ruleToDVI r] ++ [D.PopStack, D.MoveDown $ naturalHeight r + naturalDepth r]
vBoxElemToDVI (BoxFontDefinition e) = [fontDefToDVI e]
vBoxElemToDVI (BoxFontSelection e) = [fontSelToDVI e]

pageToDVI :: Page -> [D.Instruction]
pageToDVI (Page vs) = D.BeginNewPage : concatMap vBoxElemToDVI vs

pagesToDVI :: [Page] -> [D.Instruction]
pagesToDVI = concatMap pageToDVI
