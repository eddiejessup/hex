{-# LANGUAGE DuplicateRecordFields #-}

module HeX.Box.Draw where

import           Data.Concept

import qualified DVI.Document                  as D

import           HeX.Dimensioned              ( Dimensioned(..) )
import           HeX.Box

boxContentsToDVI :: BoxContents -> [D.Instruction]
boxContentsToDVI (HBoxContents cs) = concatMap hBoxElemToDVI cs
boxContentsToDVI (VBoxContents cs) = concatMap vBoxElemToDVI cs

boxToDVI :: Box -> [D.Instruction]
boxToDVI Box {contents = cs} = boxContentsToDVI cs

hBoxElemToDVI :: HBoxElem -> [D.Instruction]
hBoxElemToDVI (HVBoxElem (BoxChild b)) =
  [D.PushStack] ++ boxToDVI b ++ [D.PopStack, D.Move Horizontal $ naturalWidth b]
hBoxElemToDVI (HVBoxElem (BoxGlue g)) = [D.Move Horizontal $ glueDimen g]
hBoxElemToDVI (HVBoxElem (BoxKern k)) = [D.Move Horizontal $ kernDimen k]
hBoxElemToDVI (HVBoxElem (BoxRule r)) =
  [D.PushStack] ++ [D.AddRule r] ++ [D.PopStack, D.Move Horizontal $ naturalWidth r]
hBoxElemToDVI (HVBoxElem e@(BoxFontDefinition _)) = vBoxElemToDVI e
hBoxElemToDVI (HVBoxElem e@(BoxFontSelection _)) = vBoxElemToDVI e
hBoxElemToDVI (BoxCharacter e) = [D.AddCharacter e]

vBoxElemToDVI :: VBoxElem -> [D.Instruction]
vBoxElemToDVI (BoxChild b) =
  [D.PushStack] ++ boxToDVI b ++ [D.PopStack, D.Move Vertical $ naturalHeight b + naturalDepth b]
  -- TODO: Rule.
vBoxElemToDVI (BoxGlue g) = [D.Move Vertical $ glueDimen g]
vBoxElemToDVI (BoxKern k) = [D.Move Vertical $ kernDimen k]
vBoxElemToDVI (BoxRule r) =
  [D.PushStack] ++ [D.AddRule r] ++ [D.PopStack, D.Move Vertical $ naturalHeight r + naturalDepth r]
vBoxElemToDVI (BoxFontDefinition e) = [D.DefineFont e]
vBoxElemToDVI (BoxFontSelection e) = [D.SelectFont e]

pageToDVI :: Page -> [D.Instruction]
pageToDVI (Page vs) = D.BeginNewPage : concatMap vBoxElemToDVI vs

pagesToDVI :: [Page] -> [D.Instruction]
pagesToDVI = concatMap pageToDVI
