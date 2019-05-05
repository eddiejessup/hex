module HeX.Box.Draw where

import           HeXlude

import qualified DVI.Document as D

import           HeX.Box.Elem

ruleToDVI :: Axis -> Rule -> [D.Instruction]
ruleToDVI ax b = [ D.PushStack ]
    <> [ D.AddRule b ]
    <> [ D.PopStack, D.Move ax $ axisNaturalSpan ax b ]

boxToDVI :: Axis -> Box -> [D.Instruction]
boxToDVI ax b = [ D.PushStack ]
    <> contentDVI b
    <> [ D.PopStack, D.Move ax $ axisNaturalSpan ax b ]
  where
    contentDVI Box{contents} = case contents of
        HBoxContents elems   -> concatMap hBoxElemToDVI elems
        VBoxContents elems _ -> concatMap vBoxElemToDVI elems

axisVBoxElemToDVI :: Axis -> VBoxElem -> [D.Instruction]
axisVBoxElemToDVI ax el = case el of
    VBoxBaseElem (ElemBox b) -> boxToDVI ax b
    VBoxBaseElem (ElemRule b) -> ruleToDVI ax b
    VBoxBaseElem (ElemFontDefinition e) -> [ D.DefineFont e ]
    VBoxBaseElem (ElemFontSelection e) -> [ D.SelectFont e ]
    VBoxBaseElem (ElemKern k) -> [ D.Move ax $ kernDimen k ]
    BoxGlue g -> [ D.Move ax $ glueDimen g ]

vBoxElemToDVI :: VBoxElem -> [D.Instruction]
vBoxElemToDVI = axisVBoxElemToDVI Vertical

hBoxElemToDVI :: HBoxElem -> [D.Instruction]
hBoxElemToDVI (HBoxHBaseElem (ElemCharacter e)) = [ D.AddCharacter e ]
hBoxElemToDVI (HVBoxElem e) = axisVBoxElemToDVI Horizontal e

pageToDVI :: Page -> [D.Instruction]
pageToDVI (Page vs) = D.BeginNewPage : concatMap vBoxElemToDVI vs

pagesToDVI :: Foldable t => t Page -> [D.Instruction]
pagesToDVI = concatMap pageToDVI
