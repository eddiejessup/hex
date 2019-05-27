module HeX.Box.Draw where

import           HeXlude

import qualified DVI.Document as D

import           HeX.Box.Elem

ruleToDVI :: Axis -> Rule -> ForwardDirected [] D.Instruction
ruleToDVI ax b =
    FDirected $
           [ D.PushStack ]
        <> [ D.AddRule b ]
        <> [ D.PopStack, D.Move ax $ axisNaturalSpan ax b ]

boxToDVI :: Axis -> Box -> ForwardDirected [] D.Instruction
boxToDVI ax b =
       FDirected [ D.PushStack ]
    <> contentDVI b
    <> FDirected [ D.PopStack, D.Move ax $ axisNaturalSpan ax b ]
  where
    contentDVI :: Box -> ForwardDirected [] D.Instruction
    contentDVI Box{contents} = case contents of
        HBoxContents elems   -> mconcatMap hBoxElemToDVI elems
        VBoxContents elems _ -> mconcatMap vBoxElemToDVI elems

axisVBoxElemToDVI :: Axis -> VBoxElem -> ForwardDirected [] D.Instruction
axisVBoxElemToDVI ax el = case el of
    VBoxBaseElem (ElemBox b) -> boxToDVI ax b
    VBoxBaseElem (ElemRule b) -> ruleToDVI ax b
    VBoxBaseElem (ElemFontDefinition e) -> pure (D.DefineFont e)
    VBoxBaseElem (ElemFontSelection e) -> pure (D.SelectFont e)
    VBoxBaseElem (ElemKern k) -> pure (D.Move ax $ kernDimen k)
    BoxGlue g -> pure (D.Move ax $ glueDimen g)

vBoxElemToDVI :: VBoxElem -> ForwardDirected [] D.Instruction
vBoxElemToDVI = axisVBoxElemToDVI Vertical

hBoxElemToDVI :: HBoxElem -> ForwardDirected [] D.Instruction
hBoxElemToDVI (HBoxHBaseElem (ElemCharacter e)) = FDirected [ D.AddCharacter e ]
hBoxElemToDVI (HVBoxElem e) = axisVBoxElemToDVI Horizontal e

pageToDVI :: Page -> ForwardDirected [] D.Instruction
pageToDVI (Page vs) = FDirected [D.BeginNewPage] <> mconcatMap vBoxElemToDVI vs

pagesToDVI :: (Foldable t, Functor t) => ForwardDirected t Page -> ForwardDirected [] D.Instruction
pagesToDVI = mconcatMap pageToDVI
