module HeX.Box.Draw where

import           HeXlude

import qualified Data.Sequence as Seq
import qualified DVI.Document  as D

import           HeX.Box.Elem
import           HeX.Quantity

ruleToDVI :: Axis -> Rule -> Seq D.Instruction
ruleToDVI ax b =
    Seq.fromList $
           [ D.PushStack ]
        <> [ D.AddRule b ]
        <> [ D.PopStack, D.Move ax $ axisNaturalSpan ax b ]

boxToDVI :: Axis -> Box BoxContents -> Seq D.Instruction
boxToDVI ax b =
       Seq.fromList [ D.PushStack ]
    <> contentDVI b
    <> Seq.fromList [ D.PopStack, D.Move ax $ axisNaturalSpan ax b ]
  where
    contentDVI :: Box BoxContents -> Seq D.Instruction
    contentDVI Box { contents } = case contents of
        HBoxContents (HBox elems) -> mconcatMap hBoxElemToDVI elems
        VBoxContents (VBox elems) -> mconcatMap vBoxElemToDVI elems

axisVBoxElemToDVI :: Axis -> VBoxElem -> Seq D.Instruction
axisVBoxElemToDVI ax el = case el of
    VBoxBaseElem (ElemBox b)            -> boxToDVI ax b
    VBoxBaseElem (ElemRule b)           -> ruleToDVI ax b
    VBoxBaseElem (ElemFontDefinition e) -> pure (D.DefineFont e)
    VBoxBaseElem (ElemFontSelection e)  -> pure (D.SelectFont e)
    VBoxBaseElem (ElemKern k)           -> pure (D.Move ax $ kernDimen k)
    BoxGlue g                           -> pure (D.Move ax $ glueDimen g)

vBoxElemToDVI :: VBoxElem -> Seq D.Instruction
vBoxElemToDVI = axisVBoxElemToDVI Vertical

hBoxElemToDVI :: HBoxElem -> Seq D.Instruction
hBoxElemToDVI (HBoxHBaseElem (ElemCharacter e)) = Seq.fromList [ D.AddCharacter e ]
hBoxElemToDVI (HVBoxElem e) = axisVBoxElemToDVI Horizontal e

pageToDVI :: Page -> Seq D.Instruction
pageToDVI (Page Box { contents = VBox es }) =
    Seq.fromList [D.BeginNewPage] <> mconcatMap vBoxElemToDVI es

pagesToDVI :: Seq Page -> Seq D.Instruction
pagesToDVI = mconcatMap pageToDVI
