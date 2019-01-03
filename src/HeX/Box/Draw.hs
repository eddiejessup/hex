{-# LANGUAGE DuplicateRecordFields #-}

module HeX.Box.Draw where

import           Data.Concept

import qualified DVI.Document                  as D

import           HeX.Box

boxToDVI :: Box -> [D.Instruction]
boxToDVI Box { contents = cs } = case cs of
    HBoxContents elems -> concatMap hBoxElemToDVI elems
    VBoxContents elems -> concatMap vBoxElemToDVI elems

hBoxElemToDVI :: HBoxElem -> [D.Instruction]
hBoxElemToDVI el = case el of
    HVBoxElem (BoxChild b) ->
        [D.PushStack]
        ++ boxToDVI b
        ++ [D.PopStack, D.Move Horizontal $ naturalWidth b]
    HVBoxElem (BoxRule b) ->
        [D.PushStack]
        ++ [D.AddRule b]
        ++ [D.PopStack, D.Move Horizontal $ naturalWidth b]
    HVBoxElem (BoxGlue g)             -> [D.Move Horizontal $ glueDimen g]
    HVBoxElem (BoxKern k)             -> [D.Move Horizontal $ kernDimen k]
    HVBoxElem e@(BoxFontDefinition _) -> vBoxElemToDVI e
    HVBoxElem e@(BoxFontSelection _)  -> vBoxElemToDVI e
    BoxCharacter e                    -> [D.AddCharacter e]

vBoxElemToDVI :: VBoxElem -> [D.Instruction]
vBoxElemToDVI el = case el of
    BoxChild b ->
        [D.PushStack]
        ++ boxToDVI b
        ++ [D.PopStack, D.Move Vertical $ naturalHeight b + naturalDepth b]
    BoxRule b ->
        [D.PushStack]
        ++ [D.AddRule b]
        ++ [D.PopStack, D.Move Vertical $ naturalHeight b + naturalDepth b]
    BoxGlue g           -> [D.Move Vertical $ glueDimen g]
    BoxKern k           -> [D.Move Vertical $ kernDimen k]
    BoxFontDefinition e -> [D.DefineFont e]
    BoxFontSelection e  -> [D.SelectFont e]

pageToDVI :: Page -> [D.Instruction]
pageToDVI (Page vs) = D.BeginNewPage : concatMap vBoxElemToDVI vs

pagesToDVI :: [Page] -> [D.Instruction]
pagesToDVI = concatMap pageToDVI
