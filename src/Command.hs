{-# LANGUAGE DuplicateRecordFields #-}

module Command where

import qualified Data.Char as C

import qualified Cat
import qualified Lex

data Axis = Horizontal | Vertical
data HDirection = Left | Right
data VDirection = Up | Down
data Stream = Out | Err

data CharSource = ExplicitChar | CodeChar | TokenChar
data SpaceSource = ExplicitSpace | ControlSpace
data InsertionType = Insertion | Adjustment

-- data Distance = Distance Int
-- data HDisplacement = HDisplacement HDirection Distance | HDefault
-- data VDisplacement = VDisplacement VDirection Distance | VDefault
-- data BoxPlacement = HBox HDisplacement | VBox VDisplacement

data NonMacroAssignment = SelectFont

data Assignment = NonMacroAssign NonMacroAssignment | MacroAssign

data Command
  = Relax
  | Assign Assignment
  -- | LeftBrace
  -- | RightBrace
  -- | BeginGroup
  -- | EndGroup
  -- | ShowToken
  -- | ShowBox
  -- | ShowLists
  -- | ShowInternalQuantity
  -- | ShipOut
  -- | IgnoreSpaces
  -- | SetAfterAssignmentToken
  -- | AddToAfterGroupTokens
  -- | Message Stream
  -- | OpenInput
  -- | CloseInput
  -- | OpenOutput
  -- | CloseOutput
  -- | Write
  -- | AddWhatsit
  -- | AddPenalty
  -- | AddKern
  -- | AddMathKern
  -- | RemoveLastPenalty
  -- | RemoveLastKern
  -- | RemoveLastGlue
  -- | AddMark
  -- | AddInsertion InsertionType
  -- | AddLeaders
  -- | AddBox BoxPlacement
  -- | UnpackBox Axis
  | StartParagraph { indent :: Bool }
  -- | EndParagraph
  -- | AddGlue Axis
  -- | AddRule Axis
  -- | AddAlignedMaterial Axis
  -- | End
  -- | Dump
  | AddSpace SpaceSource
  | AddCharacter { method :: CharSource, code :: Int }
  -- | AddAccent
  -- | AddItalicCorrection
  -- | AddDiscretionaryText
  -- | ShiftMathMode

process :: [Lex.LexToken] -> [Command]
process [] = []
process (Lex.CharCat Cat.CharCat {char = char, cat = cat}:rest)
  | cat `elem` [Cat.Letter, Cat.Other] = AddCharacter{method=ExplicitChar, code=C.ord char} : process rest
  | cat == Cat.Space = AddSpace ExplicitSpace : process rest
  | otherwise = process rest
process (Lex.ControlSequenceCall {name = name}:rest)
  | name == "par" = StartParagraph{indent=True} : process rest
  | name == "font" = (Assign $ NonMacroAssign SelectFont) : process rest
  | otherwise = process rest
