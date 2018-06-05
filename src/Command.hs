{-# LANGUAGE DuplicateRecordFields #-}

module Command where

import Data.List.Split (chop)

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

extractCommand :: [Lex.Token] -> ([Command], [Lex.Token])
extractCommand [] = ([], [])
extractCommand (Lex.CharCat{cat=Lex.BeginGroup}:rest) = ([], rest)
extractCommand (Lex.CharCat{cat=Lex.EndGroup}:rest) = ([], rest)
extractCommand (Lex.CharCat{cat=Lex.MathShift}:rest) = ([], rest)
extractCommand (Lex.CharCat{cat=Lex.AlignTab}:rest) = ([], rest)
extractCommand (Lex.CharCat{cat=Lex.Parameter}:rest) = ([], rest)
extractCommand (Lex.CharCat{cat=Lex.Superscript}:rest) = ([], rest)
extractCommand (Lex.CharCat{cat=Lex.Subscript}:rest) = ([], rest)
extractCommand (Lex.CharCat{cat=Lex.Space}:rest) = ([AddSpace ExplicitSpace], rest)
extractCommand (Lex.CharCat{char=char, cat=Lex.Letter}:rest) = ([AddCharacter{method=ExplicitChar, code=char}], rest)
extractCommand (Lex.CharCat{char=char, cat=Lex.Other}:rest) = ([AddCharacter{method=ExplicitChar, code=char}], rest)
extractCommand (Lex.CharCat{cat=Lex.Active}:rest) = ([], rest)
extractCommand (Lex.ControlSequence (Lex.ControlWord name):rest)
  | name == "par" = ([StartParagraph{indent=True}], rest)
  | name == "font" = ([Assign $ NonMacroAssign SelectFont], rest)
  | otherwise = ([], rest)
extractCommand (Lex.ControlSequence (Lex.ControlSymbol _):rest) = ([], rest)

extractAll :: [Lex.Token] -> [Command]
extractAll toks = concat $ chop extractCommand toks
