{-# LANGUAGE DuplicateRecordFields #-}

module Command where

import Data.List.Split (chop)

import qualified Lex
import qualified Cat

data Axis = Horizontal | Vertical
  deriving Show
data HDirection = Left | Right
  deriving Show
data VDirection = Up | Down
  deriving Show
data Stream = Out | Err
  deriving Show

data CharSource = ExplicitChar | CodeChar | TokenChar
  deriving Show
data SpaceSource = ExplicitSpace | ControlSpace
  deriving Show
data InsertionType = Insertion | Adjustment
  deriving Show

-- data Distance = Distance Int
  -- deriving Show
-- data HDisplacement = HDisplacement HDirection Distance | HDefault
  -- deriving Show
-- data VDisplacement = VDisplacement VDirection Distance | VDefault
  -- deriving Show
-- data BoxPlacement = HBox HDisplacement | VBox VDisplacement

-- TODO.
data MacroName = ActiveChar Char | ControlSequence String
  deriving Show
-- TODO.
data ParameterText = ParameterText [String]
  deriving Show
-- TODO.
data BalancedText = BalancedText [String]
  deriving Show


data Assignment
  = DefineMacro { name :: MacroName
                , parameters :: ParameterText
                , contents :: BalancedText
                , long :: Bool
                , outer :: Bool
                , expanded :: Bool }
  | SetVariable
  | ModifyVariable
  | AssignCode
  | Let
  | ShortDefine
  | SelectFont
  | SetFamilyMember
  | SetParShape
  | Read
  | DefineBox
  | DefineFont
  -- Global assignments.
  | SetFontAttribute
  | SetHyphenation
  | SetBoxSize
  | SetInteractionMode
  | SetSpecialVariable
  deriving Show

data Command
  = Relax
  | Assign {assignment :: Assignment, global :: Bool }
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
  deriving Show

extractCommand :: Cat.CharCatMap -> Lex.LexState -> [Cat.CharCode] -> (Command, [Cat.CharCode], Lex.LexState)
extractCommand ccMap lexState0 cs
  | Lex.CharCat{cat=Lex.Space} <- tok1 =
    (AddSpace ExplicitSpace, rest, lexState1)
  | Lex.CharCat{char=char, cat=Lex.Letter} <- tok1 =
    (AddCharacter{method=ExplicitChar, code=char}, rest, lexState1)
  | Lex.CharCat{char=char, cat=Lex.Other} <- tok1 =
    (AddCharacter{method=ExplicitChar, code=char}, rest, lexState1)
  | Lex.ControlSequence (Lex.ControlWord "par") <- tok1
    = (StartParagraph{indent=True}, rest, lexState1)
  | Lex.ControlSequence (Lex.ControlWord "font") <- tok1
    = (Assign {assignment=SelectFont, global=False}, rest, lexState1)
  where
    (tok1, lexState1, rest) = Lex.extractToken ccMap lexState0 cs

extractAllInner :: Cat.CharCatMap -> Lex.LexState -> [Cat.CharCode] -> [Command]
extractAllInner _ _ [] = []
extractAllInner ccMap state cs =
    let (thisCom, rest, nextState) = extractCommand ccMap state cs
    in thisCom:extractAllInner ccMap nextState rest

extractAll :: Cat.CharCatMap -> [Cat.CharCode] -> [Command]
extractAll ccMap cs = extractAllInner ccMap Lex.LineBegin cs
