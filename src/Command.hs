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
  | AddKern Int
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

extractCommandInner :: Cat.CharCatMap -> Lex.LexState -> Lex.Token -> [Cat.CharCode] -> Maybe (Command, [Cat.CharCode], Lex.LexState)
extractCommandInner ccMap lexState tok1 cs
  | Lex.CharCat{cat=Lex.Space} <- tok1 =
    Just (AddSpace ExplicitSpace, cs, lexState)
  | Lex.CharCat{char=char, cat=Lex.Letter} <- tok1 =
    Just (AddCharacter{method=ExplicitChar, code=char}, cs, lexState)
  | Lex.CharCat{char=char, cat=Lex.Other} <- tok1 =
    Just (AddCharacter{method=ExplicitChar, code=char}, cs, lexState)
  | Lex.ControlSequence (Lex.ControlWord "par") <- tok1 =
    Just (StartParagraph{indent=True}, cs, lexState)
  | Lex.ControlSequence (Lex.ControlWord "dfont") <- tok1 =
    Just (Assign {assignment=DefineFont, global=False}, cs, lexState)
  | Lex.ControlSequence (Lex.ControlWord "sfont") <- tok1 =
    Just (Assign {assignment=SelectFont, global=False}, cs, lexState)
  | Lex.ControlSequence (Lex.ControlWord "relax") <- tok1 =
    Just (Relax, cs, lexState)
  | Lex.ControlSequence (Lex.ControlWord "kern") <- tok1 =
    Just (AddKern 2000000, cs, lexState)

extractCommand :: Cat.CharCatMap -> Lex.LexState -> [Cat.CharCode] -> Maybe (Command, [Cat.CharCode], Lex.LexState)
extractCommand _ _ [] = Nothing
extractCommand ccMap lexState0 cs = do
  (tok1, lexState1, rest) <- Lex.extractToken ccMap lexState0 cs
  extractCommandInner ccMap lexState1 tok1 rest

extractAllInner :: Cat.CharCatMap -> Lex.LexState -> [Cat.CharCode] -> [Command]
extractAllInner _ _ [] = []
extractAllInner ccMap state cs =
  case extractCommand ccMap state cs of
    Nothing -> []
    Just (thisCom, rest, nextState) ->
      thisCom:extractAllInner ccMap nextState rest

extractAll :: Cat.CharCatMap -> [Cat.CharCode] -> [Command]
extractAll ccMap = extractAllInner ccMap Lex.LineBegin
