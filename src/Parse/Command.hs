{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module Parse.Command where

import qualified Text.Megaparsec as P
import Text.Megaparsec ((<|>))
import qualified Data.Char as C
import Path (Path, Rel, File, parseRelFile)

import qualified Expand
import qualified Lex
import qualified Categorise as Cat

import qualified Parse.AST as AST
import Parse.Util (Parser, Stream, ParseState, NullParser, ParseError, MatchToken, skipOneOptionalSatisfied, easyRunParser', satisfyThen, skipSatisfiedEquals)
import qualified Parse.Util as PU
import qualified Parse.Common as PC
import qualified Parse.Quantity as PQ

data CharSource = ExplicitChar | CodeChar | TokenChar
  deriving Show

data ControlSequenceLike = ActiveCharacter Cat.CharCode | ControlSequence Lex.ControlSequence
  deriving Show

data AssignmentBody
  -- = DefineMacro { name :: ControlSequenceLike
  --               , parameters :: ParameterText
  --               , contents :: BalancedText
  --               , long, outer, expanded :: Bool }
  -- | ShortDefine {quantity :: QuantityType, name :: ControlSequenceLike, value :: Int}
  -- | SetVariable VariableAssignment
  -- | ModifyVariable VariableModification
  -- | AssignCode { codeType :: CodeType, codeIndex, value :: Int }
  -- | Let { future :: Bool, name :: ControlSequenceLike, target :: Token}
  -- | FutureLet { name :: ControlSequenceLike, token1, token2 :: Token}
  = SelectFont Int
  -- | SetFamilyMember {member :: FamilyMember, font :: Font}
  -- | SetParShape
  -- | Read
  -- | DefineBox
  -- TEMP: Dummy label constructor until properly implemented.
  | DefineFont ControlSequenceLike (Path Rel File)
  -- -- Global assignments.
  -- | SetFontAttribute
  -- | SetHyphenation
  -- | SetBoxSize
  -- | SetInteractionMode
  -- | SetSpecialVariable
  deriving Show

data Assignment
  = Assignment { body :: AssignmentBody, global :: Bool }
  deriving Show

data AllModesCommand
  = Relax
  | Assign Assignment
  -- | LeftBrace
  -- | RightBrace
  -- | BeginGroup
  -- | EndGroup
  -- | ShowToken Token
  -- | ShowBox Int
  -- | ShowLists
  -- | ShowInternalQuantity InternalQuantity
  -- | ShipOut Box
  -- | IgnoreSpaces
  -- | SetAfterAssignmentToken Token
  -- | AddToAfterGroupTokens Tokens
  -- | ChangeCase VDirection GeneralText
  -- | Message MessageStream GeneralText
  -- | OpenInput { streamNr :: Int, fileName :: String }
  -- | CloseInput { streamNr :: Int }
  -- | OpenOutput { streamNr :: Int, fileName :: String, immediate :: Bool }
  -- | CloseOutput { streamNr :: Int, immediate :: Bool }
  -- | Write { streamNr :: Int, contents :: GeneralText, immediate :: Bool }
  -- | AddWhatsit GeneralText
  -- | AddPenalty Int
  | AddKern AST.Length
  -- | RemoveLastPenalty
  -- | RemoveLastKern
  -- | RemoveLastGlue
  -- | AddMark GeneralText
  -- -- Note: this *is* an all-modes command. It can happen in non-vertical modes,
  -- -- then can 'migrate' out.
  -- | AddInsertion {nr :: Int, contents :: VModeMaterial}
  -- | AddGlue Glue
  -- | AddLeaders {type :: LeadersType, template :: BoxOrRule, glue :: Glue}
  | AddSpace
  -- | AddBox Box
  -- | AddShiftedBox Distance Box
  -- | AddFetchedBox { register :: Int, unwrap, pop :: Bool } -- \box, \copy, \un{v,h}{box,copy}
  -- | AddRule { width, height, depth :: Maybe Distance }
  -- | AddAlignedMaterial DesiredLength AlignmentMaterial
  | StartParagraph { indent :: Bool }
  | EndParagraph
  deriving Show

data VModeCommand
  = VAllModesCommand AllModesCommand
  | EnterHMode
  | End
  -- | Dump
  deriving Show

data HModeCommand
  = HAllModesCommand AllModesCommand
  | LeaveHMode
  -- | EnterMathMode
  -- | AddAdjustment VModeMaterial
  -- | AddControlSpace
  | AddCharacter { method :: CharSource, code :: Int }
  -- | AddAccentedCharacter { accentCode :: Int, targetCode :: Maybe Int, assignments :: [Assignment]}
  -- | AddItalicCorrection
  -- | AddDiscretionaryText { preBreak, postBreak, noBreak :: GeneralText }
  deriving Show

-- All-mode Commands.

type AllModeCommandParser = Parser AllModesCommand

cRelax :: AllModeCommandParser
cRelax = do
  skipSatisfiedEquals Expand.Relax
  return Relax

cAddKern :: AllModeCommandParser
cAddKern = do
  skipSatisfiedEquals Expand.AddKern
  ln <- PQ.parseLength
  return $ AddKern ln

cStartParagraph :: AllModeCommandParser
cStartParagraph = satisfyThen parToCom
  where
    parToCom (Expand.StartParagraph _indent) = Just StartParagraph{indent=_indent}
    parToCom _ = Nothing

cEndParagraph :: AllModeCommandParser
cEndParagraph = do
  skipSatisfiedEquals Expand.EndParagraph
  return EndParagraph

-- \font <control-sequence> <equals> <file-name> <at-clause>
cMacroToFont :: AllModeCommandParser
cMacroToFont = do
  skipSatisfiedEquals Expand.MacroToFont
  cs <- parseCSName
  skipOptionalEquals
  fontPath <- parseFileName
  return $ Assign Assignment {body=DefineFont cs fontPath, global=False}

cTokenForFont :: AllModeCommandParser
cTokenForFont = satisfyThen tokToCom
  where
    tokToCom (Expand.TokenForFont n) = Just $ Assign Assignment {body=SelectFont n , global=False}
    tokToCom _ = Nothing

cAddSpace :: AllModeCommandParser
cAddSpace = do
  PU.skipSatisfied isSpace
  return AddSpace

cCommands :: [Parser AllModesCommand]
cCommands =
  [ cRelax
  , cAddKern
  , cStartParagraph
  , cEndParagraph
  , cMacroToFont
  , cTokenForFont
  , cAddSpace
  ]

parseAllModeCommand :: Parser AllModesCommand
parseAllModeCommand = P.choice cCommands

parseCSName :: Parser ControlSequenceLike
parseCSName = do
  PU.disableExpansion
  csLike <- satisfyThen parseCSLike
  PU.enableExpansion
  return csLike
  where
    parseCSLike (Expand.CharCat Lex.LexCharCat{cat=Lex.Active, char=c}) = Just $ ActiveCharacter c
    parseCSLike (Expand.UnexpandedControlSequence cs) = Just $ ControlSequence cs
    parseCSLike _ = Nothing

-- <file name> = <optional spaces> <some explicit letter or digit characters> <space>
parseFileName :: Parser (Path Rel File)
parseFileName = do
  PC.skipOptionalSpaces
  nameCodes <- P.some $ satisfyThen tokToChar
  let name = fmap C.chr nameCodes
  PU.skipSatisfied isSpace
  case parseRelFile (name ++ ".tfm") of
    Just p -> return p
    Nothing -> fail $ "Invalid filename: " ++ name ++ ".tfm"
  where
    tokToChar (Expand.CharCat Lex.LexCharCat{cat=Lex.Letter, char=c}) = Just c
    tokToChar (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=c})
      | PQ.isDigit c = Just c
      | otherwise = Nothing
    tokToChar _ = Nothing

skipOptionalEquals :: NullParser
skipOptionalEquals = do
  PC.skipOptionalSpaces
  skipOneOptionalSatisfied isEquals

-- HMode.

type HModeCommandParser = Parser HModeCommand

extractHModeCommand :: Stream -> (ParseState, Either ParseError HModeCommand)
extractHModeCommand = easyRunParser' parseHModeCommand

parseHModeCommand :: Parser HModeCommand
parseHModeCommand =
  P.choice hCommands
  <|>
  (HAllModesCommand <$> parseAllModeCommand)

hCommands :: [HModeCommandParser]
hCommands =
  [ hLeaveHMode
  , hAddCharacter
  ]

-- HMode Commands.

hLeaveHMode :: HModeCommandParser
hLeaveHMode = do
  PU.skipSatisfied endsHMode
  return LeaveHMode

hAddCharacter :: HModeCommandParser
hAddCharacter = do
  c <- satisfyThen charToCode
  return AddCharacter{method=ExplicitChar, code=c}
  where
    charToCode (Expand.CharCat Lex.LexCharCat{cat=Lex.Letter, char=c}) = Just c
    charToCode (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=c}) = Just c
    charToCode _ = Nothing

-- VMode.

type VModeCommandParser = Parser VModeCommand

extractVModeCommand :: Stream -> (ParseState, Either ParseError VModeCommand)
extractVModeCommand = easyRunParser' parseVModeCommand

parseVModeCommand :: Parser VModeCommand
parseVModeCommand =
  P.choice vCommands
  <|>
  (VAllModesCommand <$> parseAllModeCommand)

vCommands :: [VModeCommandParser]
vCommands =
  [ vEnterHMode
  , vEnd
  ]

-- VMode Commands.

vEnd :: VModeCommandParser
vEnd = do
  skipSatisfiedEquals Expand.End
  return End

vEnterHMode :: VModeCommandParser
vEnterHMode = do
  PU.skipSatisfied startsHMode
  return EnterHMode

-- Token matching.

endsHMode :: MatchToken
endsHMode Expand.End = True
-- endsHMode Expand.Dump = True
-- TODO:
-- - AddUnwrappedFetchedBox Vertical
-- - AddUnwrappedFetchedBox Vertical
-- - AddAlignedMaterial Horizontal
-- - AddRule Horizontal
-- - AddSpecifiedGlue Vertical
-- - AddPresetGlue Vertical
endsHMode _ = False

startsHMode :: MatchToken
startsHMode x
  | isLetterOrOther x = True
  | otherwise = False
-- TODO:
-- - \char
-- - TokenForCharacter
-- - AddUnwrappedFetchedBox Horizontal
-- - AddUnwrappedFetchedBox Horizontal
-- - AddAlignedMaterial Vertical
-- - AddRule Vertical
-- - AddSpecifiedGlue Horizontal
-- - AddPresetGlue Horizontal
-- - AddAccentedCharacter
-- - AddItalicCorrection
-- - AddDiscretionaryText
-- - AddDiscretionaryHyphen
-- - ToggleMathMode

isLetterOrOther :: MatchToken
isLetterOrOther x = isLetter x || isOther x

isTokenForFont :: MatchToken
isTokenForFont (Expand.TokenForFont _) = True
isTokenForFont _ = False

isUnexpandedControlSequence :: MatchToken
isUnexpandedControlSequence (Expand.UnexpandedControlSequence _) = True
isUnexpandedControlSequence _ = False

isActiveCharacter :: MatchToken
isActiveCharacter (Expand.CharCat Lex.LexCharCat{cat=Lex.Active}) = True
isActiveCharacter _ = False

-- <space token> = character token of category [space], or a control sequence
-- or active character \let equal to such.
isSpace :: MatchToken
isSpace (Expand.CharCat Lex.LexCharCat{cat=Lex.Space}) = True
isSpace _ = False

isEquals :: MatchToken
isEquals (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=61}) = True
isEquals _ = False

isLetter :: MatchToken
isLetter (Expand.CharCat Lex.LexCharCat{cat=Lex.Letter}) = True
isLetter _ = False

isOther :: MatchToken
isOther (Expand.CharCat Lex.LexCharCat{cat=Lex.Other}) = True
isOther _ = False
