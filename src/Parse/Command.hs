{-# LANGUAGE DuplicateRecordFields #-}

module Parse.Command where

import qualified Text.Megaparsec as P
import Text.Megaparsec ((<|>))
import qualified Data.Char as C
import Path (Path, Rel, File, parseRelFile)

import qualified Expand
import qualified Lex
import qualified Categorise as Cat

import Parse.Util (Parser, Stream, ParseState, NullParser, ParseError, skipOneOptionalSatisfied, easyRunParser', satisfyThen, skipSatisfiedEquals)
import qualified Parse.Util as PU
import qualified Parse.Common as PC
import qualified Parse.Length as PL
import qualified Parse.Glue as PG

-- AST.

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
  | AddKern PL.Length
  -- | RemoveLastPenalty
  -- | RemoveLastKern
  -- | RemoveLastGlue
  -- | AddMark GeneralText
  -- -- Note: this *is* an all-modes command. It can happen in non-vertical modes,
  -- -- then can 'migrate' out.
  -- | AddInsertion {nr :: Int, contents :: VModeMaterial}
  | AddGlue PG.Glue
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


-- Entry-points.

extractHModeCommand :: Stream -> (ParseState, Either ParseError HModeCommand)
extractHModeCommand = easyRunParser' parseHModeCommand

extractVModeCommand :: Stream -> (ParseState, Either ParseError VModeCommand)
extractVModeCommand = easyRunParser' parseVModeCommand

-- Parse.

-- All-mode Commands.

type AllModeCommandParser = Parser AllModesCommand

parseAllModeCommand :: Expand.Axis -> Parser AllModesCommand
parseAllModeCommand mode = P.choice [ relax
                                    , addKern
                                    , addSpecifiedGlue mode
                                    , startParagraph
                                    , endParagraph
                                    , macroToFont
                                    , tokenForFont
                                    , addSpace
                                    ]

relax :: AllModeCommandParser
relax = do
  skipSatisfiedEquals Expand.Relax
  return Relax

addKern :: AllModeCommandParser
addKern = do
  skipSatisfiedEquals Expand.AddKern
  ln <- PL.parseLength
  return $ AddKern ln

addSpecifiedGlue :: Expand.Axis -> AllModeCommandParser
addSpecifiedGlue mode = do
  PU.skipSatisfied $ checkModeAndToken mode (== Expand.AddSpecifiedGlue)
  AddGlue <$> PG.parseGlue

checkModeAndToken :: Expand.Axis -> (Expand.ModedCommandParseToken -> Bool) ->
                     Expand.ParseToken -> Bool
checkModeAndToken m1 chk (Expand.ModedCommand m2 tok) = (m1 == m2) && chk tok
checkModeAndToken _ _ _ = False

startParagraph :: AllModeCommandParser
startParagraph = satisfyThen parToCom
  where
    parToCom (Expand.StartParagraph _indent) = Just StartParagraph{indent=_indent}
    parToCom _ = Nothing

endParagraph :: AllModeCommandParser
endParagraph = do
  skipSatisfiedEquals Expand.EndParagraph
  return EndParagraph

-- \font <control-sequence> <equals> <file-name> <at-clause>
macroToFont :: AllModeCommandParser
macroToFont = do
  skipSatisfiedEquals Expand.MacroToFont
  cs <- parseCSName
  skipOptionalEquals
  fontPath <- parseFileName
  return $ Assign Assignment {body=DefineFont cs fontPath, global=False}

skipOptionalEquals :: NullParser
skipOptionalEquals = do
  PC.skipOptionalSpaces
  skipOneOptionalSatisfied PC.isEquals

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
  PU.skipSatisfied PC.isSpace
  case parseRelFile (name ++ ".tfm") of
    Just p -> return p
    Nothing -> fail $ "Invalid filename: " ++ name ++ ".tfm"
  where
    tokToChar (Expand.CharCat Lex.LexCharCat{cat=Lex.Letter, char=c}) = Just c
    -- 'Other' Characters for decimal digits are OK.
    tokToChar (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=c}) = case c of
      48 -> Just c
      49 -> Just c
      50 -> Just c
      51 -> Just c
      52 -> Just c
      53 -> Just c
      54 -> Just c
      55 -> Just c
      56 -> Just c
      57 -> Just c
      _ -> Nothing
    tokToChar _ = Nothing

tokenForFont :: AllModeCommandParser
tokenForFont = satisfyThen tokToCom
  where
    tokToCom (Expand.TokenForFont n) = Just $ Assign Assignment {body=SelectFont n , global=False}
    tokToCom _ = Nothing

addSpace :: AllModeCommandParser
addSpace = do
  PU.skipSatisfied PC.isSpace
  return AddSpace

-- HMode.

type HModeCommandParser = Parser HModeCommand

parseHModeCommand :: Parser HModeCommand
parseHModeCommand =
  P.choice [ leaveHMode
           , addCharacter
           ]
  <|>
  (HAllModesCommand <$> parseAllModeCommand Expand.Horizontal)

leaveHMode :: HModeCommandParser
leaveHMode = do
  PU.skipSatisfied endsHMode
  return LeaveHMode
  where
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


addCharacter :: HModeCommandParser
addCharacter = do
  c <- satisfyThen charToCode
  return AddCharacter{method=ExplicitChar, code=c}
  where
    charToCode (Expand.CharCat Lex.LexCharCat{cat=Lex.Letter, char=c}) = Just c
    charToCode (Expand.CharCat Lex.LexCharCat{cat=Lex.Other, char=c}) = Just c
    charToCode _ = Nothing

-- VMode.

type VModeCommandParser = Parser VModeCommand

parseVModeCommand :: Parser VModeCommand
parseVModeCommand =
  P.choice [ enterHMode
           , end
           ]
  <|>
  (VAllModesCommand <$> parseAllModeCommand Expand.Vertical)

end :: VModeCommandParser
end = do
  skipSatisfiedEquals Expand.End
  return End

enterHMode :: VModeCommandParser
enterHMode = do
  PU.skipSatisfied startsHMode
  return EnterHMode
  where
    startsHMode x
      | PC.isLetter x = True
      | PC.isOther x = True
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
