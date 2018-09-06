{-# LANGUAGE DuplicateRecordFields #-}

module HeX.Parse.Expanded.Command where

import Control.Monad (when)
import Path (File, Path, Rel, parseRelFile)
import qualified Text.Megaparsec as P
import Text.Megaparsec ((<|>))

import qualified HeX.Lex as Lex
import HeX.Categorise (CharCode)

import HeX.Parse.Helpers

import HeX.Parse.Lexed.Inhibited
import qualified HeX.Parse.Resolved.Token as R

import HeX.Parse.Expanded.Common
import HeX.Parse.Expanded.Glue
import HeX.Parse.Expanded.Length
import HeX.Parse.Expanded.Number
import HeX.Parse.Expanded.Stream

-- AST.
data CharSource
  = ExplicitChar
  | CodeChar
  | TokenChar
  deriving (Show)

data MacroPrefix
  = Long
  | Outer
  | Global
  deriving (Eq)

data AssignmentBody
  = DefineMacro { name :: Lex.ControlSequenceLike
                , parameters :: [R.MacroParameter]
                , contents :: BalancedText
                , long, outer :: Bool }
  -- | ShortDefine {quantity :: QuantityType, name :: ControlSequenceLike, value :: Int}
  -- | SetVariable VariableAssignment
  -- | ModifyVariable VariableModificatxion
  -- | AssignCode { codeType :: CodeType, codeIndex, value :: Int }
  -- | Let { future :: Bool, name :: ControlSequenceLike, target :: Token}
  -- | FutureLet { name :: ControlSequenceLike, token1, token2 :: Token}
  | SelectFont Int
  -- | SetFamilyMember {member :: FamilyMember, font :: Font}
  -- | SetParShape
  -- | Read
  -- | DefineBox
  -- TEMP: Dummy label constructor until properly implemented.
  | DefineFont Lex.ControlSequenceLike
               (Path Rel File)
  -- -- Global assignments.
  -- | SetFontAttribute
  -- | SetHyphenation
  -- | SetBoxSize
  -- | SetInteractionMode
  -- | SetSpecialVariable
  deriving (Show)

data Assignment = Assignment
  { body :: AssignmentBody
  , global :: Bool
  } deriving (Show)

data AllModesCommand
  = Assign Assignment
  | Relax
  -- | LeftBrace
  -- | RightBrace
  -- | BeginGroup
  -- | EndGroup
  -- | ShowToken Token
  -- | ShowBox Int
  -- | ShowLists
  -- | ShowInternalQuantity InternalQuantity
  -- | ShipOut Box
  | IgnoreSpaces
  -- | SetAfterAssignmentToken Token
  -- | AddToAfterGroupTokens Tokens
  -- | Message MessageStream GeneralText
  -- | OpenInput { streamNr :: Int, fileName :: String }
  -- | CloseInput { streamNr :: Int }
  -- | OpenOutput { streamNr :: Int, fileName :: String, immediate :: Bool }
  -- | CloseOutput { streamNr :: Int, immediate :: Bool }
  -- | Write { streamNr :: Int, contents :: GeneralText, immediate :: Bool }
  -- | AddWhatsit GeneralText
  | AddPenalty Number
  | AddKern Length
  -- | RemoveLastPenalty
  -- | RemoveLastKern
  -- | RemoveLastGlue
  -- | AddMark GeneralText
  -- -- Note: this *is* an all-modes command. It can happen in non-vertical modes,
  -- -- then can 'migrate' out.
  -- | AddInsertion {nr :: Int, contents :: VModeMaterial}
  | AddGlue Glue
  -- | AddLeaders {type :: LeadersType, template :: BoxOrRule, glue :: Glue}
  | AddSpace
  -- | AddBox Box
  -- | AddShiftedBox Distance Box
  -- | AddFetchedBox { register :: Int, unwrap, pop :: Bool } -- \box, \copy, \un{v,h}{box,copy}
  | AddRule { width, height, depth :: Maybe Length }
  -- | AddAlignedMaterial DesiredLength AlignmentMaterial
  | StartParagraph { indent :: Bool }
  | EndParagraph
  deriving (Show)

data VModeCommand
  = VAllModesCommand AllModesCommand
  | EnterHMode
  | End
  -- | Dump
  deriving (Show)

data HModeCommand
  = HAllModesCommand AllModesCommand
  | LeaveHMode
  -- | EnterMathMode
  -- | AddAdjustment VModeMaterial
  -- | AddControlSpace
  | AddCharacter { method :: CharSource
                 , char :: CharCode }
  -- | AddAccentedCharacter { accentCode :: Int, targetCode :: Maybe Int, assignments :: [Assignment]}
  -- | AddItalicCorrection
  -- | AddDiscretionaryText { preBreak, postBreak, noBreak :: GeneralText }
  deriving (Show)

-- Entry-points.
extractHModeCommand ::
     ExpandedStream
  -> (P.State ExpandedStream, Either (ParseError ExpandedStream) HModeCommand)
extractHModeCommand = easyRunParser parseHModeCommand

extractVModeCommand ::
     ExpandedStream
  -> (P.State ExpandedStream, Either (ParseError ExpandedStream) VModeCommand)
extractVModeCommand = easyRunParser parseVModeCommand

-- Parse.
-- All-mode Commands.
type AllModeCommandParser = SimpExpandParser AllModesCommand

parseAllModeCommand :: R.Axis -> SimpExpandParser AllModesCommand
parseAllModeCommand mode =
  P.choice
    [ relax
    , ignorespaces
    , tokenForFont
    , macroToFont
    , addPenalty
    , addKern
    , addSpecifiedGlue mode
    , addSpace
    , addRule mode
    , startParagraph
    , endParagraph
    , defineMacro
    ]

checkModeAndToken ::
     R.Axis
  -> (R.ModedCommandPrimitiveToken -> Bool)
  -> R.PrimitiveToken
  -> Bool
checkModeAndToken m1 chk (R.ModedCommand m2 tok) = (m1 == m2) && chk tok
checkModeAndToken _ _ _ = False

relax :: AllModeCommandParser
relax = do
  skipSatisfiedEquals R.Relax
  return Relax

ignorespaces :: AllModeCommandParser
ignorespaces = do
  skipSatisfiedEquals R.IgnoreSpaces
  skipOptionalSpaces
  return IgnoreSpaces

tokenForFont :: AllModeCommandParser
tokenForFont = satisfyThen tokToCom
  where
    tokToCom (R.TokenForFont n) =
      Just $ Assign Assignment {body = SelectFont n, global = False}
    tokToCom _ = Nothing

-- \font <control-sequence> <equals> <file-name> <at-clause>
macroToFont :: AllModeCommandParser
macroToFont = do
  skipSatisfiedEquals R.MacroToFont
  cs <- parseInhibited parseCSName
  skipOptionalEquals
  fontPath <- parseFileName
  return $ Assign Assignment {body = DefineFont cs fontPath, global = False}
    -- <file name> = <optional spaces> <some explicit letter or digit characters> <space>
  where
    parseFileName :: SimpExpandParser (Path Rel File)
    parseFileName = do
      skipOptionalSpaces
      fileName <- P.some $ satisfyThen tokToChar
      skipSatisfied isSpace
      case parseRelFile (fileName ++ ".tfm") of
        Just p -> return p
        Nothing -> fail $ "Invalid filename: " ++ fileName ++ ".tfm"
    tokToChar (R.CharCat Lex.CharCat {cat = Lex.Letter, char = c}) = Just c
    -- 'Other' Characters for decimal digits are OK.
    tokToChar (R.CharCat Lex.CharCat {cat = Lex.Other, char = c}) =
      case c of
        '0' -> Just c
        '1' -> Just c
        '2' -> Just c
        '3' -> Just c
        '4' -> Just c
        '5' -> Just c
        '6' -> Just c
        '7' -> Just c
        '8' -> Just c
        '9' -> Just c
      -- Not in the spec, but let's say "/" and "." are OK.
        '/' -> Just c
        '.' -> Just c
        _ -> Nothing
    tokToChar _ = Nothing

skipOptionalEquals :: NullSimpParser ExpandedStream
skipOptionalEquals = do
  skipOptionalSpaces
  skipOneOptionalSatisfied isEquals

addPenalty :: AllModeCommandParser
addPenalty = do
  skipSatisfiedEquals R.AddPenalty
  AddPenalty <$> parseNumber

addKern :: AllModeCommandParser
addKern = do
  skipSatisfiedEquals R.AddKern
  AddKern <$> parseLength

addSpecifiedGlue :: R.Axis -> AllModeCommandParser
addSpecifiedGlue mode = do
  skipSatisfied $ checkModeAndToken mode (== R.AddSpecifiedGlue)
  AddGlue <$> parseGlue

addSpace :: AllModeCommandParser
addSpace = const AddSpace <$> skipSatisfied isSpace

addRule :: R.Axis -> AllModeCommandParser
addRule mode = do
  skipSatisfied $ checkModeAndToken mode (== R.AddRule)
  let cmd = AddRule {width = Nothing, height = Nothing, depth = Nothing}
  parseRuleSpecification cmd
  where
    parseRuleSpecification cmd = do
      skipOptionalSpaces
      x <-
        P.optional $
        P.try $
        P.choice [parseRuleWidth cmd, parseRuleHeight cmd, parseRuleDepth cmd]
      case x of
        Just newCmd -> parseRuleSpecification newCmd
        Nothing -> return cmd
    parseRuleWidth cmd = do
      skipKeyword "width"
      ln <- parseLength
      return cmd {width = Just ln}
    parseRuleHeight cmd = do
      skipKeyword "height"
      ln <- parseLength
      return cmd {height = Just ln}
    parseRuleDepth cmd = do
      skipKeyword "depth"
      ln <- parseLength
      return cmd {depth = Just ln}

startParagraph :: AllModeCommandParser
startParagraph = satisfyThen parToCom
  where
    parToCom (R.StartParagraph _indent) =
      Just StartParagraph {indent = _indent}
    parToCom _ = Nothing

endParagraph :: AllModeCommandParser
endParagraph = const EndParagraph <$> skipSatisfiedEquals R.EndParagraph

defineMacro :: AllModeCommandParser
defineMacro = do
  prefixes <- P.many $ satisfyThen tokToPrefix
  (defGlobal, defExpand) <- satisfyThen tokToDef
  cs <- parseInhibited parseCSName
  params <- parseParameters
  skipSatisfied isExplicitLeftBrace
  when defExpand $ error "expanded-def not implemented"
  _contents <- parseInhibited parseBalancedText
  return $
    Assign $
    Assignment
    { body =
        DefineMacro
        { name = cs
        , parameters = params
        , contents = _contents
        , long = Long `elem` prefixes
        , outer = Outer `elem` prefixes
        }
    , global = defGlobal || Global `elem` prefixes
    }
  where
    tokToPrefix R.Global = Just Global
    tokToPrefix R.Outer = Just Outer
    tokToPrefix R.Long = Just Long
    tokToPrefix _ = Nothing
    tokToDef R.DefineMacro {global = _global, expand = _expand} =
      Just (_global, _expand)
    tokToDef _ = Nothing
    -- TODO.
    parseParameters = return []

-- HMode.
type HModeCommandParser = SimpExpandParser HModeCommand

parseHModeCommand :: SimpExpandParser HModeCommand
parseHModeCommand =
  P.choice [leaveHMode, addCharacter] <|>
  (HAllModesCommand <$> parseAllModeCommand R.Horizontal)

leaveHMode :: HModeCommandParser
leaveHMode = do
  skipSatisfied endsHMode
  return LeaveHMode
  where
    endsHMode R.End = True
    endsHMode (R.ModedCommand R.Vertical _) = True
    -- endsHMode Dump = True
    endsHMode _ = False

addCharacter :: HModeCommandParser
addCharacter = do
  c <- satisfyThen charToCode
  return AddCharacter {method = ExplicitChar, char = c}
  where
    charToCode (R.CharCat Lex.CharCat {cat = Lex.Letter, char = c}) =
      Just c
    charToCode (R.CharCat Lex.CharCat {cat = Lex.Other, char = c}) = Just c
    charToCode _ = Nothing

-- VMode.
type VModeCommandParser = SimpExpandParser VModeCommand

parseVModeCommand :: SimpExpandParser VModeCommand
parseVModeCommand =
  P.choice [enterHMode, end] <|>
  (VAllModesCommand <$> parseAllModeCommand R.Vertical)

end :: VModeCommandParser
end = do
  skipSatisfiedEquals R.End
  return End

enterHMode :: VModeCommandParser
enterHMode = do
  skipSatisfied startsHMode
  return EnterHMode
  where
    startsHMode (R.ModedCommand R.Horizontal _) = True
    startsHMode x
      | isLetter x = True
      | isOther x = True
      | otherwise = False
    -- TODO:
    -- - \char
    -- - TokenForCharacter
    -- - AddAccentedCharacter
    -- - AddItalicCorrection
    -- - AddDiscretionaryText
    -- - AddDiscretionaryHyphen
    -- - ToggleMathMode
