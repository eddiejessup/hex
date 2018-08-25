{-# LANGUAGE DuplicateRecordFields #-}

module Parse.Command where

import qualified Text.Megaparsec as P
import Text.Megaparsec ((<|>))
import qualified Data.Char as C
import Path (Path, Rel, File, parseRelFile)
import Control.Monad (when)

import qualified Expand
import qualified Lex

import Parse.Stream (SimpExpandParser, ExpandedStream)
import Parse.Helpers (ParseError, NullSimpParser, skipOneOptionalSatisfied, satisfyThen, skipSatisfiedEquals, skipSatisfied, easyRunParser)
import Parse.Inhibited (parseInhibited, parseGeneralText, parseBalancedText, parseCSName)
import qualified Parse.Common as PC
import qualified Parse.Number as PN
import qualified Parse.Length as PL
import qualified Parse.Glue as PG

-- AST.

data CharSource = ExplicitChar | CodeChar | TokenChar
  deriving Show

data MacroPrefix
  = Long
  | Outer
  | Global
  deriving Eq

data AssignmentBody
  = DefineMacro { name :: Lex.ControlSequenceLike
                , parameters :: [Expand.MacroParameter]
                , contents :: Expand.BalancedText
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
  | DefineFont Lex.ControlSequenceLike (Path Rel File)
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
  | ChangeCase Expand.VDirection Expand.BalancedText
  -- | Message MessageStream GeneralText
  -- | OpenInput { streamNr :: Int, fileName :: String }
  -- | CloseInput { streamNr :: Int }
  -- | OpenOutput { streamNr :: Int, fileName :: String, immediate :: Bool }
  -- | CloseOutput { streamNr :: Int, immediate :: Bool }
  -- | Write { streamNr :: Int, contents :: GeneralText, immediate :: Bool }
  -- | AddWhatsit GeneralText
  | AddPenalty PN.Number
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
  | AddRule { width, height, depth :: Maybe PL.Length }
  -- | AddAlignedMaterial DesiredLength AlignmentMaterial
  | StartParagraph { indent :: Bool }
  | EndParagraph

  -- Unofficial stuff while playing.
  | ExpandMacro Expand.Macro
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

extractHModeCommand :: ExpandedStream -> (P.State ExpandedStream, Either (ParseError ExpandedStream) HModeCommand)
extractHModeCommand = easyRunParser parseHModeCommand

extractVModeCommand :: ExpandedStream -> (P.State ExpandedStream, Either (ParseError ExpandedStream) VModeCommand)
extractVModeCommand = easyRunParser parseVModeCommand

-- Parse.

-- All-mode Commands.

type AllModeCommandParser = SimpExpandParser AllModesCommand

parseAllModeCommand :: Expand.Axis -> SimpExpandParser AllModesCommand
parseAllModeCommand mode = P.choice [ relax
                                    , ignorespaces
                                    , changeCase
                                    , tokenForFont
                                    , macroToFont
                                    , addPenalty
                                    , addKern
                                    , addSpecifiedGlue mode
                                    , addSpace
                                    , addRule mode
                                    , startParagraph
                                    , endParagraph
                                    , expandMacro
                                    , defineMacro
                                    ]

checkModeAndToken :: Expand.Axis -> (Expand.ModedCommandParseToken -> Bool) ->
                     Expand.ParseToken -> Bool
checkModeAndToken m1 chk (Expand.ModedCommand m2 tok) = (m1 == m2) && chk tok
checkModeAndToken _ _ _ = False

relax :: AllModeCommandParser
relax = do
  skipSatisfiedEquals Expand.Relax
  return Relax

ignorespaces :: AllModeCommandParser
ignorespaces = do
  skipSatisfiedEquals Expand.IgnoreSpaces
  PC.skipOptionalSpaces
  return IgnoreSpaces

changeCase :: AllModeCommandParser
changeCase =
  ChangeCase <$> satisfyThen tokToDirection <*> parseGeneralText
  where
    tokToDirection (Expand.ChangeCase d) = Just d
    tokToDirection _ = Nothing

tokenForFont :: AllModeCommandParser
tokenForFont = satisfyThen tokToCom
  where
    tokToCom (Expand.TokenForFont n) = Just $ Assign Assignment {body=SelectFont n , global=False}
    tokToCom _ = Nothing

-- \font <control-sequence> <equals> <file-name> <at-clause>
macroToFont :: AllModeCommandParser
macroToFont = do
  skipSatisfiedEquals Expand.MacroToFont
  cs <- parseInhibited parseCSName
  skipOptionalEquals
  fontPath <- parseFileName
  return $ Assign Assignment {body=DefineFont cs fontPath, global=False}
  where
    -- <file name> = <optional spaces> <some explicit letter or digit characters> <space>
    parseFileName :: SimpExpandParser (Path Rel File)
    parseFileName = do
      PC.skipOptionalSpaces
      nameCodes <- P.some $ satisfyThen tokToChar
      let fileName = fmap C.chr nameCodes
      skipSatisfied PC.isSpace
      case parseRelFile (fileName ++ ".tfm") of
        Just p -> return p
        Nothing -> fail $ "Invalid filename: " ++ fileName ++ ".tfm"

    tokToChar (Expand.CharCat Lex.CharCat{cat=Lex.Letter, char=c}) = Just c
    -- 'Other' Characters for decimal digits are OK.
    tokToChar (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=c}) = case c of
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

skipOptionalEquals :: NullSimpParser ExpandedStream
skipOptionalEquals = do
  PC.skipOptionalSpaces
  skipOneOptionalSatisfied PC.isEquals

addPenalty :: AllModeCommandParser
addPenalty = do
  skipSatisfiedEquals Expand.AddPenalty
  AddPenalty <$> PN.parseNumber

addKern :: AllModeCommandParser
addKern = do
  skipSatisfiedEquals Expand.AddKern
  AddKern <$> PL.parseLength

addSpecifiedGlue :: Expand.Axis -> AllModeCommandParser
addSpecifiedGlue mode = do
  skipSatisfied $ checkModeAndToken mode (== Expand.AddSpecifiedGlue)
  AddGlue <$> PG.parseGlue

addSpace :: AllModeCommandParser
addSpace = const AddSpace <$> skipSatisfied PC.isSpace

addRule :: Expand.Axis -> AllModeCommandParser
addRule mode = do
  skipSatisfied $ checkModeAndToken mode (== Expand.AddRule)
  let cmd = AddRule{width=Nothing, height=Nothing, depth=Nothing}
  parseRuleSpecification cmd
  where
    parseRuleSpecification cmd = do
      PC.skipOptionalSpaces
      x <- P.optional $ P.try $ P.choice [ parseRuleWidth cmd
                                         , parseRuleHeight cmd
                                         , parseRuleDepth cmd ]
      case x of
        Just newCmd -> parseRuleSpecification newCmd
        Nothing -> return cmd

    parseRuleWidth cmd = do
      PC.skipKeyword "width"
      ln <- PL.parseLength
      return cmd{width=Just ln}

    parseRuleHeight cmd = do
      PC.skipKeyword "height"
      ln <- PL.parseLength
      return cmd{height=Just ln}

    parseRuleDepth cmd = do
      PC.skipKeyword "depth"
      ln <- PL.parseLength
      return cmd{depth=Just ln}

startParagraph :: AllModeCommandParser
startParagraph = satisfyThen parToCom
  where
    parToCom (Expand.StartParagraph _indent) = Just StartParagraph{indent=_indent}
    parToCom _ = Nothing

endParagraph :: AllModeCommandParser
endParagraph = const EndParagraph <$> skipSatisfiedEquals Expand.EndParagraph

expandMacro :: AllModeCommandParser
expandMacro = do
  macro <- satisfyThen tokToMacro
  return $ ExpandMacro macro
  where
    tokToMacro (Expand.MacroToken m) = Just m
    tokToMacro _ = Nothing

defineMacro :: AllModeCommandParser
defineMacro = do
  prefixes <- P.many $ satisfyThen tokToPrefix
  (defGlobal, defExpand) <- satisfyThen tokToDef
  cs <- parseInhibited parseCSName
  params <- parseParameters
  skipSatisfied PC.isExplicitLeftBrace
  when defExpand $ error "expanded-def not implemented"
  _contents <- parseInhibited parseBalancedText
  return $ Assign $
    Assignment {
      body=DefineMacro { name=cs
                       , parameters=params
                       , contents=_contents
                       , long=Long `elem` prefixes
                       , outer=Outer `elem` prefixes }
      , global=defGlobal || Global `elem` prefixes
    }

  where
    tokToPrefix Expand.Global = Just Global
    tokToPrefix Expand.Outer = Just Outer
    tokToPrefix Expand.Long = Just Long
    tokToPrefix _ = Nothing

    tokToDef Expand.DefineMacro{global=_global, expand=_expand} = Just (_global, _expand)
    tokToDef _ = Nothing

    -- TODO.
    parseParameters = return []

-- HMode.

type HModeCommandParser = SimpExpandParser HModeCommand

parseHModeCommand :: SimpExpandParser HModeCommand
parseHModeCommand =
  P.choice [ leaveHMode
           , addCharacter
           ]
  <|>
  (HAllModesCommand <$> parseAllModeCommand Expand.Horizontal)

leaveHMode :: HModeCommandParser
leaveHMode = do
  skipSatisfied endsHMode
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
    charToCode (Expand.CharCat Lex.CharCat{cat=Lex.Letter, char=c}) = Just c
    charToCode (Expand.CharCat Lex.CharCat{cat=Lex.Other, char=c}) = Just c
    charToCode _ = Nothing

-- VMode.

type VModeCommandParser = SimpExpandParser VModeCommand

parseVModeCommand :: SimpExpandParser VModeCommand
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
  skipSatisfied startsHMode
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
