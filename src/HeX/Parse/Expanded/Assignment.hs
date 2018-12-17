{-# LANGUAGE DuplicateRecordFields #-}

module HeX.Parse.Expanded.Assignment where

import           Control.Monad                  ( when )
import           Path                           ( File
                                                , Path
                                                , Rel
                                                , parseRelFile
                                                )
import qualified Text.Megaparsec               as P
import           Text.Megaparsec               ((<|>))

import qualified HeX.Lex                       as Lex
import           HeX.Parse.Helpers
import qualified HeX.Parse.Lexed.Inhibited     as Inh
import qualified HeX.Parse.Resolved            as R
import           HeX.Parse.Expanded.Common
import           HeX.Parse.Expanded.Stream
import           HeX.Parse.Expanded.Number

-- AST.

data MacroPrefix
  = Long
  | Outer
  | Global
  deriving (Eq)

data MacroAssignment
  = MacroAssignment { name :: Lex.ControlSequenceLike
                    , contents :: Inh.MacroContents
                    , long, outer :: Bool }
  deriving (Show)

data AssignmentBody
  = DefineMacro MacroAssignment
  -- \| ShortDefine {quantity :: QuantityType, name :: ControlSequenceLike, value :: Int}
  | SetVariable VariableAssignment
  -- \| ModifyVariable VariableModificatxion
  -- \| AssignCode { codeType :: CodeType, codeIndex, value :: Int }
  -- \| Let { future :: Bool, name :: ControlSequenceLike, target :: Token}
  -- \| FutureLet { name :: ControlSequenceLike, token1, token2 :: Token}
  | SelectFont Int
  -- \| SetFamilyMember {member :: FamilyMember, font :: Font}
  -- \| SetParShape
  -- \| Read
  -- \| DefineBox
  -- TEMP: Dummy label constructor until properly implemented.
  | DefineFont Lex.ControlSequenceLike
               (Path Rel File)
  -- -- Global assignments.
  -- \| SetFontAttribute
  -- \| SetHyphenation
  -- \| SetBoxSize
  -- \| SetInteractionMode
  -- \| SetSpecialVariable
  deriving (Show)

data VariableAssignment
  = IntegerVariableAssignment IntegerVariable Number
  deriving (Show)

data IntegerVariable
  = IntegerParameter R.IntegerParameter
  -- \| CountDefToken
  -- \| CountRegister
  deriving (Show)

data Assignment = Assignment
  { body :: AssignmentBody
  , global :: Bool
  } deriving (Show)

type AssignmentParser = SimpExpandParser Assignment

parseAssignment :: AssignmentParser
parseAssignment = parseDefineMacro <|> parseNonMacroAssignment

parseNonMacroAssignment :: AssignmentParser
parseNonMacroAssignment = do
  _global <- parseGlobal
  _body <- parseNonMacroAssignmentBody
  pure $ Assignment _body _global
  where
    -- TODO: Parse globals.
    parseGlobal = pure True
    parseNonMacroAssignmentBody
      = P.choice [ parseVariableAssignment
                 , parseTokenForFont
                 , parseMacroToFont ]

skipOptionalEquals :: NullSimpParser ExpandedStream
skipOptionalEquals = do
  skipOptionalSpaces
  skipOneOptionalSatisfied isEquals

-- Parse Macro.

parseDefineMacro :: AssignmentParser
parseDefineMacro = do
  -- Macro prefixes.
  prefixes <- P.many $ satisfyThen tokToPrefix
  -- \def-like thing.
  (defGlobal, defExpand) <- satisfyThen tokToDef
  -- Macro's name.
  cs <- parseInhibited Inh.parseCSName
  -- Parameter text.
  (preParamToks, paramDelims) <- parseInhibited Inh.parseParamText
  -- TODO: Support expanded-def.
  when defExpand $ error "expanded-def not implemented"
  -- Replacement text.
  replaceToks <- parseInhibited Inh.parseMacroText
  pure $
    Assignment
    { body = DefineMacro $ MacroAssignment
        { name = cs
        , contents = Inh.MacroContents preParamToks paramDelims replaceToks
        , long = Long `elem` prefixes
        , outer = Outer `elem` prefixes
        }
    , global = defGlobal || Global `elem` prefixes
    }
  where
    -- TODO: Can avoid this manual mapping by putting it in the token.
    tokToPrefix R.Global = Just Global
    tokToPrefix R.Outer = Just Outer
    tokToPrefix R.Long = Just Long
    tokToPrefix _ = Nothing

    tokToDef R.DefineMacro {global = _global, expand = _expand} =
      Just (_global, _expand)
    tokToDef _ = Nothing

parseTokenForFont :: SimpExpandParser AssignmentBody
parseTokenForFont = satisfyThen tokToCom
  where
    tokToCom (R.TokenForFont n) = Just $ SelectFont n
    tokToCom _ = Nothing

-- \font <control-sequence> <equals> <file-name> <at-clause>
parseMacroToFont :: SimpExpandParser AssignmentBody
parseMacroToFont = do
  skipSatisfiedEquals R.MacroToFont
  cs <- parseInhibited Inh.parseCSName
  skipOptionalEquals
  DefineFont cs <$> parseFileName
    -- <file name> = <optional spaces> <some explicit letter or digit characters> <space>
  where
    parseFileName :: SimpExpandParser (Path Rel File)
    parseFileName = do
      skipOptionalSpaces
      fileName <- P.some $ satisfyThen tokToChar
      skipSatisfied isSpace
      case parseRelFile (fileName ++ ".tfm") of
        Just p -> pure p
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

-- Variable assignment

parseVariableAssignment :: SimpExpandParser AssignmentBody
parseVariableAssignment = SetVariable <$> P.choice [parseIntegerVariableAssignment]

parseIntegerVariableAssignment :: SimpExpandParser VariableAssignment
parseIntegerVariableAssignment = do
  v <- parseIntegerVariable
  skipOptionalEquals
  IntegerVariableAssignment v <$> parseNumber

parseIntegerVariable :: SimpExpandParser IntegerVariable
parseIntegerVariable = P.choice [parseIntegerParameter]
  where
    parseIntegerParameter =
      IntegerParameter <$> satisfyThen tokToIntParam
      where
        tokToIntParam (R.IntegerParameter p) = Just p
        tokToIntParam _ = Nothing
