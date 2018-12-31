{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module HeX.Parse.Assignment where

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
import qualified HeX.Parse.Token               as T
import           HeX.Parse.Common
import           HeX.Parse.Stream
import           HeX.Parse.VarAssignment

-- AST.

data MacroPrefix
  = Long
  | Outer
  | Global
  deriving (Eq)

data MacroAssignment
  = MacroAssignment { name :: Lex.ControlSequenceLike
                    , contents :: T.MacroContents
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
      = P.choice [ SetVariable <$> parseVariableAssignment
                 , parseTokenForFont
                 , parseMacroToFont ]

-- Parse Macro.

parseDefineMacro :: AssignmentParser
parseDefineMacro = do
  -- Macro prefixes.
  prefixes <- P.many $ satisfyThen tokToPrefix
  -- \def-like thing.
  (defGlobal, defExpand) <- satisfyThen tokToDef
  -- Macro's name.
  cs <- parseCSName
  -- Parameter text.
  (preParamToks, paramDelims) <- parseParamText
  -- TODO: Support expanded-def.
  when defExpand $ error "expanded-def not implemented"
  -- Replacement text.
  replaceToks <- parseMacroText
  pure $
    Assignment
    { body = DefineMacro $ MacroAssignment
        { name = cs
        , contents = T.MacroContents preParamToks paramDelims replaceToks
        , long = Long `elem` prefixes
        , outer = Outer `elem` prefixes
        }
    , global = defGlobal || Global `elem` prefixes
    }
  where
    -- TODO: Can avoid this manual mapping by putting it in the token.
    tokToPrefix T.GlobalTok = Just Global
    tokToPrefix T.OuterTok = Just Outer
    tokToPrefix T.LongTok = Just Long
    tokToPrefix _ = Nothing

    tokToDef T.DefineMacroTok {global = _global, expand = _expand} =
      Just (_global, _expand)
    tokToDef _ = Nothing

parseTokenForFont :: SimpExpandParser AssignmentBody
parseTokenForFont = satisfyThen tokToCom
  where
    tokToCom (T.TokenForFont n) = Just $ SelectFont n
    tokToCom _ = Nothing

-- \font <control-sequence> <equals> <file-name> <at-clause>
parseMacroToFont :: SimpExpandParser AssignmentBody
parseMacroToFont = do
  skipSatisfiedEquals T.FontTok
  cs <- parseCSName
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
    tokToChar (T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Lex.Letter))) = Just c
    -- 'Other' Characters for decimal digits are OK.
    tokToChar (T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Lex.Other))) =
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
