{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module HeX.Parse.Assignment where

import           Data.Functor                   ( ($>) )
import           Control.Monad                  ( when )
import           Path                           ( File
                                                , Path
                                                , Rel
                                                , parseRelFile
                                                )
import qualified Text.Megaparsec               as P
import           Text.Megaparsec                ( (<|>) )

import qualified HeX.Lex                       as Lex
import           HeX.Evaluate                   ( evaluateNumber )

import           HeX.Parse.Helpers
import           HeX.Parse.AST
import           HeX.Parse.Common
import           HeX.Parse.Quantity
import           HeX.Parse.Stream
import qualified HeX.Parse.Token               as T

type AssignmentParser = SimpExpandParser Assignment

parseAssignment :: AssignmentParser
-- 'Try' because both can start with 'global'.
parseAssignment = P.try parseDefineMacro <|> parseNonMacroAssignment

-- Parse Macro.

parseDefineMacro :: AssignmentParser
parseDefineMacro =
    do
    -- Macro prefixes.
    prefixes <- P.many $ satisfyThen tokToPrefix
    -- \def-like thing.
    (defGlobalType, defExpandType) <- satisfyThen tokToDef
    -- Macro's name.
    cs <- parseCSName
    -- Parameter text.
    (preParamToks, paramDelims) <- parseParamText
    -- TODO: Support expanded-def.
    when (defExpandType == T.ExpandDef) $ error "expanded-def not implemented"
    -- Replacement text.
    replaceToks <- parseMacroText
    pure Assignment
        { body = DefineMacro $ MacroAssignment
            { name = cs
            , contents = T.MacroContents preParamToks paramDelims replaceToks
            , long = T.LongTok `elem` prefixes
            , outer = T.OuterTok `elem` prefixes
            }
        , global = case defGlobalType of
            T.Global                              -> T.Global
            T.Local | T.GlobalTok `elem` prefixes -> T.Global
            _                                     -> T.Local
        }
  where
    tokToPrefix (T.AssignPrefixTok t) = Just t
    tokToPrefix _ = Nothing

    tokToDef (T.DefineMacroTok _global expand) = Just (_global, expand)
    tokToDef _ = Nothing

parseNonMacroAssignment :: AssignmentParser
parseNonMacroAssignment =
    do
    _global <- parseGlobal
    _body <- parseNonMacroAssignmentBody
    pure $ Assignment _body _global
  where
    parseGlobal =
        do
        gs <- P.many $ skipSatisfiedEquals $ T.AssignPrefixTok T.GlobalTok
        pure $ case gs of
            [] -> T.Local
            _  -> T.Global

    parseNonMacroAssignmentBody =
        P.choice [ SetVariable <$> parseVariableAssignment
                 , ModifyVariable <$> parseVariableModification
                 , AssignCode <$> parseCodeAssignment
                 , parseLet
                 , parseFutureLet
                 , parseShortMacroAssignment
                 , SelectFont <$> parseFontRefToken
                 , parseSetFamilyMember
                 , parseSetParShape
                 , parseReadToControlSequence
                 , parseSetBoxRegister
                 , parseNewFontAssignment
                 , parseSetFontDimension
                 , parseSetFontChar
                 , SetHyphenation <$> parseGeneralText
                 , SetHyphenationPatterns <$> parseGeneralText
                 , parseSetBoxDimension
                 , SetInteractionMode <$> satisfyThen tokToInteractionMode
                 , parseSetSpecialInteger
                 , parseSetSpecialLength
                 ]

    tokToInteractionMode (T.InteractionModeTok m) = Just m
    tokToInteractionMode _                        = Nothing

numVarValPair = (parseIntegerVariable, parseNumber)
lenVarValPair = (parseLengthVariable, parseLength)
glueVarValPair = (parseGlueVariable, parseGlue)
mathGlueVarValPair = (parseMathGlueVariable, parseMathGlue)
tokenListVarValPair = (parseTokenListVariable, parseGeneralText)

parseVarEqVal (varParser, valParser) f =
    f <$> varParser <* skipOptionalEquals <*> valParser

parseVariableAssignment :: SimpExpandParser VariableAssignment
parseVariableAssignment =
    P.choice [ parseVarEqVal numVarValPair IntegerVariableAssignment
             , parseVarEqVal lenVarValPair LengthVariableAssignment
             , parseVarEqVal glueVarValPair GlueVariableAssignment
             , parseVarEqVal mathGlueVarValPair MathGlueVariableAssignment
             , parseVarEqVal tokenListVarValPair TokenListVariableAssignmentText
             , TokenListVariableAssignmentVar <$> parseTokenListVariable <* skipFiller <*> parseTokenListVariable
             ]

parseVariableModification :: SimpExpandParser VariableModification
parseVariableModification = P.choice [ parseAdvanceVar numVarValPair AdvanceIntegerVariable
                                     , parseAdvanceVar lenVarValPair AdvanceLengthVariable
                                     , parseAdvanceVar glueVarValPair AdvanceGlueVariable
                                     , parseAdvanceVar mathGlueVarValPair AdvanceMathGlueVariable
                                     , parseScaleVar
                                     ]
  where
    parseAdvanceVar :: (SimpExpandParser a, SimpExpandParser b) -> (a -> b -> VariableModification) -> SimpExpandParser VariableModification
    parseAdvanceVar (varParser, valParser) f =
        do
        skipSatisfiedEquals T.AdvanceVarTok
        var <- varParser
        skipOptionalBy
        val <- valParser
        pure $ f var val

    parseScaleVar =
        do
        d <- satisfyThen (\case
            T.ScaleVarTok d -> Just d
            _               -> Nothing)
        var <- parseNumericVariable
        skipOptionalBy
        nr <- parseNumber
        pure $ ScaleVariable d var nr

    skipOptionalBy = (parseOptionalKeyword "by" $> ()) <|> skipOptionalSpaces

    parseNumericVariable = P.choice [ IntegerNumericVariable <$> parseIntegerVariable
                                    , LengthNumericVariable <$> parseLengthVariable
                                    , GlueNumericVariable <$> parseGlueVariable
                                    , MathGlueNumericVariable <$> parseMathGlueVariable
                                    ]

parseCodeAssignment :: SimpExpandParser CodeAssignment
parseCodeAssignment =
    parseVarEqVal (parseCodeTableRef, parseNumber) CodeAssignment

parseLet =
    do
    skipSatisfiedEquals T.LetTok
    parseVarEqVal (parseCSName, skipOneOptionalSpace >> parseToken) Let

parseFutureLet =
    do
    skipSatisfiedEquals T.FutureLetTok
    FutureLet <$> parseCSName <*> parseToken <*> parseToken

parseShortMacroAssignment =
    do
    quant <- satisfyThen (\case
        T.ShortDefHeadTok t -> Just t
        _                   -> Nothing)
    parseVarEqVal (parseCSName, parseNumber) $ ShortDefine quant

parseSetFamilyMember :: SimpExpandParser AssignmentBody
parseSetFamilyMember =
    parseVarEqVal (parseFamilyMember, parseFontRef) SetFamilyMember

parseSetParShape :: SimpExpandParser AssignmentBody
parseSetParShape =
    do
    skipSatisfiedEquals T.ParagraphShapeTok
    skipOptionalEquals
    -- In a ⟨shape assignment⟩ for which the ⟨number⟩ is n, the ⟨shape
    -- dimensions⟩ are ⟨empty⟩ if n ≤ 0, otherwise they consist of 2n
    -- consecutive occurrences of ⟨dimen⟩
    nrPairs <- parseNumber
    -- TODO: Not sure how to handle this, we must be able to evaluate things
    -- halfway through a command.
    let eNrPairs = evaluateNumber nrPairs
    SetParShape <$> P.count eNrPairs parseLengthPair
  where
    parseLengthPair = (,) <$> parseLength <*> parseLength

parseReadToControlSequence =
    do
    skipSatisfiedEquals T.ReadTok
    nr <- parseNumber
    skipKeyword "to"
    skipOptionalSpaces
    cs <- parseCSName
    pure $ ReadToControlSequence nr cs

parseSetBoxRegister =
    do
    skipSatisfiedEquals T.SetBoxRegisterTok
    parseVarEqVal (parseNumber, skipFiller >> parseBox) SetBoxRegister

-- \font <control-sequence> <equals> <file-name> <at-clause>
parseNewFontAssignment :: SimpExpandParser AssignmentBody
parseNewFontAssignment =
    do
    skipSatisfiedEquals T.FontTok
    cs <- parseCSName
    skipOptionalEquals
    fname <- parseFileName
    fontSpec <- parseFontSpecification
    pure $ DefineFont cs fontSpec fname
  where
    -- <file name> = <optional spaces> <some explicit letter or digit characters> <space>
    parseFileName :: SimpExpandParser (Path Rel File)
    parseFileName =
        do
        skipOptionalSpaces
        fileName <- P.some $ satisfyThen tokToPathChar
        skipSatisfied isSpace
        case parseRelFile (fileName ++ ".tfm") of
          Just p -> pure p
          Nothing -> fail $ "Invalid filename: " ++ fileName ++ ".tfm"

    tokToPathChar (T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Lex.Letter))) =
        Just c
    -- 'Other' Characters for decimal digits are OK.
    tokToPathChar (T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Lex.Other))) =
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
    tokToPathChar _ = Nothing

    parseFontSpecification = P.choice [ parseFontSpecAt
                                      , parseFontSpecScaled
                                      , skipOptionalSpaces $> NaturalFont ]

    parseFontSpecAt = skipKeyword "at" >> (FontAt <$> parseLength)
    parseFontSpecScaled = skipKeyword "scaled" >> (FontScaled <$> parseNumber)

parseSetFontDimension =
    parseVarEqVal (parseFontDimensionRef, parseLength) SetFontDimension

parseSetFontChar =
    parseVarEqVal (parseFontCharRef, parseNumber) SetFontChar

parseSetBoxDimension =
    parseVarEqVal (parseBoxDimensionRef, parseLength) SetBoxDimension

parseSetSpecialInteger =
    parseVarEqVal (parseSpecialInteger, parseNumber) SetSpecialInteger

parseSetSpecialLength =
    parseVarEqVal (parseSpecialLength, parseLength) SetSpecialLength
