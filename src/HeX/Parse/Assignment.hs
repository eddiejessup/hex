{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module HeX.Parse.Assignment where

import           Control.Monad                  ( when )
import           Control.Monad.Except           ( runExceptT )
import           Control.Monad.Reader           ( runReaderT
                                                )
import           Data.Functor                   ( ($>) )
import qualified Data.Set                      as Set

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
import           HeX.Parse.Inhibited
import           HeX.Parse.Quantity
import qualified HeX.Parse.Token               as T

parseAssignment :: InhibitableStream s => SimpParser s Assignment
-- 'Try' because both can start with 'global'.
parseAssignment = (P.try parseDefineMacro) <|> parseNonMacroAssignment

-- Parse Macro.

parseDefineMacro :: InhibitableStream s => SimpParser s Assignment
parseDefineMacro =
    do
    -- Macro prefixes.
    prefixes <- P.many $ satisfyThen $ \case
        T.AssignPrefixTok t -> Just t
        _                   -> Nothing
    -- \def-like thing.
    (defGlobalType, defExpandType) <- satisfyThen $ \case
        T.DefineMacroTok _global expand -> Just (_global, expand)
        _                               -> Nothing

    -- Macro's name.
    cs <- parseCSName
    -- Parameter text.
    (preParamTokens, parameters) <- parseParamText
    -- TODO: Support expanded-def.
    when (defExpandType == T.ExpandDef) $ error "expanded-def not implemented"
    -- Replacement text.
    replacementTokens <- parseMacroText
    let tgt = T.MacroContents
            { preParamTokens = preParamTokens
            , parameters = parameters
            , replacementTokens = replacementTokens
            , long = T.LongTok `elem` prefixes
            , outer = T.OuterTok `elem` prefixes
            }
    pure Assignment
        { body = DefineControlSequence cs (MacroTarget tgt)
        , global = case defGlobalType of
            T.Global                              -> T.Global
            T.Local | T.GlobalTok `elem` prefixes -> T.Global
            _                                     -> T.Local
        }

parseNonMacroAssignment :: InhibitableStream s => SimpParser s Assignment
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
                 , skipSatisfiedEquals T.HyphenationTok >> (SetHyphenation <$> parseGeneralText)
                 , skipSatisfiedEquals T.HyphenationPatternsTok >> (SetHyphenationPatterns <$> parseGeneralText)
                 , parseSetBoxDimension
                 , SetInteractionMode <$> satisfyThen tokToInteractionMode
                 ]

    tokToInteractionMode (T.InteractionModeTok m) = Just m
    tokToInteractionMode _                        = Nothing

numVarValPair :: InhibitableStream s => (SimpParser s IntegerVariable, SimpParser s Number)
numVarValPair = (parseIntegerVariable, parseNumber)

lenVarValPair :: InhibitableStream s => (SimpParser s LengthVariable, SimpParser s Length)
lenVarValPair = (parseLengthVariable, parseLength)

glueVarValPair :: InhibitableStream s => (SimpParser s GlueVariable, SimpParser s Glue)
glueVarValPair = (parseGlueVariable, parseGlue)

mathGlueVarValPair :: InhibitableStream s => (SimpParser s MathGlueVariable, SimpParser s MathGlue)
mathGlueVarValPair = (parseMathGlueVariable, parseMathGlue)

tokenListVarValPair :: InhibitableStream s => (SimpParser s TokenListVariable, SimpParser s TokenListAssignmentTarget)
tokenListVarValPair = (parseTokenListVariable, TokenListAssignmentText <$> parseGeneralText)

parseVarEqVal :: InhibitableStream s => (SimpParser s a, SimpParser s b)
              -> (a -> b -> c)
              -> SimpParser s c
parseVarEqVal (varParser, valParser) f =
    f <$> varParser <* skipOptionalEquals <*> valParser

parseVariableAssignment :: InhibitableStream s => SimpParser s VariableAssignment
parseVariableAssignment =
    P.choice [ parseVarEqVal numVarValPair IntegerVariableAssignment
             , parseVarEqVal lenVarValPair LengthVariableAssignment
             , parseVarEqVal glueVarValPair GlueVariableAssignment
             , parseVarEqVal mathGlueVarValPair MathGlueVariableAssignment
             , parseVarEqVal tokenListVarValPair TokenListVariableAssignment
             , TokenListVariableAssignment <$> parseTokenListVariable <* skipFiller <*> (TokenListAssignmentVar <$> parseTokenListVariable)

             -- Unofficial variable assignments, separated because of being
             -- global in the TeXbook.
             , parseVarEqVal (parseSpecialInteger, parseNumber) SpecialIntegerVariableAssignment
             , parseVarEqVal (parseSpecialLength, parseLength) SpecialLengthVariableAssignment
             ]

parseVariableModification :: forall s. InhibitableStream s => SimpParser s VariableModification
parseVariableModification = P.choice [ parseAdvanceVar numVarValPair AdvanceIntegerVariable
                                     , parseAdvanceVar lenVarValPair AdvanceLengthVariable
                                     , parseAdvanceVar glueVarValPair AdvanceGlueVariable
                                     , parseAdvanceVar mathGlueVarValPair AdvanceMathGlueVariable
                                     , parseScaleVar
                                     ]
  where
    parseAdvanceVar :: InhibitableStream s => (SimpParser s a, SimpParser s b) -> (a -> b -> VariableModification) -> SimpParser s VariableModification
    parseAdvanceVar (varParser, valParser) f =
        do
        skipSatisfiedEquals T.AdvanceVarTok
        var <- varParser
        skipOptionalBy
        f var <$> valParser

    parseScaleVar =
        do
        d <- satisfyThen $ \case
            T.ScaleVarTok d -> Just d
            _               -> Nothing
        var <- parseNumericVariable
        skipOptionalBy
        ScaleVariable d var <$> parseNumber

    skipOptionalBy = (parseOptionalKeyword "by" $> ()) <|> skipOptionalSpaces

    parseNumericVariable = P.choice [ IntegerNumericVariable <$> parseIntegerVariable
                                    , LengthNumericVariable <$> parseLengthVariable
                                    , GlueNumericVariable <$> parseGlueVariable
                                    , MathGlueNumericVariable <$> parseMathGlueVariable
                                    ]

parseCodeAssignment :: InhibitableStream s => SimpParser s CodeAssignment
parseCodeAssignment =
    parseVarEqVal (parseCodeTableRef, parseNumber) CodeAssignment

parseLet :: InhibitableStream s => SimpParser s AssignmentBody
parseLet =
    do
    skipSatisfiedEquals T.LetTok
    (cs, tok) <- parseVarEqVal (parseCSName, parseLetArg) (,)
    pure $ DefineControlSequence cs (LetTarget tok)

parseFutureLet :: InhibitableStream s => SimpParser s AssignmentBody
parseFutureLet =
    do
    skipSatisfiedEquals T.FutureLetTok
    cs <- parseCSName
    tok1 <- parseLexToken
    tok2 <- parseLexToken
    pure $ DefineControlSequence cs (FutureLetTarget tok1 tok2)

parseShortMacroAssignment :: InhibitableStream s => SimpParser s AssignmentBody
parseShortMacroAssignment =
    do
    quant <- satisfyThen $ \case
        T.ShortDefHeadTok t -> Just t
        _                   -> Nothing
    (cs, n) <- parseVarEqVal (parseCSName, parseNumber) (,)
    pure $ DefineControlSequence cs (ShortDefineTarget quant n)

parseSetFamilyMember :: InhibitableStream s => SimpParser s AssignmentBody
parseSetFamilyMember =
    parseVarEqVal (parseFamilyMember, parseFontRef) SetFamilyMember

parseSetParShape :: InhibitableStream s => SimpParser s AssignmentBody
parseSetParShape =
    do
    skipSatisfiedEquals T.ParagraphShapeTok
    skipOptionalEquals
    -- In a ⟨shape assignment⟩ for which the ⟨number⟩ is n, the ⟨shape
    -- dimensions⟩ are ⟨empty⟩ if n ≤ 0, otherwise they consist of 2n
    -- consecutive occurrences of ⟨dimen⟩
    nrPairs <- parseNumber
    stream <- P.getInput
    eNrPairs <-
        runExceptT (runReaderT (evaluateNumber nrPairs) (getConfig stream))
            >>= \case
                Left _ -> P.failure Nothing Set.empty
                Right v -> pure v
    SetParShape <$> P.count eNrPairs parseLengthPair
  where
    parseLengthPair = (,) <$> parseLength <*> parseLength

parseReadToControlSequence :: InhibitableStream s => SimpParser s AssignmentBody
parseReadToControlSequence =
    do
    skipSatisfiedEquals T.ReadTok
    nr <- parseNumber
    skipKeyword "to"
    skipOptionalSpaces
    cs <- parseCSName
    pure $ DefineControlSequence cs (ReadTarget nr)

parseSetBoxRegister :: InhibitableStream s => SimpParser s AssignmentBody
parseSetBoxRegister =
    do
    skipSatisfiedEquals T.SetBoxRegisterTok
    parseVarEqVal (parseNumber, skipFiller >> parseBox) SetBoxRegister

-- <file name> = <optional spaces> <some explicit letter or digit characters> <space>
parseFileName :: InhibitableStream s => SimpParser s (Path Rel File)
parseFileName =
    do
    skipOptionalSpaces
    fileName <- P.some $ satisfyThen $ \case
        T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Lex.Letter)) ->
            Just c
        -- 'Other' Characters for decimal digits are OK.
        T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Lex.Other)) ->
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
        _ -> Nothing
    skipSatisfied isSpace
    case parseRelFile (fileName ++ ".tfm") of
      Just p -> pure p
      Nothing -> fail $ "Invalid filename: " ++ fileName ++ ".tfm"

-- \font <control-sequence> <equals> <file-name> <at-clause>
parseNewFontAssignment :: InhibitableStream s => SimpParser s AssignmentBody
parseNewFontAssignment =
    do
    skipSatisfiedEquals T.FontTok
    cs <- parseCSName
    skipOptionalEquals
    fname <- parseFileName
    fontSpec <- parseFontSpecification
    pure $ DefineControlSequence cs (FontTarget fontSpec fname)
  where
    parseFontSpecification = P.choice [ parseFontSpecAt
                                      , parseFontSpecScaled
                                      , skipOptionalSpaces $> NaturalFont ]

    parseFontSpecAt = skipKeyword "at" >> (FontAt <$> parseLength)
    parseFontSpecScaled = skipKeyword "scaled" >> (FontScaled <$> parseNumber)

parseSetFontDimension :: InhibitableStream s => SimpParser s AssignmentBody
parseSetFontDimension =
    parseVarEqVal (parseFontDimensionRef, parseLength) SetFontDimension

parseSetFontChar :: InhibitableStream s => SimpParser s AssignmentBody
parseSetFontChar =
    parseVarEqVal (parseFontCharRef, parseNumber) SetFontChar

parseSetBoxDimension :: InhibitableStream s => SimpParser s AssignmentBody
parseSetBoxDimension =
    parseVarEqVal (parseBoxDimensionRef, parseLength) SetBoxDimension
