{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module HeX.Parse.Assignment where

import           Control.Monad                  ( when )
import           Control.Monad.Except           ( runExceptT )
import           Control.Monad.State.Lazy       ( runStateT
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
import           HeX.Parse.Quantity
import           HeX.Parse.Stream
import qualified HeX.Parse.Token               as T

type AssignmentParser = SimpExpandParser Assignment

parseAssignment :: AssignmentParser
-- 'Try' because both can start with 'global'.
parseAssignment = (P.try parseDefineMacro) <|> parseNonMacroAssignment

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
                 , skipSatisfiedEquals T.HyphenationTok >> (SetHyphenation <$> parseGeneralText)
                 , skipSatisfiedEquals T.HyphenationPatternsTok >> (SetHyphenationPatterns <$> parseGeneralText)
                 , parseSetBoxDimension
                 , SetInteractionMode <$> satisfyThen tokToInteractionMode
                 , parseSetSpecialInteger
                 , parseSetSpecialLength
                 ]

    tokToInteractionMode (T.InteractionModeTok m) = Just m
    tokToInteractionMode _                        = Nothing

numVarValPair :: (SimpExpandParser IntegerVariable, SimpExpandParser Number)
numVarValPair = (parseIntegerVariable, parseNumber)

lenVarValPair :: (SimpExpandParser LengthVariable, SimpExpandParser Length)
lenVarValPair = (parseLengthVariable, parseLength)

glueVarValPair :: (SimpExpandParser GlueVariable, SimpExpandParser Glue)
glueVarValPair = (parseGlueVariable, parseGlue)

mathGlueVarValPair :: (SimpExpandParser MathGlueVariable, SimpExpandParser MathGlue)
mathGlueVarValPair = (parseMathGlueVariable, parseMathGlue)

tokenListVarValPair :: (SimpExpandParser TokenListVariable, SimpExpandParser BalancedText)
tokenListVarValPair = (parseTokenListVariable, parseGeneralText)

parseVarEqVal :: (SimpExpandParser a, SimpExpandParser b)
              -> (a -> b -> c)
              -> SimpExpandParser c
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
        f var <$> valParser

    parseScaleVar =
        do
        d <- satisfyThen (\case
            T.ScaleVarTok d -> Just d
            _               -> Nothing)
        var <- parseNumericVariable
        skipOptionalBy
        ScaleVariable d var <$> parseNumber

    skipOptionalBy = (parseOptionalKeyword "by" $> ()) <|> skipOptionalSpaces

    parseNumericVariable = P.choice [ IntegerNumericVariable <$> parseIntegerVariable
                                    , LengthNumericVariable <$> parseLengthVariable
                                    , GlueNumericVariable <$> parseGlueVariable
                                    , MathGlueNumericVariable <$> parseMathGlueVariable
                                    ]

parseCodeAssignment :: SimpExpandParser CodeAssignment
parseCodeAssignment =
    parseVarEqVal (parseCodeTableRef, parseNumber) CodeAssignment

parseLet :: SimpExpandParser AssignmentBody
parseLet =
    do
    skipSatisfiedEquals T.LetTok
    (cs, tok) <- parseVarEqVal (parseCSName, skipOneOptionalSpace >> parseToken) (,)
    pure $ DefineControlSequence cs (LetTarget tok)

parseFutureLet :: SimpExpandParser AssignmentBody
parseFutureLet =
    do
    skipSatisfiedEquals T.FutureLetTok
    cs <- parseCSName
    tok1 <- parseToken
    tok2 <- parseToken
    pure $ DefineControlSequence cs (FutureLetTarget tok1 tok2)

parseShortMacroAssignment :: SimpExpandParser AssignmentBody
parseShortMacroAssignment =
    do
    quant <- satisfyThen (\case
        T.ShortDefHeadTok t -> Just t
        _                   -> Nothing)
    (cs, n) <- parseVarEqVal (parseCSName, parseNumber) (,)
    pure $ DefineControlSequence cs (ShortDefineTarget quant n)

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
    stream <- P.stateInput <$> P.getParserState
    (eNrPairs, conf') <-
        runExceptT (runStateT (evaluateNumber nrPairs) (config stream))
            >>= \case
                Left _ -> P.failure Nothing Set.empty
                Right v -> pure v
    P.updateParserState (\state -> state{P.stateInput=stream{config=conf'}})
    SetParShape <$> P.count eNrPairs parseLengthPair
  where
    parseLengthPair = (,) <$> parseLength <*> parseLength

parseReadToControlSequence :: SimpExpandParser AssignmentBody
parseReadToControlSequence =
    do
    skipSatisfiedEquals T.ReadTok
    nr <- parseNumber
    skipKeyword "to"
    skipOptionalSpaces
    cs <- parseCSName
    pure $ DefineControlSequence cs (ReadTarget nr)

parseSetBoxRegister :: SimpExpandParser AssignmentBody
parseSetBoxRegister =
    do
    skipSatisfiedEquals T.SetBoxRegisterTok
    parseVarEqVal (parseNumber, skipFiller >> parseBox) SetBoxRegister

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
  where
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

-- \font <control-sequence> <equals> <file-name> <at-clause>
parseNewFontAssignment :: SimpExpandParser AssignmentBody
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

parseSetFontDimension :: SimpExpandParser AssignmentBody
parseSetFontDimension =
    parseVarEqVal (parseFontDimensionRef, parseLength) SetFontDimension

parseSetFontChar :: SimpExpandParser AssignmentBody
parseSetFontChar =
    parseVarEqVal (parseFontCharRef, parseNumber) SetFontChar

parseSetBoxDimension :: SimpExpandParser AssignmentBody
parseSetBoxDimension =
    parseVarEqVal (parseBoxDimensionRef, parseLength) SetBoxDimension

parseSetSpecialInteger :: SimpExpandParser AssignmentBody
parseSetSpecialInteger =
    parseVarEqVal (parseSpecialInteger, parseNumber) SetSpecialInteger

parseSetSpecialLength :: SimpExpandParser AssignmentBody
parseSetSpecialLength =
    parseVarEqVal (parseSpecialLength, parseLength) SetSpecialLength
