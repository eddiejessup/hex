{-# LANGUAGE RankNTypes #-}

module HeX.Parse.Assignment where

import           HeXlude

import           Control.Monad        ( when )
import qualified Control.Monad.Combinators as PC
import           Control.Monad.Except ( runExceptT )
import           Control.Monad.Reader ( runReaderT )
import           Data.Functor         ( ($>) )
import qualified Path

import           HeX.Evaluate
import qualified HeX.Lex              as Lex
import           HeX.Parse.AST
import           HeX.Parse.Inhibited
import           HeX.Parse.Quantity
import qualified HeX.Parse.Token      as T

parseAssignment :: TeXPrimParser s Assignment
parseAssignment = parseDefineMacro <|> parseNonMacroAssignment

-- Parse Macro.
parseDefineMacro :: TeXPrimParser s Assignment
parseDefineMacro = do
    -- Macro prefixes.
    prefixes <- PC.many $
        satisfyThen $
        \case
            T.AssignPrefixTok t -> Just t
            _ -> Nothing
    -- \def-like thing.
    (defGlobalType, defExpandType) <- satisfyThen $
        \case
            T.DefineMacroTok _global expand -> Just (_global, expand)
            _ -> Nothing

    -- Macro's name.
    cs <- parseCSName
    -- Parameter text.
    (preParamTokens, parameters) <- parseParamText

    when (defExpandType == T.ExpandDef) $ panic "Not implemented: ExpandDef"

    -- Replacement text.
    replacementTokens <- parseMacroText
    let tgt = T.MacroContents { T.preParamTokens = preParamTokens
                              , T.parameters = parameters
                              , T.replacementTokens = replacementTokens
                              , T.long = T.LongTok `elem` prefixes
                              , T.outer = T.OuterTok `elem` prefixes
                              }
    pure Assignment { body   = DefineControlSequence cs (MacroTarget tgt)
                    , global = case defGlobalType of
                          T.Global -> T.Global
                          T.Local
                              | T.GlobalTok `elem` prefixes -> T.Global
                          _        -> T.Local
                    }

parseNonMacroAssignment :: TeXPrimParser s Assignment
parseNonMacroAssignment = do
    _global <- parseGlobal
    _body <- parseNonMacroAssignmentBody
    pure $ Assignment _body _global
  where
    parseGlobal = do
        gs <- PC.many $ skipSatisfiedEquals $ T.AssignPrefixTok T.GlobalTok
        pure $
            case gs of
                [] -> T.Local
                _  -> T.Global

    parseNonMacroAssignmentBody =
        PC.choice [ SetVariable <$> parseVariableAssignment
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
                  , skipSatisfiedEquals T.HyphenationTok
                        >> (SetHyphenation <$> parseGeneralText)
                  , skipSatisfiedEquals T.HyphenationPatternsTok
                        >> (SetHyphenationPatterns <$> parseGeneralText)
                  , parseSetBoxDimension
                  , SetInteractionMode <$> satisfyThen tokToInteractionMode
                  ]

    tokToInteractionMode (T.InteractionModeTok m) = Just m
    tokToInteractionMode _ = Nothing

numVarValPair :: TeXPrimStream s => (SParser s TeXStreamM TeXIntVariable, SParser s TeXStreamM TeXInt)
numVarValPair = (parseTeXIntVariable, parseTeXInt)

lenVarValPair :: TeXPrimStream s => (SParser s TeXStreamM LengthVariable, SParser s TeXStreamM Length)
lenVarValPair = (parseLengthVariable, parseLength)

glueVarValPair :: TeXPrimStream s => (SParser s TeXStreamM GlueVariable, SParser s TeXStreamM Glue)
glueVarValPair = (parseGlueVariable, parseGlue)

mathGlueVarValPair :: TeXPrimStream s => (SParser s TeXStreamM MathGlueVariable, SParser s TeXStreamM MathGlue)
mathGlueVarValPair = (parseMathGlueVariable, parseMathGlue)

tokenListVarValPair :: TeXPrimStream s => (SParser s TeXStreamM TokenListVariable, SParser s TeXStreamM TokenListAssignmentTarget)
tokenListVarValPair = (parseTokenListVariable, TokenListAssignmentText <$> parseGeneralText)

parseVarEqVal :: TeXPrimStream s
              => (SParser s TeXStreamM a, SParser s TeXStreamM b)
              -> (a -> b -> c)
              -> SParser s TeXStreamM c
parseVarEqVal (varParser, valParser) f =
    f <$> varParser <* skipOptionalEquals <*> valParser

parseVariableAssignment :: TeXPrimParser s VariableAssignment
parseVariableAssignment =
    PC.choice [ parseVarEqVal numVarValPair TeXIntVariableAssignment
              , parseVarEqVal lenVarValPair LengthVariableAssignment
              , parseVarEqVal glueVarValPair GlueVariableAssignment
              , parseVarEqVal mathGlueVarValPair MathGlueVariableAssignment
              , parseVarEqVal tokenListVarValPair TokenListVariableAssignment
              , TokenListVariableAssignment <$> parseTokenListVariable
                    <* skipFiller
                    <*> (TokenListAssignmentVar <$> parseTokenListVariable)
                -- Unofficial variable assignments, separated because of being
                -- global in the TeXbook.
              , parseVarEqVal (parseSpecialTeXInt, parseTeXInt)
                              SpecialTeXIntVariableAssignment
              , parseVarEqVal (parseSpecialLength, parseLength)
                              SpecialLengthVariableAssignment
              ]

parseVariableModification :: forall s. TeXPrimParser s VariableModification
parseVariableModification =
    PC.choice [ parseAdvanceVar numVarValPair AdvanceTeXIntVariable
              , parseAdvanceVar lenVarValPair AdvanceLengthVariable
              , parseAdvanceVar glueVarValPair AdvanceGlueVariable
              , parseAdvanceVar mathGlueVarValPair AdvanceMathGlueVariable
              , parseScaleVar
              ]
  where
    parseAdvanceVar :: (SParser s TeXStreamM a, SParser s TeXStreamM b)
                    -> (a -> b -> VariableModification)
                    -> SParser s TeXStreamM VariableModification
    parseAdvanceVar (varParser, valParser) f = do
        skipSatisfiedEquals T.AdvanceVarTok
        var <- varParser
        skipOptionalBy
        f var <$> valParser

    parseScaleVar = do
        d <- satisfyThen $
            \case
                T.ScaleVarTok d -> Just d
                _ -> Nothing
        var <- parseNumericVariable
        skipOptionalBy
        ScaleVariable d var <$> parseTeXInt

    skipOptionalBy = (parseOptionalKeyword "by" $> ()) <|> skipOptionalSpaces

    parseNumericVariable =
        PC.choice [ TeXIntNumericVariable <$> parseTeXIntVariable
                  , LengthNumericVariable <$> parseLengthVariable
                  , GlueNumericVariable <$> parseGlueVariable
                  , MathGlueNumericVariable <$> parseMathGlueVariable
                  ]

parseCodeAssignment :: TeXPrimParser s CodeAssignment
parseCodeAssignment =
    parseVarEqVal (parseCodeTableRef, parseTeXInt) CodeAssignment

parseLet :: TeXPrimParser s AssignmentBody
parseLet = do
    skipSatisfiedEquals T.LetTok
    (cs, tok) <- parseVarEqVal (parseCSName, parseLetArg) (,)
    pure $ DefineControlSequence cs (LetTarget tok)

parseFutureLet :: TeXPrimParser s AssignmentBody
parseFutureLet = do
    skipSatisfiedEquals T.FutureLetTok
    DefineControlSequence <$> parseCSName <*> (FutureLetTarget <$> parseLexToken <*> parseLexToken)

parseShortMacroAssignment :: TeXPrimParser s AssignmentBody
parseShortMacroAssignment = do
    quant <- satisfyThen $
        \case
            T.ShortDefHeadTok t -> Just t
            _ -> Nothing
    (cs, n) <- parseVarEqVal (parseCSName, parseTeXInt) (,)
    pure $ DefineControlSequence cs (ShortDefineTarget quant n)

parseSetFamilyMember :: TeXPrimParser s AssignmentBody
parseSetFamilyMember =
    parseVarEqVal (parseFamilyMember, parseFontRef) SetFamilyMember

parseSetParShape :: TeXPrimParser s AssignmentBody
parseSetParShape = do
    skipSatisfiedEquals T.ParagraphShapeTok
    skipOptionalEquals
    -- In a ⟨shape assignment⟩ for which the ⟨number⟩ is n, the ⟨shape
    -- dimensions⟩ are ⟨empty⟩ if n ≤ 0, otherwise they consist of 2n
    -- consecutive occurrences of ⟨dimen⟩
    nrPairs <- parseTeXInt
    stream <- getInput
    eNrPairs
        <- runExceptT (runReaderT (texEvaluate nrPairs) (getConfig stream))
        >>= \case
            Left err  -> throwError $ ErrorWhileTaking err
            Right v -> pure v
    SetParShape <$> PC.count eNrPairs parseLengthPair
  where
    parseLengthPair = (,) <$> parseLength <*> parseLength

parseReadToControlSequence :: TeXPrimParser s AssignmentBody
parseReadToControlSequence = do
    skipSatisfiedEquals T.ReadTok
    nr <- parseTeXInt
    skipKeyword "to"
    skipOptionalSpaces
    cs <- parseCSName
    pure $ DefineControlSequence cs (ReadTarget nr)

parseSetBoxRegister :: TeXPrimParser s AssignmentBody
parseSetBoxRegister = do
    skipSatisfiedEquals T.SetBoxRegisterTok
    parseVarEqVal (parseEightBitTeXInt, skipFiller >> parseBox) SetBoxRegister

-- <file name> = <optional spaces> <some explicit letter or digit characters> <space>
parseFileName :: TeXPrimParser s TeXFilePath
parseFileName = do
    skipOptionalSpaces
    fileNameChars <- PC.some $ satisfyThen $ \case
        T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Lex.Letter)) ->
            Just c
        T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Lex.Other)) | isValidOther c ->
            Just c
        _ -> Nothing
    skipSatisfied isSpace
    case Path.parseRelFile fileNameChars of
        Just p -> pure $ TeXFilePath p
        Nothing -> panic $ show fileNameChars

  where
    isValidOther = \case
        -- 'Other' Characters for decimal digits are OK.
        '0' -> True
        '1' -> True
        '2' -> True
        '3' -> True
        '4' -> True
        '5' -> True
        '6' -> True
        '7' -> True
        '8' -> True
        '9' -> True
        -- Not in the spec, but let's say these are OK.
        '/' -> True
        '.' -> True
        '_' -> True
        _ -> False

-- \font <control-sequence> <equals> <file-name> <at-clause>
parseNewFontAssignment :: TeXPrimParser s AssignmentBody
parseNewFontAssignment = do
    skipSatisfiedEquals T.FontTok
    cs <- parseCSName
    skipOptionalEquals
    fname <- parseFileName
    fontSpec <- parseFontSpecification
    pure $ DefineControlSequence cs (FontTarget fontSpec fname)
  where
    parseFontSpecification =
        PC.choice [ parseFontSpecAt
                  , parseFontSpecScaled
                  , skipOptionalSpaces $> NaturalFont
                  ]

    parseFontSpecAt = skipKeyword "at" >> (FontAt <$> parseLength)

    parseFontSpecScaled = skipKeyword "scaled" >> (FontScaled <$> parseTeXInt)

parseSetFontDimension :: TeXPrimParser s AssignmentBody
parseSetFontDimension =
    parseVarEqVal (parseFontDimensionRef, parseLength) SetFontDimension

parseSetFontChar :: TeXPrimParser s AssignmentBody
parseSetFontChar = parseVarEqVal (parseFontCharRef, parseTeXInt) SetFontChar

parseSetBoxDimension :: TeXPrimParser s AssignmentBody
parseSetBoxDimension =
    parseVarEqVal (parseBoxDimensionRef, parseLength) SetBoxDimension
