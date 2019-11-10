{-# LANGUAGE RankNTypes #-}

module HeX.Parse.Assignment where

import           HeXlude

import           Control.Monad             (when)
import qualified Control.Monad.Combinators as PC
import           Control.Monad.Reader      (runReaderT)
import           Data.Functor              (($>))
import qualified Path
import qualified Text.Megaparsec           as P

import           HeX.Evaluate
import           HeX.Config.Codes          (codesFromStr)
import qualified HeX.Config.Codes          as Code
import qualified HeX.Lex                   as Lex
import           HeX.Parse.AST
import           HeX.Parse.Quantity
import           HeX.Parse.Stream.Class
import qualified HeX.Parse.Token           as T
import qualified HeX.Quantity              as Q

parseAssignment :: TeXParser s e m Assignment
parseAssignment = PC.choice [parseDefineMacro, parseNonMacroAssignment]

-- Parse Macro.
parseDefineMacro :: TeXParser s e m Assignment
parseDefineMacro = do
    -- Macro prefixes.
    prefixes <- PC.many $ satisfyThen $ \case
        T.AssignPrefixTok t -> Just t
        _ -> Nothing
    -- \def-like thing.
    (defGlobalType, defExpandType) <- satisfyThen $ \case
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
                        T.Global ->
                            T.Global
                        T.Local | T.GlobalTok `elem` prefixes ->
                            T.Global
                        _ ->
                            T.Local
                    }

parseNonMacroGlobal :: TeXParser s e m T.GlobalFlag
parseNonMacroGlobal = PC.option T.Local $ satisfyEquals (T.AssignPrefixTok T.GlobalTok) $> T.Global

parseNonMacroAssignment :: TeXParser s e m Assignment
parseNonMacroAssignment =
    flip Assignment <$> parseNonMacroGlobal <*> parseNonMacroAssignmentBody
  where
    parseNonMacroAssignmentBody = PC.choice
        [ AssignCode <$> parseCodeAssignment
        , ModifyVariable <$> parseVariableModification
        , SetVariable <$> parseVariableAssignment
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
        , satisfyEquals T.HyphenationTok
              >> (SetHyphenation <$> parseGeneralText)
        , satisfyEquals T.HyphenationPatternsTok
              >> (SetHyphenationPatterns <$> parseGeneralText)
        , parseSetBoxDimension
        , SetInteractionMode <$> satisfyThen tokToInteractionMode
        ]

    tokToInteractionMode (T.InteractionModeTok m) = Just m
    tokToInteractionMode _                        = Nothing

numVarValPair
    :: ( TeXParseable s e m
       )
    => (SimpleParsecT s m TeXIntVariable, SimpleParsecT s m TeXInt)
numVarValPair = (parseTeXIntVariable, parseTeXInt)

lenVarValPair
    :: ( TeXParseable s e m
       )
    => (SimpleParsecT s m LengthVariable, SimpleParsecT s m Length)
lenVarValPair = (parseLengthVariable, parseLength)

glueVarValPair
    :: ( TeXParseable s e m
       )
    => (SimpleParsecT s m GlueVariable, SimpleParsecT s m Glue)
glueVarValPair = (parseGlueVariable, parseGlue)

mathGlueVarValPair
    :: ( TeXParseable s e m
       )
    => (SimpleParsecT s m MathGlueVariable, SimpleParsecT s m MathGlue)
mathGlueVarValPair = (parseMathGlueVariable, parseMathGlue)

tokenListVarValPair
    :: ( TeXParseable s e m
       )
    => (SimpleParsecT s m TokenListVariable, SimpleParsecT s m TokenListAssignmentTarget)
tokenListVarValPair = (parseTokenListVariable, TokenListAssignmentText <$> parseGeneralText)

parseVarEqVal
    :: ( TeXParseable s e m
       )
    => (SimpleParsecT s m a, SimpleParsecT s m b)
    -> (a -> b -> c)
    -> SimpleParsecT s m c
parseVarEqVal (varParser, valParser) f =
    f <$> varParser <* skipOptionalEquals <*> valParser

parseVariableAssignment :: TeXParser s e m VariableAssignment
parseVariableAssignment =
    PC.choice
        [ parseVarEqVal numVarValPair TeXIntVariableAssignment
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

parseVariableModification :: forall s e m. TeXParser s e m VariableModification
parseVariableModification =
    PC.choice
        [ parseAdvanceVar
        , parseScaleVar
        ]
  where
    parseAdvanceVar =
        do
        satisfyEquals T.AdvanceVarTok
        PC.choice
            [ parseModVarArgs numVarValPair AdvanceTeXIntVariable
            , parseModVarArgs lenVarValPair AdvanceLengthVariable
            , parseModVarArgs glueVarValPair AdvanceGlueVariable
            , parseModVarArgs mathGlueVarValPair AdvanceMathGlueVariable
            ]

    parseScaleVar = do
        d <- satisfyThen $ \case
            T.ScaleVarTok d -> Just d
            _ -> Nothing
        parseModVarArgs (parseNumericVariable, parseTeXInt) (ScaleVariable d)

    parseModVarArgs
        :: (SimpleParsecT s m a, SimpleParsecT s m b)
        -> (a -> b -> VariableModification)
        -> SimpleParsecT s m VariableModification
    parseModVarArgs (varParser, valParser) f = do
        var <- varParser
        skipOptionalBy
        f var <$> valParser

    skipOptionalBy = PC.choice [void (parseOptionalKeyword (codesFromStr "by")), skipOptionalSpaces]

    parseNumericVariable =
        PC.choice
            [ TeXIntNumericVariable <$> parseTeXIntVariable
            , LengthNumericVariable <$> parseLengthVariable
            , GlueNumericVariable <$> parseGlueVariable
            , MathGlueNumericVariable <$> parseMathGlueVariable
            ]

parseCodeAssignment :: TeXParser s e m CodeAssignment
parseCodeAssignment =
    parseVarEqVal (parseCodeTableRef, parseTeXInt) CodeAssignment

parseLet :: TeXParser s e m AssignmentBody
parseLet = do
    satisfyEquals T.LetTok
    (cs, tok) <- parseVarEqVal (parseCSName, parseLetArg) (,)
    pure $ DefineControlSequence cs (LetTarget tok)

parseFutureLet :: TeXParser s e m AssignmentBody
parseFutureLet = do
    satisfyEquals T.FutureLetTok
    DefineControlSequence <$> parseCSName <*> (FutureLetTarget <$> parseLexToken <*> parseLexToken)

parseShortMacroAssignment :: TeXParser s e m AssignmentBody
parseShortMacroAssignment = do
    quant <- satisfyThen $
        \case
            T.ShortDefHeadTok t -> Just t
            _ -> Nothing
    (cs, n) <- parseVarEqVal (parseCSName, parseTeXInt) (,)
    pure $ DefineControlSequence cs (ShortDefineTarget quant n)

parseSetFamilyMember :: TeXParser s e m AssignmentBody
parseSetFamilyMember =
    parseVarEqVal (parseFamilyMember, parseFontRef) SetFamilyMember

parseSetParShape :: TeXParser s e m AssignmentBody
parseSetParShape = do
    satisfyEquals T.ParagraphShapeTok
    skipOptionalEquals
    -- In a ⟨shape assignment⟩ for which the ⟨number⟩ is n, the ⟨shape
    -- dimensions⟩ are ⟨empty⟩ if n ≤ 0, otherwise they consist of 2n
    -- consecutive occurrences of ⟨dimen⟩
    nrPairs <- parseTeXInt
    (Q.TeXInt eNrPairsInt) <- P.getInput <&> getConfig >>= runReaderT (texEvaluate nrPairs)
    SetParShape <$> PC.count eNrPairsInt parseLengthPair
  where
    parseLengthPair = (,) <$> parseLength <*> parseLength

parseReadToControlSequence :: TeXParser s e m AssignmentBody
parseReadToControlSequence = do
    satisfyEquals T.ReadTok
    nr <- parseTeXInt
    skipKeyword (codesFromStr "to")
    skipOptionalSpaces
    cs <- parseCSName
    pure $ DefineControlSequence cs (ReadTarget nr)

parseSetBoxRegister :: TeXParser s e m AssignmentBody
parseSetBoxRegister = do
    satisfyEquals T.SetBoxRegisterTok
    parseVarEqVal (parseEightBitTeXInt, skipFiller >> parseBox) SetBoxRegister

-- <file name> = <optional spaces> <some explicit letter or digit characters> <space>
parseFileName :: TeXParser s e m TeXFilePath
parseFileName = do
    skipOptionalSpaces
    fileNameChars <- PC.some $ satisfyThen $ \case
        T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Code.Letter)) ->
            Just c
        T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Code.Other)) | isValidOther c ->
            Just c
        _ -> Nothing
    skipSatisfied isSpace
    case Path.parseRelFile (Code.codeAsChar <$> fileNameChars) of
        Just p  -> pure $ TeXFilePath p
        Nothing -> panic $ show fileNameChars

  where
    isValidOther = \case
        -- Not in the spec, but let's say these are OK.
        Code.CharCode_ '/' -> True
        Code.CharCode_ '.'-> True
        Code.CharCode_ '_' -> True
        -- 'Other' Characters for decimal digits are OK.
        cc -> Code.isDigitChar cc

-- \font <control-sequence> <equals> <file-name> <at-clause>
parseNewFontAssignment :: TeXParser s e m AssignmentBody
parseNewFontAssignment = do
    satisfyEquals T.FontTok
    cs <- parseCSName
    skipOptionalEquals
    fname <- parseFileName
    fontSpec <- parseFontSpecification
    pure $ DefineControlSequence cs (FontTarget fontSpec fname)
  where
    parseFontSpecification =
        PC.choice
            [ parseFontSpecAt
            , parseFontSpecScaled
            , skipOptionalSpaces $> NaturalFont
            ]

    parseFontSpecAt = skipKeyword (codesFromStr "at") >> (FontAt <$> parseLength)

    parseFontSpecScaled = skipKeyword (codesFromStr "scaled") >> (FontScaled <$> parseTeXInt)

parseSetFontDimension :: TeXParser s e m AssignmentBody
parseSetFontDimension =
    parseVarEqVal (parseFontDimensionRef, parseLength) SetFontDimension

parseSetFontChar :: TeXParser s e m AssignmentBody
parseSetFontChar = parseVarEqVal (parseFontCharRef, parseTeXInt) SetFontChar

parseSetBoxDimension :: TeXParser s e m AssignmentBody
parseSetBoxDimension =
    parseVarEqVal (parseBoxDimensionRef, parseLength) SetBoxDimension
