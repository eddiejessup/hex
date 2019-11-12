{-# LANGUAGE RankNTypes #-}

module HeX.Parse.Assignment where

import           HeXlude

import           Control.Monad             (when)
import qualified Control.Monad.Combinators as PC
import           Control.Monad.Reader      (runReaderT)
import           Data.Functor              (($>))
import qualified Data.Set                  as Set
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

headToParseAssignment :: T.PrimitiveToken -> TeXParser s e m Assignment
headToParseAssignment = go []
  where
    go prefixes = \case
        T.AssignPrefixTok prefix ->
            P.anySingle >>= go (prefix : prefixes)
        T.DefineMacroTok defGlobalType defExpandType ->
            do
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
            let body = DefineControlSequence cs (MacroTarget tgt)
            pure $ Assignment
                { body
                , global = if defGlobalType == T.Global || T.GlobalTok `elem` prefixes
                                then T.Global
                                else T.Local
                }
        t ->
            do
            body <- headToParseNonMacroAssignmentBody t
            pure $ Assignment
                { body
                , global = if T.GlobalTok `elem` prefixes
                                then T.Global
                                else T.Local
                }

headToParseNonMacroAssignmentBody :: forall s e m. TeXParseable s e m => T.PrimitiveToken -> SimpleParsecT s m AssignmentBody
headToParseNonMacroAssignmentBody =
    choiceFlap
        [ \case
            T.HyphenationTok ->
                SetHyphenation <$> parseGeneralText
            T.HyphenationPatternsTok ->
                SetHyphenationPatterns <$> parseGeneralText
            T.InteractionModeTok intMode ->
                pure (SetInteractionMode intMode)
            _ ->
                empty
        , headToParseCodeAssignment
        , headToModifyVariable
        , headToParseVariableAssignment
        , headToParseLet
        , headToParseFutureLet
        , headToParseShortMacroAssignment
        , fmap SelectFont <$> headToParseFontRefToken
        , headToParseSetFamilyMember
        , headToParseSetParShape
        , headToParseReadToControlSequence
        , headToParseSetBoxRegister
        , headToParseNewFontAssignment
        , headToParseSetFontDimension
        , headToParseSetFontChar
        , headToParseSetBoxDimension
        ]
  where
    headToParseCodeAssignment t =
        do
        ref <- headToParseCodeTableRef t
        skipOptionalEquals
        AssignCode . CodeAssignment ref <$> parseTeXInt

    headToModifyVariable t = ModifyVariable <$> case t of
        T.AdvanceVarTok ->
            PC.choice
                [ uncurry AdvanceTeXIntVariable <$> parseVarSepVal
                    (parseHeaded headToParseTeXIntVariable)
                    skipOptionalBy
                    parseTeXInt
                , uncurry AdvanceLengthVariable <$> parseVarSepVal
                    (parseHeaded headToParseLengthVariable)
                    skipOptionalBy
                    parseLength
                , uncurry AdvanceGlueVariable <$> parseVarSepVal
                    (parseHeaded headToParseGlueVariable)
                    skipOptionalBy
                    parseGlue
                , uncurry AdvanceMathGlueVariable <$> parseVarSepVal
                    (parseHeaded headToParseMathGlueVariable)
                    skipOptionalBy
                    parseMathGlue
                ]
        T.ScaleVarTok d ->
            uncurry (ScaleVariable d) <$> parseVarSepVal
                parseNumericVariable
                skipOptionalBy
                parseTeXInt
        _ ->
            P.failure (Just (P.Tokens (t :| []))) (Set.singleton (P.Label ('M' :| "odify variable")))

    parseNumericVariable =
        PC.choice
            [ TeXIntNumericVariable <$> parseHeaded headToParseTeXIntVariable
            , LengthNumericVariable <$> parseHeaded headToParseLengthVariable
            , GlueNumericVariable <$> parseHeaded headToParseGlueVariable
            , MathGlueNumericVariable <$> parseHeaded headToParseMathGlueVariable
            ]

    skipOptionalBy =
        PC.choice
            [ void $ parseOptionalKeyword $ codesFromStr "by"
            , skipOptionalSpaces
            ]

    headToParseVariableAssignment t =
        SetVariable <$> PC.choice
            [ uncurry TeXIntVariableAssignment <$> parseVarSepVal
                (headToParseTeXIntVariable t)
                skipOptionalEquals
                parseTeXInt
            , uncurry LengthVariableAssignment <$> parseVarSepVal
                (headToParseLengthVariable t)
                skipOptionalEquals
                parseLength
            , uncurry GlueVariableAssignment <$> parseVarSepVal
                (headToParseGlueVariable t)
                skipOptionalEquals
                parseGlue
            , uncurry MathGlueVariableAssignment <$> parseVarSepVal
                (headToParseMathGlueVariable t)
                skipOptionalEquals
                parseMathGlue
            , uncurry TokenListVariableAssignment <$> parseVarSepVal
                (headToParseTokenListVariable t)
                skipOptionalEquals
                (TokenListAssignmentText <$> parseGeneralText)
            , uncurry TokenListVariableAssignment <$> parseVarSepVal
                (headToParseTokenListVariable t)
                skipFiller
                (TokenListAssignmentVar <$> parseHeaded headToParseTokenListVariable)
              -- Unofficial variable assignments, separated because of being
              -- global in the TeXbook.
            , uncurry SpecialTeXIntVariableAssignment <$> parseVarSepVal
                (headToParseSpecialTeXInt t)
                skipOptionalEquals
                parseTeXInt
            , uncurry SpecialLengthVariableAssignment <$> parseVarSepVal
                (headToParseSpecialLength t)
                skipOptionalEquals
                parseLength
            ]

    headToParseLet = \case
        T.LetTok ->
            do
            (cs, tok) <- parseVarSepVal
                parseCSName
                skipOptionalEquals
                parseLetArg
            pure $ DefineControlSequence cs (LetTarget tok)
        _ ->
            empty

    headToParseFutureLet = \case
        T.FutureLetTok ->
            DefineControlSequence <$> parseCSName <*> (FutureLetTarget <$> parseLexToken <*> parseLexToken)
        _ ->
            empty

    headToParseSetFamilyMember t =
        uncurry SetFamilyMember <$> parseVarSepVal
            (headToParseFamilyMember t)
            skipOptionalEquals
            (parseHeaded headToParseFontRef)

    headToParseSetParShape = \case
        T.ParagraphShapeTok ->
            do
            skipOptionalEquals
            -- In a ⟨shape assignment⟩ for which the ⟨number⟩ is n, the ⟨shape
            -- dimensions⟩ are ⟨empty⟩ if n ≤ 0, otherwise they consist of 2n
            -- consecutive occurrences of ⟨dimen⟩
            nrPairs <- parseTeXInt
            (Q.TeXInt eNrPairsInt) <- P.getInput <&> getConfig >>= runReaderT (texEvaluate nrPairs)
            SetParShape <$> PC.count eNrPairsInt parseLengthPair
        _ ->
            empty
      where
        parseLengthPair = (,) <$> parseLength <*> parseLength

    headToParseReadToControlSequence = \case
        T.ReadTok ->
            do
            nr <- parseTeXInt
            skipKeyword (codesFromStr "to")
            skipOptionalSpaces
            cs <- parseCSName
            pure $ DefineControlSequence cs (ReadTarget nr)
        _ ->
            empty

    headToParseSetBoxRegister = \case
        T.SetBoxRegisterTok ->
            uncurry SetBoxRegister <$> parseVarSepVal
                parseEightBitTeXInt
                skipOptionalEquals
                (skipFiller >> parseHeaded headToParseBox)
        _ ->
            empty

    -- \font <control-sequence> <equals> <file-name> <at-clause>
    headToParseNewFontAssignment = \case
        T.FontTok ->
            do
            cs <- parseCSName
            skipOptionalEquals
            fname <- parseFileName
            fontSpec <- parseFontSpecification
            pure $ DefineControlSequence cs (FontTarget fontSpec fname)
        _ ->
            empty
      where
        parseFontSpecification =
            PC.choice
                [ parseFontSpecAt
                , parseFontSpecScaled
                , skipOptionalSpaces $> NaturalFont
                ]

        parseFontSpecAt = skipKeyword (codesFromStr "at") >> (FontAt <$> parseLength)

        parseFontSpecScaled = skipKeyword (codesFromStr "scaled") >> (FontScaled <$> parseTeXInt)

    headToParseSetFontDimension t =
        do
        var <- headToParseFontDimensionRef t
        skipOptionalEquals
        SetFontDimension var <$> parseLength

    headToParseSetFontChar t =
        do
        var <- headToParseFontCharRef t
        skipOptionalEquals
        SetFontChar var <$> parseTeXInt

    headToParseSetBoxDimension t =
        do
        var <- headToParseBoxDimensionRef t
        skipOptionalEquals
        SetBoxDimension var <$> parseLength

    headToParseShortMacroAssignment = \case
        T.ShortDefHeadTok quant ->
            do
            (cs, n) <- parseVarSepVal
                parseCSName
                skipOptionalEquals
                parseTeXInt
            pure $ DefineControlSequence cs (ShortDefineTarget quant n)
        _ ->
            empty

parseVarSepVal
    :: ( TeXParseable s e m
       )
    => SimpleParsecT s m a
    -> SimpleParsecT s m ()
    -> SimpleParsecT s m b
    -> SimpleParsecT s m (a, b)
parseVarSepVal varParser sepParser valParser =
    do
    var <- varParser
    sepParser
    val <- valParser
    pure (var, val)

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
