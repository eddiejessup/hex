{-# LANGUAGE RankNTypes #-}
module Hex.Parse.CommandParser.Assignment where

import qualified Control.Monad.Combinators as PC
import Hex.Config.Codes (unsafeCodesFromChars)
import qualified Hex.Config.Codes as Code
import Hex.Evaluate
import qualified Hex.Lex as Lex
import Hex.Parse.AST
import Hex.Parse.CommandParser.Quantity
import Hex.Parse.TokenParser.Combinators
import Hex.Parse.CommandParser.Inhibited
import Hex.Parse.TokenParser.Class
import qualified Hex.Resolve.Token as T
import qualified Hex.Quantity as Q
import Hexlude
import qualified Path

parseAssignment :: forall m. (MonadTokenParse m, MonadEvaluate m TeXInt) => m Assignment
parseAssignment = anySingle >>= headToParseAssignment

headToParseAssignment :: forall m. (MonadTokenParse m, MonadEvaluate m TeXInt) => T.PrimitiveToken -> m Assignment
headToParseAssignment = go []
  where
    go :: [T.AssignPrefixTok] -> T.PrimitiveToken -> m Assignment
    go prefixes = \case
      T.AssignPrefixTok prefix ->
        anySingle >>= go (prefix : prefixes)
      T.DefineMacroTok defGlobalType defExpandType -> do
        traceM "Saw define-macro token, parsing macro name"
        -- Macro's name.
        cs <- parseCSName
        traceM $ "Parsed macro name: " <> show cs
        -- Parameter text.
        (preParamTokens, parameters) <- parseParamText
        when (defExpandType == T.ExpandDef) $ panic "Not implemented: ExpandDef"
        -- Replacement text.
        replacementTokens <- parseMacroText
        let tgt = T.MacroContents
              { T.preParamTokens = preParamTokens
              , T.parameters = parameters
              , T.replacementTokens = replacementTokens
              , T.long = T.LongTok `elem` prefixes
              , T.outer = T.OuterTok `elem` prefixes
              }
        let body = DefineControlSequence (ControlSequenceDefinition cs (MacroTarget tgt))
        pure $ Assignment
          { body
          , scope = if defGlobalType == T.Global || T.GlobalTok `elem` prefixes
          then T.Global
          else T.Local
          }
      t -> do
        body <- headToParseNonMacroAssignmentBody t
        pure $ Assignment
          { body
          , scope = if T.GlobalTok `elem` prefixes
          then T.Global
          else T.Local
          }

headToParseNonMacroAssignmentBody
  :: (MonadTokenParse m, MonadEvaluate m TeXInt)
  => T.PrimitiveToken
  -> m AssignmentBody
headToParseNonMacroAssignmentBody = \case
  T.HyphenationTok -> SetHyphenation <$> parseGeneralText
  T.HyphenationPatternsTok ->
    SetHyphenationPatterns <$> parseGeneralText
  T.InteractionModeTok intMode ->
    pure (SetInteractionMode intMode)
  T.LetTok -> do
    cs <- parseCSName
    skipOptionalEquals
    DefineControlSequence . ControlSequenceDefinition cs <$> (LetTarget <$> parseLetArg)
  T.FutureLetTok -> do
    csDefn <- ControlSequenceDefinition <$>
      parseCSName <*>
      (FutureLetTarget <$> parseLexToken <*> parseLexToken)
    pure $ DefineControlSequence csDefn
  T.ShortDefHeadTok quant -> do
    cs <- parseCSName
    skipOptionalEquals
    csDefn <- ControlSequenceDefinition cs <$> (ShortDefineTarget quant <$> parseTeXInt)
    pure $ DefineControlSequence csDefn
  T.ParagraphShapeTok -> do
    skipOptionalEquals
    -- In a ⟨shape assignment⟩ for which the ⟨number⟩ is n, the ⟨shape
    -- dimensions⟩ are ⟨empty⟩ if n ≤ 0, otherwise they consist of 2n
    -- consecutive occurrences of ⟨dimen⟩
    nrPairs <- parseTeXInt
    Q.TeXInt eNrPairsInt <- astEval nrPairs
    let parseLengthPair = (,) <$> parseLength <*> parseLength
    SetParShape <$> PC.count eNrPairsInt parseLengthPair
  T.ReadTok -> do
    nr <- parseTeXInt
    skipKeyword (unsafeCodesFromChars "to")
    skipOptionalSpaces
    cs <- parseCSName
    pure $ DefineControlSequence $ ControlSequenceDefinition cs (ReadTarget nr)
  T.SetBoxRegisterTok -> do
    var <- parseEightBitTeXInt
    skipOptionalEquals
    skipFiller
    SetBoxRegister var <$> parseHeaded headToParseBox
  -- \font <control-sequence> <equals> <file-name> <at-clause>
  T.FontTok -> do
    cs <- parseCSName
    skipOptionalEquals
    fname <- parseFileName
    fontSpec <-
      PC.choice
        [ skipKeyword (unsafeCodesFromChars "at") >> (FontAt <$> parseLength)
        , skipKeyword (unsafeCodesFromChars "scaled") >> (FontScaled <$> parseTeXInt)
        , skipOptionalSpaces $> NaturalFont
        ]
    pure $ DefineControlSequence $ ControlSequenceDefinition cs (FontTarget fontSpec fname)
  t ->
    choiceFlap
      [ headToParseCodeAssignment
      , fmap ModifyVariable <$> headToModifyVariable
      , fmap SetVariable <$> headToParseVariableAssignment
      , fmap SelectFont <$> headToParseFontRefToken
      , headToParseSetFamilyMember
      , headToParseSetFontDimension
      , headToParseSetFontChar
      , headToParseSetBoxDimension
      ]
      t
  where
    headToParseCodeAssignment t = do
      ref <- headToParseCodeTableRef t
      skipOptionalEquals
      AssignCode . CodeAssignment ref <$> parseTeXInt
    headToModifyVariable = \case
      T.AdvanceVarTok ->
        PC.choice
          [ do
              var <- parseHeaded headToParseTeXIntVariable
              skipOptionalBy
              AdvanceTeXIntVariable var <$> parseTeXInt
          , do
            var <- parseHeaded headToParseLengthVariable
            skipOptionalBy
            AdvanceLengthVariable var <$> parseLength
          , do
            var <- parseHeaded headToParseGlueVariable
            skipOptionalBy
            AdvanceGlueVariable var <$> parseGlue
          , do
            var <- parseHeaded headToParseMathGlueVariable
            skipOptionalBy
            AdvanceMathGlueVariable var <$> parseMathGlue
          ]
      T.ScaleVarTok d -> do
        var <- parseNumericVariable
        skipOptionalBy
        ScaleVariable d var <$> parseTeXInt
      t ->
        parseError $ ParseErrorWithMsg $ "Expected 'AdvanceVarTok' or 'ScaleVarTok', saw " <> show t
    parseNumericVariable =
      PC.choice
        [ TeXIntNumericVariable <$> parseHeaded headToParseTeXIntVariable
        , LengthNumericVariable <$> parseHeaded headToParseLengthVariable
        , GlueNumericVariable <$> parseHeaded headToParseGlueVariable
        , MathGlueNumericVariable <$> parseHeaded headToParseMathGlueVariable
        ]
    skipOptionalBy =
      PC.choice
        [ void $ parseOptionalKeyword $ unsafeCodesFromChars "by"
        , skipOptionalSpaces
        ]
    headToParseVariableAssignment t =
      PC.choice
        [ do
            var <- headToParseTeXIntVariable t
            skipOptionalEquals
            TeXIntVariableAssignment var <$> parseTeXInt
        , do
          var <- headToParseLengthVariable t
          skipOptionalEquals
          LengthVariableAssignment var <$> parseLength
        , do
          var <- headToParseGlueVariable t
          skipOptionalEquals
          GlueVariableAssignment var <$> parseGlue
        , do
          var <- headToParseMathGlueVariable t
          skipOptionalEquals
          MathGlueVariableAssignment var <$> parseMathGlue
        , do
          var <- headToParseTokenListVariable t
          skipOptionalEquals
          TokenListVariableAssignment var <$> (TokenListAssignmentText <$> parseGeneralText)
        , do
          var <- headToParseTokenListVariable t
          skipFiller
          TokenListVariableAssignment var <$> (TokenListAssignmentVar <$> parseHeaded headToParseTokenListVariable)
        , -- Unofficial variable assignments, separated because of being
        -- global in the TeXbook.
        do
          var <- headToParseSpecialTeXInt t
          skipOptionalEquals
          SpecialTeXIntVariableAssignment var <$> parseTeXInt
        , do
          var <- headToParseSpecialLength t
          skipOptionalEquals
          SpecialLengthVariableAssignment var <$> parseLength
        ]
    headToParseSetFamilyMember t = do
      var <- headToParseFamilyMember t
      skipOptionalEquals
      SetFamilyMember var <$> parseHeaded headToParseFontRef
    headToParseSetFontDimension t = do
      var <- headToParseFontDimensionRef t
      skipOptionalEquals
      SetFontDimension var <$> parseLength
    headToParseSetFontChar t = do
      var <- headToParseFontCharRef t
      skipOptionalEquals
      SetFontChar var <$> parseTeXInt
    headToParseSetBoxDimension t = do
      var <- headToParseBoxDimensionRef t
      skipOptionalEquals
      SetBoxDimension var <$> parseLength

-- <file name> = <optional spaces> <some explicit letter or digit characters> <space>
parseFileName :: MonadTokenParse m=> m TeXFilePath
parseFileName = do
  skipOptionalSpaces
  fileNameChars <-
    PC.some $ satisfyThen $ \case
      T.UnresolvedTok (Lex.CharCatToken (Lex.CharCat c Code.Letter)) ->
        Just c
      T.UnresolvedTok (Lex.CharCatToken (Lex.CharCat c Code.Other))
        | isValidOther c ->
          Just c
      _ -> Nothing
  skipSatisfied isSpace
  case Path.parseRelFile (Code.unsafeCodeAsChar <$> fileNameChars) of
    Just p -> pure $ TeXFilePath p
    Nothing -> panic $ show fileNameChars
  where
    isValidOther = \case
      -- Not in the spec, but let's say these are OK.
      Code.CharCode_ '/' -> True
      Code.CharCode_ '.' -> True
      Code.CharCode_ '_' -> True
      -- 'Other' Characters for decimal digits are OK.
      cc -> Code.isDecDigitChar cc
