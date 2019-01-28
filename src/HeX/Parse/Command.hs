{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module HeX.Parse.Command where

import qualified Text.Megaparsec               as P
import           Text.Megaparsec                ( (<|>) )
import           Data.Functor                   ( ($>) )

import           HeX.Concept
import qualified HeX.Lex                       as Lex

import           HeX.Parse.Helpers
import           HeX.Parse.AST
import qualified HeX.Parse.Token               as T
import           HeX.Parse.Common
import           HeX.Parse.Quantity
import           HeX.Parse.Stream
import           HeX.Parse.Assignment

-- Entry-points.

type ExtractResult c = Either (ParseErrorBundle ExpandedStream) (P.State ExpandedStream, c)

extractResult :: SimpExpandParser c -> ExpandedStream -> ExtractResult c
extractResult p stream =
    do
    let (state, eCom) = easyRunParser p stream
    com <- eCom
    pure (state, com)

extractHModeCommand :: ExpandedStream -> ExtractResult HModeCommand
extractHModeCommand = extractResult parseHModeCommand

extractVModeCommand :: ExpandedStream -> ExtractResult VModeCommand
extractVModeCommand = extractResult parseVModeCommand

-- Parse.

-- All-mode Commands.

parseAllModeCommand :: Axis -> SimpExpandParser AllModesCommand
parseAllModeCommand mode =
    P.choice [ parseChangeScope
             , parseShowToken
             , skipSatisfiedEquals T.ShowBoxTok >> (ShowBox <$> parseNumber)
             , skipSatisfiedEquals T.ShowListsTok $> ShowLists
             , skipSatisfiedEquals T.ShipOutTok >> (ShipOut <$> parseBox)
             , skipSatisfiedEquals T.SetAfterAssignmentTokenTok >> (SetAfterAssignmentToken <$> parseToken)
             , skipSatisfiedEquals T.ToAfterGroupTokensTok >> (AddToAfterGroupTokens <$> parseToken)
             , skipSatisfiedEquals T.MarkTok >> (AddMark <$> parseGeneralText)
             -- , parseInsert
             -- , parseVAdjust
             , skipSatisfied isSpace $> AddSpace
             , AddBox NaturalPlacement <$> parseBox
             , parseStartParagraph
             , skipSatisfiedEquals T.EndParagraphTok $> EndParagraph

             -- Mode-parametrised.
             , parseAddLeaders mode
             , parseAddShiftedBox mode
             , parseAddUnwrappedFetchedBox mode
             , AddRule <$> parseRule mode
             -- , parseAlign mode

             , ModeIndependentCommand <$> parseModeIndependentCommand mode
             ]

parseChangeScope :: SimpExpandParser AllModesCommand
parseChangeScope = satisfyThen tokToChangeScope
  where
    tokToChangeScope t
        | primTokHasCategory Lex.BeginGroup t = Just $ ChangeScope (T.Sign True) CharCommandTrigger
        | primTokHasCategory Lex.EndGroup t   = Just $ ChangeScope (T.Sign False) CharCommandTrigger
        | (T.ChangeScopeCSTok sign) <- t      = Just $ ChangeScope sign CSCommandTrigger
        | otherwise                           = Nothing

parseShowToken :: SimpExpandParser AllModesCommand
parseShowToken = skipSatisfiedEquals T.ShowTokenTok >> (ShowToken <$> parseToken)

parseStartParagraph :: SimpExpandParser AllModesCommand
parseStartParagraph = satisfyThen (\case
    (T.StartParagraphTok _indent) -> Just $ StartParagraph _indent
    _                             -> Nothing)

parseAddLeaders :: Axis -> SimpExpandParser AllModesCommand
parseAddLeaders mode =
    AddLeaders <$> parseLeaders <*> parseBoxOrRule <*> parseModedGlue mode
  where
    parseLeaders = satisfyThen (\case
        T.LeadersTok t -> Just t
        _                 -> Nothing)

parseAddShiftedBox mode = AddBox <$> parsePlacement <*> parseBox
  where
    parseDirection = satisfyThen (\case
        T.ModedCommand mode2 (T.ShiftedBoxTok d) | (mode == mode2) -> Just d
        _ -> Nothing)

    parsePlacement = ShiftedPlacement <$> parseDirection <*> parseLength

parseAddUnwrappedFetchedBox mode =
    do
    fetchMode <- satisfyThen (\case
        T.ModedCommand mode2 (T.UnwrappedFetchedBoxTok fm) | (mode == mode2) -> Just fm
        _ -> Nothing)
    n <- parseNumber
    pure $ AddUnwrappedFetchedBox n fetchMode

parseBoxOrRule :: SimpExpandParser BoxOrRule
parseBoxOrRule = P.choice [ BoxOrRuleBox <$> parseBox
                          , BoxOrRuleRule <$> parseRule Vertical
                          , BoxOrRuleRule <$> parseRule Horizontal
                          ]

-- \hrule and such.
parseRule :: Axis -> SimpExpandParser Rule
parseRule mode =
    do
    skipSatisfied $ checkModeAndToken mode T.RuleTok
    parseRuleSpecification Rule { width = Nothing, height = Nothing, depth = Nothing }
  where
    parseRuleSpecification rule =
        do
        skipOptionalSpaces
        mayNewRule <- P.optional $ P.try $ P.choice [ parseRuleWidth rule
                                                    , parseRuleHeight rule
                                                    , parseRuleDepth rule
                                                    ]
        case mayNewRule of
            Just newRule -> parseRuleSpecification newRule
            Nothing      -> pure rule
    parseRuleWidth rule =
        do
        skipKeyword "width"
        ln <- parseLength
        pure rule { width = Just ln }
    parseRuleHeight rule =
        do
        skipKeyword "height"
        ln <- parseLength
        pure rule { height = Just ln }
    parseRuleDepth rule =
        do
        skipKeyword "depth"
        ln <- parseLength
        pure rule { depth = Just ln }

parseModeIndependentCommand :: Axis -> SimpExpandParser ModeIndependentCommand
parseModeIndependentCommand mode =
    P.choice [ skipSatisfiedEquals T.RelaxTok $> Relax
             , skipSatisfiedEquals T.IgnoreSpacesTok >> skipOptionalSpaces $> IgnoreSpaces
             , skipSatisfiedEquals T.PenaltyTok >> (AddPenalty <$> parseNumber)
             , skipSatisfiedEquals T.KernTok >> (AddKern <$> parseLength)
             , skipSatisfiedEquals T.MathKernTok >> (AddMathKern <$> parseMathLength)
             , parseRemoveItem
             , AddGlue <$> parseModedGlue mode
             , Assign <$> parseAssignment
             , parseMessage
             , parseOpenInput
             , skipSatisfiedEquals T.CloseInputTok >> (ModifyFileStream FileInput Close <$> parseNumber)
             , parseOpenOutput
             , parseCloseOutput
             , parseWriteToStream
             , skipSatisfiedEquals T.DoSpecialTok >> (DoSpecial <$> parseGeneralText)
             ]

checkModeAndToken
    :: Axis
    -> T.ModedCommandPrimitiveToken
    -> T.PrimitiveToken
    -> Bool
checkModeAndToken m1 tok1 (T.ModedCommand m2 tok2) = (m1 == m2) && (tok1 == tok2)
checkModeAndToken _  _   _                       = False

parseRemoveItem :: SimpExpandParser ModeIndependentCommand
parseRemoveItem = RemoveItem <$> satisfyThen (\case
    T.RemoveItemTok i -> Just i
    _                   -> Nothing)

parseModedGlue :: Axis -> SimpExpandParser Glue
parseModedGlue mode = P.choice [ parseSpecifiedGlue mode
                               , parsePresetGlue mode T.Fil
                               , parsePresetGlue mode T.Fill
                               , parsePresetGlue mode T.StretchOrShrink
                               , parsePresetGlue mode T.FilNeg
                               ]

-- \hskip 10pt and such.
parseSpecifiedGlue :: Axis -> SimpExpandParser Glue
parseSpecifiedGlue mode =
    do
    skipSatisfied $ checkModeAndToken mode T.SpecifiedGlueTok
    parseGlue

-- \{v,h}fil:    0pt plus 1fil
-- \{v,h}fill:   0pt plus 1fill
-- \{v,h}ss:     0pt plus 1fil minus 1fil
-- \{v,h}filneg: 0pt plus -1fil
presetToSpecifiedGlue :: T.PresetGlueType -> Glue
presetToSpecifiedGlue = \case
    T.Fil             -> f (Just oneFilFlex)      Nothing
    T.Fill            -> f (Just oneFillFlex)     Nothing
    T.StretchOrShrink -> f (Just oneFilFlex)      (Just oneFilFlex)
    T.FilNeg          -> f (Just minusOneFilFlex) Nothing
  where
    f = ExplicitGlue zeroLength

parsePresetGlue :: Axis -> T.PresetGlueType -> SimpExpandParser Glue
parsePresetGlue mode t =
    do
    skipSatisfied $ checkModeAndToken mode (T.PresetGlueTok t)
    pure $ presetToSpecifiedGlue t

parseMessage :: SimpExpandParser ModeIndependentCommand
parseMessage = Message <$> parseMsgStream <*> parseGeneralText
  where
    parseMsgStream = satisfyThen (\case
        T.MessageTok str -> Just str
        _                -> Nothing)

parseOpenInput :: SimpExpandParser ModeIndependentCommand
parseOpenInput =
    do
    skipSatisfiedEquals T.OpenInputTok
    n <- parseNumber
    skipOptionalEquals
    fn <- parseFileName
    pure $ ModifyFileStream FileInput (Open fn) n

parseOptionalImmediate :: SimpExpandParser WritePolicy
parseOptionalImmediate = P.option Deferred $ skipSatisfiedEquals T.ImmediateTok $> Immediate

parseOpenOutput :: SimpExpandParser ModeIndependentCommand
parseOpenOutput =
    do
    writePolicy <- parseOptionalImmediate
    skipSatisfiedEquals T.OpenOutputTok
    n <- parseNumber
    skipOptionalEquals
    fn <- parseFileName
    pure $ ModifyFileStream (FileOutput writePolicy) (Open fn) n

parseCloseOutput :: SimpExpandParser ModeIndependentCommand
parseCloseOutput =
    do
    writePolicy <- parseOptionalImmediate
    skipSatisfiedEquals T.CloseOutputTok >> (ModifyFileStream (FileOutput writePolicy) Close <$> parseNumber)

parseWriteToStream :: SimpExpandParser ModeIndependentCommand
parseWriteToStream =
    do
    writePolicy <- parseOptionalImmediate
    skipSatisfiedEquals T.WriteTok
    n <- parseNumber
    txt <- parseGeneralText
    pure $ WriteToStream n txt writePolicy

-- VMode.

parseVModeCommand :: SimpExpandParser VModeCommand
parseVModeCommand =
    P.choice [ skipSatisfiedEquals T.EndTok $> End
             , skipSatisfiedEquals T.DumpTok $> Dump
             , parseEnterHMode
             ] <|> (VAllModesCommand <$> parseAllModeCommand Vertical)

parseEnterHMode :: SimpExpandParser VModeCommand
parseEnterHMode = skipSatisfied startsHMode $> EnterHMode
  where
    startsHMode t = case t of
        T.ModedCommand Horizontal _         -> True
        T.ControlCharTok                    -> True
        T.ShortCharRefToken _               -> True
        T.AccentTok                         -> True
        T.DiscretionaryTextTok              -> True
        T.DiscretionaryHyphenTok            -> True
        T.ControlSpaceTok                   -> True
        T.ToggleMathModeTok                 -> True
        _ | primTokHasCategory Lex.Letter t -> True
        _ | primTokHasCategory Lex.Other t  -> True
        _                                   -> False

-- HMode.

parseHModeCommand :: SimpExpandParser HModeCommand
parseHModeCommand =
    P.choice [ skipSatisfiedEquals T.ControlSpaceTok $> AddControlSpace
             , AddCharacter <$> parseCharCodeRef
             , parseAddAccentedCharacter
             , skipSatisfiedEquals T.ItalicCorrectionTok $> AddItalicCorrection
             , parseAddDiscretionaryText
             , skipSatisfiedEquals T.DiscretionaryHyphenTok $> AddDiscretionaryHyphen
             , skipSatisfied (primTokHasCategory Lex.MathShift) $> EnterMathMode
             , parseLeaveHMode
             ] <|> (HAllModesCommand <$> parseAllModeCommand Horizontal)

parseCharCodeRef :: SimpExpandParser CharCodeRef
parseCharCodeRef =
    P.choice [ parseAddCharacterCharOrTok
             , parseAddControlCharacter
             ]
  where
    parseAddCharacterCharOrTok = satisfyThen (\case
        T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Lex.Letter)) ->
            Just $ CharRef c
        T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Lex.Other)) ->
            Just $ CharRef c
        T.ShortCharRefToken c ->
            Just $ CharTokenRef c
        _ ->
          Nothing)

    parseAddControlCharacter =
        skipSatisfiedEquals T.ControlCharTok >> (CharCodeNrRef <$> parseNumber)

parseAddAccentedCharacter :: SimpExpandParser HModeCommand
parseAddAccentedCharacter =
    do
    skipSatisfiedEquals T.AccentTok
    AddAccentedCharacter
        <$> parseNumber
        <*> P.many parseNonSetBoxAssignment
        <*> P.optional parseCharCodeRef
  where
    -- ⟨optional assignments⟩ stands for zero or more ⟨assignment⟩ commands
    -- other than \setbox.
    parseNonSetBoxAssignment =
        parseAssignment >>= \case
            Assignment (SetBoxRegister _ _) _ -> P.empty
            a -> pure a

parseAddDiscretionaryText =
    do
    skipSatisfiedEquals T.DiscretionaryTextTok
    AddDiscretionaryText <$> parseGeneralText <*> parseGeneralText <*> parseGeneralText

parseLeaveHMode :: SimpExpandParser HModeCommand
parseLeaveHMode = skipSatisfied endsHMode $> LeaveHMode
  where
    endsHMode = \case
        T.ModedCommand Vertical _ -> True
        T.EndTok                  -> True
        T.DumpTok                 -> True
        _                         -> False
