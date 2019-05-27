module HeX.Parse.Command where

import           HeXlude

import           Data.Functor         ( ($>) )

import qualified Text.Megaparsec      as P

import qualified HeX.Lex              as Lex
import           HeX.Parse.AST
import           HeX.Parse.Assignment
import           HeX.Parse.Common
import           HeX.Parse.Helpers
import           HeX.Parse.Inhibited
import           HeX.Parse.Quantity
import qualified HeX.Parse.Token      as T

-- Entry-points.
type ExtractResult s c = Either (ParseErrorBundle s) (P.State s, c)

extractResult :: SimpParser s c -> s -> ExtractResult s c
extractResult p stream = do
    let (parseState, eCom) = easyRunParser p stream
    com <- eCom
    pure (parseState, com)

extractCommand :: InhibitableStream s => s -> ExtractResult s Command
extractCommand = extractResult parseCommand

-- Parse.

parseInternalQuantity :: InhibitableStream s => SimpParser s InternalQuantity
parseInternalQuantity =
    P.choice [ InternalTeXIntQuantity <$> parseInternalTeXInt
             , InternalLengthQuantity <$> parseInternalLength
             , InternalGlueQuantity <$> parseInternalGlue
             , InternalMathGlueQuantity <$> parseInternalMathGlue
             , FontQuantity <$> parseFontRef
             , TokenListVariableQuantity <$> parseTokenListVariable
             ]

parseStartParagraph :: InhibitableStream s => SimpParser s Command
parseStartParagraph = satisfyThen $
    \case
        (T.StartParagraphTok _indent) -> Just $ StartParagraph _indent
        _ -> Nothing

parseLeadersSpec :: InhibitableStream s => Axis -> SimpParser s LeadersSpec
parseLeadersSpec axis =
    LeadersSpec <$> parseLeaders <*> parseBoxOrRule <*> parseModedGlue axis
  where
    parseLeaders = satisfyThen $
        \case
            T.LeadersTok t -> Just t
            _ -> Nothing

parseFetchedBoxRef
    :: InhibitableStream s
    => Axis
    -> SimpParser s FetchedBoxRef
parseFetchedBoxRef tgtAxis = do
    fetchMode <- satisfyThen $ \case
        T.ModedCommand seenAxis (T.UnwrappedFetchedBoxTok fm) | tgtAxis == seenAxis ->
            Just fm
        _ ->
            Nothing
    n <- parseTeXInt
    pure $ FetchedBoxRef n fetchMode

parseBoxOrRule :: InhibitableStream s => SimpParser s BoxOrRule
parseBoxOrRule = P.choice [ BoxOrRuleBox <$> parseBox
                          , BoxOrRuleRule Horizontal <$> parseModedRule Horizontal
                          , BoxOrRuleRule Vertical <$> parseModedRule Vertical
                          ]

parseModedRule :: InhibitableStream s => Axis -> SimpParser s Rule
parseModedRule axis =
    skipSatisfiedEquals (T.ModedCommand axis T.RuleTok) >> parseRule

-- \hrule and such.
parseRule :: InhibitableStream s => SimpParser s Rule
parseRule = parseRuleSpecification Rule{ width  = Nothing
                                       , height = Nothing
                                       , depth  = Nothing
                                       }
  where
    parseRuleSpecification rule = do
        skipOptionalSpaces
        mayNewRule <- P.optional $
            P.try $
            P.choice [ parseRuleWidth rule
                     , parseRuleHeight rule
                     , parseRuleDepth rule
                     ]
        case mayNewRule of
            Just newRule -> parseRuleSpecification newRule
            Nothing      -> pure rule

    parseRuleWidth rule = do
        skipKeyword "width"
        ln <- parseLength
        pure rule { width = Just ln }

    parseRuleHeight rule = do
        skipKeyword "height"
        ln <- parseLength
        pure rule { height = Just ln }

    parseRuleDepth rule = do
        skipKeyword "depth"
        ln <- parseLength
        pure rule { depth = Just ln }

parseModeIndependentCommand
    :: InhibitableStream s
    => SimpParser s ModeIndependentCommand
parseModeIndependentCommand =
    P.choice
        [ skipSatisfiedEquals T.RelaxTok $> Relax
        , skipSatisfiedEquals T.IgnoreSpacesTok
              >> skipOptionalSpaces $> IgnoreSpaces
        , skipSatisfiedEquals T.PenaltyTok >> (AddPenalty <$> parseTeXInt)
        , skipSatisfiedEquals T.KernTok >> (AddKern <$> parseLength)
        , skipSatisfiedEquals T.MathKernTok
              >> (AddMathKern <$> parseMathLength)
        , parseRemoveItem
        , Assign <$> parseAssignment
        , skipSatisfiedEquals T.SetAfterAssignmentTokenTok
              >> (SetAfterAssignmentToken <$> parseLexToken)
        , skipSatisfiedEquals T.AddToAfterGroupTokensTok
              >> (AddToAfterGroupTokens <$> parseLexToken)
        , parseMessage
        , parseOpenInput
        , skipSatisfiedEquals T.CloseInputTok
              >> (ModifyFileStream FileInput Close <$> parseTeXInt)
          -- Need a 'try' because all these can start with '\immediate'.
        , P.try parseOpenOutput
        , P.try parseCloseOutput
        , P.try parseWriteToStream
        , skipSatisfiedEquals T.DoSpecialTok
              >> (DoSpecial <$> parseExpandedGeneralText)
        , parseAddShiftedBox
        , AddBox NaturalPlacement <$> parseBox
        , parseChangeScope
        ]

parseChangeScope :: InhibitableStream s => SimpParser s ModeIndependentCommand
parseChangeScope = satisfyThen $
    \t -> if
        | primTokHasCategory Lex.BeginGroup t -> Just $
            ChangeScope (T.Sign True) CharCommandTrigger
        | primTokHasCategory Lex.EndGroup t -> Just $
            ChangeScope (T.Sign False) CharCommandTrigger
        | (T.ChangeScopeCSTok sign)
            <- t -> Just $ ChangeScope sign CSCommandTrigger
        | otherwise -> Nothing

satisfyThenGetMode
    :: (P.Stream s, P.Token s ~ T.PrimitiveToken)
    => T.ModedCommandPrimitiveToken -> SimpParser s Axis
satisfyThenGetMode validTok = satisfyThen $ \case
    T.ModedCommand axis seenTok | seenTok == validTok ->
        Just axis
    _ ->
        Nothing

parseRemoveItem :: InhibitableStream s => SimpParser s ModeIndependentCommand
parseRemoveItem =
    RemoveItem <$> satisfyThen (\case
                        T.RemoveItemTok i -> Just i
                        _ -> Nothing)

parseModedGlue :: InhibitableStream s => Axis -> SimpParser s Glue
parseModedGlue axis =
    P.choice [ parseSpecifiedGlue
             , parsePresetGlue T.Fil
             , parsePresetGlue T.Fill
             , parsePresetGlue T.StretchOrShrink
             , parsePresetGlue T.FilNeg
             ]
  where
    -- \hskip 10pt and such.
    parseSpecifiedGlue =
        skipSatisfiedEquals (T.ModedCommand axis T.SpecifiedGlueTok)
        >> parseGlue

    parsePresetGlue t =
        skipSatisfiedEquals (T.ModedCommand axis (T.PresetGlueTok t))
        $> presetToSpecifiedGlue t

    -- \{v,h}fil:    0pt plus 1fil
    -- \{v,h}fill:   0pt plus 1fill
    -- \{v,h}ss:     0pt plus 1fil minus 1fil
    -- \{v,h}filneg: 0pt plus -1fil
    presetToSpecifiedGlue = \case
        T.Fil -> noLengthGlue (Just oneFilFlex) Nothing
        T.Fill -> noLengthGlue (Just oneFillFlex) Nothing
        T.StretchOrShrink -> noLengthGlue (Just oneFilFlex) (Just oneFilFlex)
        T.FilNeg -> noLengthGlue (Just minusOneFilFlex) Nothing

    noLengthGlue = ExplicitGlue zeroLength

parseMessage :: InhibitableStream s => SimpParser s ModeIndependentCommand
parseMessage = Message <$> parseMsgStream <*> parseExpandedGeneralText
  where
    parseMsgStream = satisfyThen $
        \case
            T.MessageTok str -> Just str
            _ -> Nothing

parseOpenInput :: InhibitableStream s => SimpParser s ModeIndependentCommand
parseOpenInput =
    do
    skipSatisfiedEquals T.OpenInputTok
    parseModifyFileStream FileInput

parseModifyFileStream :: InhibitableStream s => FileStreamType -> SimpParser s ModeIndependentCommand
parseModifyFileStream fileStreamType =
    do
    n <- parseTeXInt
    skipOptionalEquals
    fn <- parseFileName
    pure $ ModifyFileStream fileStreamType (Open fn) n

parseOptionalImmediate :: InhibitableStream s => SimpParser s WritePolicy
parseOptionalImmediate =
    P.option Deferred $ skipSatisfiedEquals T.ImmediateTok $> Immediate

parseOpenOutput :: InhibitableStream s => SimpParser s ModeIndependentCommand
parseOpenOutput = do
    writePolicy <- parseOptionalImmediate
    skipSatisfiedEquals T.OpenOutputTok
    parseModifyFileStream (FileOutput writePolicy)

parseCloseOutput :: InhibitableStream s => SimpParser s ModeIndependentCommand
parseCloseOutput = do
    writePolicy <- parseOptionalImmediate
    skipSatisfiedEquals T.CloseOutputTok
        >> (ModifyFileStream (FileOutput writePolicy) Close <$> parseTeXInt)

parseWriteToStream :: InhibitableStream s
                   => SimpParser s ModeIndependentCommand
parseWriteToStream = do
    writePolicy <- parseOptionalImmediate
    skipSatisfiedEquals T.WriteTok
    n <- parseTeXInt
    txt <- case writePolicy of
        Immediate -> ImmediateWriteText <$> parseExpandedGeneralText
        Deferred  -> DeferredWriteText <$> parseGeneralText
    pure $ WriteToStream n txt

parseAddShiftedBox :: InhibitableStream s
                   => SimpParser s ModeIndependentCommand
parseAddShiftedBox = AddBox <$> parsePlacement <*> parseBox
  where
    parseDirection = satisfyThen $ \case
        T.ModedCommand axis (T.ShiftedBoxTok d) ->
            Just (axis, d)
        _ ->
            Nothing

    parsePlacement =
        do
        (axis, direction) <- parseDirection
        ShiftedPlacement axis direction <$> parseLength

parseCommand :: InhibitableStream s => SimpParser s Command
parseCommand =
    P.choice
        [ HModeCommand <$> parseHModeCommand
        , VModeCommand <$> parseVModeCommand

        , skipSatisfiedEquals T.ShowTokenTok
               >> (ShowToken <$> parseLexToken)
        , skipSatisfiedEquals T.ShowBoxTok >> (ShowBox <$> parseTeXInt)
        , skipSatisfiedEquals T.ShowListsTok $> ShowLists
        , skipSatisfiedEquals T.ShowTheInternalQuantityTok
              >> (ShowTheInternalQuantity <$> parseInternalQuantity)
        , skipSatisfiedEquals T.ShipOutTok >> (ShipOut <$> parseBox)
        , skipSatisfiedEquals T.MarkTok >> (AddMark <$> parseGeneralText)
          -- , parseInsert
          -- , parseVAdjust
        , skipSatisfied isSpace $> AddSpace
        , parseStartParagraph
        , skipSatisfiedEquals T.EndParagraphTok $> EndParagraph
          -- , parseAlign mode
        , ModeIndependentCommand <$> parseModeIndependentCommand
        ]
  where
    parseHModeCommand =
        P.choice
            [ skipSatisfiedEquals T.ControlSpaceTok $> AddControlSpace
            , AddCharacter <$> parseCharCodeRef
            , parseAddAccentedCharacter
            , skipSatisfiedEquals T.ItalicCorrectionTok $> AddItalicCorrection
            , parseAddDiscretionaryText
            , skipSatisfiedEquals T.DiscretionaryHyphenTok $> AddDiscretionaryHyphen
            , skipSatisfied (primTokHasCategory Lex.MathShift) $> EnterMathMode
            , AddHGlue <$> parseModedGlue Horizontal
            , AddHLeaders <$> parseLeadersSpec Horizontal
            , AddHRule <$> parseModedRule Horizontal
            , AddUnwrappedFetchedHBox <$> parseFetchedBoxRef Horizontal
            ]

    parseVModeCommand =
        P.choice
            [ skipSatisfiedEquals T.EndTok $> End
            , skipSatisfiedEquals T.DumpTok $> Dump
            , AddVGlue <$> parseModedGlue Vertical
            , AddVLeaders <$> parseLeadersSpec Vertical
            , AddVRule <$> parseModedRule Vertical
            , AddUnwrappedFetchedVBox <$> parseFetchedBoxRef Vertical
            ]

parseCharCodeRef :: InhibitableStream s => SimpParser s CharCodeRef
parseCharCodeRef =
    P.choice [ parseAddCharacterCharOrTok, parseAddControlCharacter ]
  where
    parseAddCharacterCharOrTok = satisfyThen $
        \case
            T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Lex.Letter)) ->
                Just $ CharRef c
            T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Lex.Other)) ->
                Just $ CharRef c
            T.IntRefTok T.CharQuantity i -> Just $ CharTokenRef i
            _ -> Nothing

    parseAddControlCharacter = skipSatisfiedEquals T.ControlCharTok
        >> (CharCodeNrRef <$> parseTeXInt)

parseAddAccentedCharacter :: InhibitableStream s => SimpParser s HModeCommand
parseAddAccentedCharacter = do
    skipSatisfiedEquals T.AccentTok
    AddAccentedCharacter <$> parseTeXInt
        <*> P.many parseNonSetBoxAssignment
        <*> P.optional parseCharCodeRef
-- ⟨optional assignments⟩ stands for zero or more ⟨assignment⟩ commands
-- other than \setbox.
  where
    parseNonSetBoxAssignment = parseAssignment
        >>= \case
            Assignment (SetBoxRegister _ _) _ -> P.empty
            a -> pure a

parseAddDiscretionaryText :: InhibitableStream s => SimpParser s HModeCommand
parseAddDiscretionaryText = do
    skipSatisfiedEquals T.DiscretionaryTextTok
    AddDiscretionaryText <$> parseGeneralText
        <*> parseGeneralText
        <*> parseGeneralText
