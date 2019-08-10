{-# LANGUAGE RankNTypes #-}

module HeX.Parse.Command where

import           HeXlude

import qualified Control.Monad.Combinators as PC
import           Data.Functor              (($>))

import qualified HeX.Lex                   as Lex
import           HeX.Parse.Assignment
import           HeX.Parse.AST
import           HeX.Parse.Parser
import           HeX.Parse.Quantity
import           HeX.Parse.Stream.Class
import qualified HeX.Parse.Token           as T

-- Parse.

parseInternalQuantity :: TeXParser s e m InternalQuantity
parseInternalQuantity = PC.choice
    [ InternalTeXIntQuantity <$> parseInternalTeXInt
    , InternalLengthQuantity <$> parseInternalLength
    , InternalGlueQuantity <$> parseInternalGlue
    , InternalMathGlueQuantity <$> parseInternalMathGlue
    , FontQuantity <$> parseFontRef
    , TokenListVariableQuantity <$> parseTokenListVariable
    ]

parseStartParagraph :: TeXParser s e m Command
parseStartParagraph = satisfyThen $
    \case
        (T.StartParagraphTok _indent) -> Just $ StartParagraph _indent
        _ -> Nothing

parseLeadersSpec :: Axis -> TeXParser s e m LeadersSpec
parseLeadersSpec axis =
    LeadersSpec <$> parseLeaders <*> parseBoxOrRule <*> parseModedGlue axis
  where
    parseLeaders = satisfyThen $
        \case
            T.LeadersTok t -> Just t
            _ -> Nothing

parseFetchedBoxRef :: Axis -> TeXParser s e m FetchedBoxRef
parseFetchedBoxRef tgtAxis = do
    fetchMode <- satisfyThen $ \case
        T.ModedCommand seenAxis (T.UnwrappedFetchedBoxTok fm) | tgtAxis == seenAxis ->
            Just fm
        _ ->
            Nothing
    n <- parseTeXInt
    pure $ FetchedBoxRef n fetchMode

parseBoxOrRule :: TeXParser s e m BoxOrRule
parseBoxOrRule = PC.choice [ BoxOrRuleBox <$> parseBox
                          , BoxOrRuleRule Horizontal <$> parseModedRule Horizontal
                          , BoxOrRuleRule Vertical <$> parseModedRule Vertical
                          ]

parseModedRule :: Axis -> TeXParser s e m Rule
parseModedRule axis =
    skipSatisfiedEquals (T.ModedCommand axis T.RuleTok) >> parseRule

-- \hrule and such.
parseRule :: TeXParser s e m Rule
parseRule = parseRuleSpecification Rule{ width  = Nothing
                                       , height = Nothing
                                       , depth  = Nothing
                                       }
  where
    parseRuleSpecification rule =
        do
        skipOptionalSpaces
        PC.optional $ PC.choice
            [ parseRuleWidth rule
            , parseRuleHeight rule
            , parseRuleDepth rule
            ]
        >>= \case
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

parseModeIndependentCommand :: TeXParser s e m ModeIndependentCommand
parseModeIndependentCommand =
    PC.choice
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
        , parseOpenOutput
        , parseCloseOutput
        , parseWriteToStream
        , skipSatisfiedEquals T.DoSpecialTok
              >> (DoSpecial <$> parseExpandedGeneralText)
        , parseAddShiftedBox
        , AddBox NaturalPlacement <$> parseBox
        , parseChangeScope
        ]

parseChangeScope :: TeXParser s e m ModeIndependentCommand
parseChangeScope = satisfyThen $
    \t -> if
        | primTokHasCategory Lex.BeginGroup t -> Just $
            ChangeScope (T.Sign True) CharCommandTrigger
        | primTokHasCategory Lex.EndGroup t -> Just $
            ChangeScope (T.Sign False) CharCommandTrigger
        | (T.ChangeScopeCSTok sign)
            <- t -> Just $ ChangeScope sign CSCommandTrigger
        | otherwise -> Nothing

satisfyThenGetMode :: T.ModedCommandPrimitiveToken -> TeXParser s e m Axis
satisfyThenGetMode validTok = satisfyThen $ \case
    T.ModedCommand axis seenTok | seenTok == validTok ->
        Just axis
    _ ->
        Nothing

parseRemoveItem :: TeXParser s e m ModeIndependentCommand
parseRemoveItem =
    RemoveItem <$> satisfyThen (\case
                        T.RemoveItemTok i -> Just i
                        _ -> Nothing)

parseModedGlue :: Axis -> TeXParser s e m Glue
parseModedGlue axis =
    PC.choice [ parseSpecifiedGlue
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

parseMessage :: TeXParser s e m ModeIndependentCommand
parseMessage = Message <$> parseMsgStream <*> parseExpandedGeneralText
  where
    parseMsgStream = satisfyThen $
        \case
            T.MessageTok str -> Just str
            _ -> Nothing

parseOpenInput :: TeXParser s e m ModeIndependentCommand
parseOpenInput =
    do
    skipSatisfiedEquals T.OpenInputTok
    parseModifyFileStream FileInput

parseModifyFileStream :: FileStreamType -> TeXParser s e m ModeIndependentCommand
parseModifyFileStream fileStreamType =
    do
    n <- parseTeXInt
    skipOptionalEquals
    fn <- parseFileName
    pure $ ModifyFileStream fileStreamType (Open fn) n

parseOptionalImmediate :: TeXParser s e m WritePolicy
parseOptionalImmediate =
    PC.option Deferred $ skipSatisfiedEquals T.ImmediateTok $> Immediate

parseOpenOutput :: TeXParser s e m ModeIndependentCommand
parseOpenOutput = do
    writePolicy <- parseOptionalImmediate
    skipSatisfiedEquals T.OpenOutputTok
    parseModifyFileStream (FileOutput writePolicy)

parseCloseOutput :: TeXParser s e m ModeIndependentCommand
parseCloseOutput = do
    writePolicy <- parseOptionalImmediate
    skipSatisfiedEquals T.CloseOutputTok
        >> (ModifyFileStream (FileOutput writePolicy) Close <$> parseTeXInt)

parseWriteToStream :: TeXParser s e m ModeIndependentCommand
parseWriteToStream = do
    writePolicy <- parseOptionalImmediate
    skipSatisfiedEquals T.WriteTok
    n <- parseTeXInt
    txt <- case writePolicy of
        Immediate -> ImmediateWriteText <$> parseExpandedGeneralText
        Deferred  -> DeferredWriteText <$> parseGeneralText
    pure $ WriteToStream n txt

parseAddShiftedBox :: TeXParser s e m ModeIndependentCommand
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

parseCommand :: TeXParser s e m Command
parseCommand =
    PC.choice
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
        PC.choice
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
        PC.choice
            [ skipSatisfiedEquals T.EndTok $> End
            , skipSatisfiedEquals T.DumpTok $> Dump
            , AddVGlue <$> parseModedGlue Vertical
            , AddVLeaders <$> parseLeadersSpec Vertical
            , AddVRule <$> parseModedRule Vertical
            , AddUnwrappedFetchedVBox <$> parseFetchedBoxRef Vertical
            ]

parseCharCodeRef :: TeXParser s e m CharCodeRef
parseCharCodeRef =
    PC.choice [ parseAddCharacterCharOrTok, parseAddControlCharacter ]
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

parseAddAccentedCharacter :: TeXParser s e m HModeCommand
parseAddAccentedCharacter =
    do
    skipSatisfiedEquals T.AccentTok
    AddAccentedCharacter <$> parseTeXInt
        <*> PC.many parseNonSetBoxAssignment
        <*> PC.optional parseCharCodeRef
  where
    -- ⟨optional assignments⟩ stands for zero or more ⟨assignment⟩ commands
    -- other than \setbox.
    parseNonSetBoxAssignment =
        parseAssignment >>= \case
            Assignment (SetBoxRegister _ _) _ -> throwParseError $ throw $ ParseError "Cannot set-box while adding accented character"
            a -> pure a

parseAddDiscretionaryText :: TeXParser s e m HModeCommand
parseAddDiscretionaryText =
    skipSatisfiedEquals T.DiscretionaryTextTok
    >> AddDiscretionaryText
        <$> parseGeneralText
        <*> parseGeneralText
        <*> parseGeneralText
