{-# LANGUAGE RankNTypes #-}

module HeX.Parse.Command where

import           HeXlude

import qualified Control.Monad.Combinators as PC
import           Data.Functor              (($>))
import           Data.List.NonEmpty        (NonEmpty(..))
import qualified Text.Megaparsec           as P

import           HeX.Config.Codes          (codesFromStr)
import qualified HeX.Config.Codes          as Code
import qualified HeX.Lex                   as Lex
import           HeX.Parse.Assignment
import           HeX.Parse.AST
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
parseStartParagraph = StartParagraph <$> satisfyThen (\case
    T.StartParagraphTok _indent -> Just _indent
    _ -> Nothing)

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
parseBoxOrRule = PC.choice
    [ BoxOrRuleBox <$> parseBox
    , BoxOrRuleRule Horizontal <$> parseModedRule Horizontal
    , BoxOrRuleRule Vertical <$> parseModedRule Vertical
    ]

parseModedRule :: Axis -> TeXParser s e m Rule
parseModedRule axis =
    satisfyEquals (T.ModedCommand axis T.RuleTok) >> parseRule

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
        skipKeyword (codesFromStr "width")
        ln <- parseLength
        pure rule { width = Just ln }

    parseRuleHeight rule = do
        skipKeyword (codesFromStr "height")
        ln <- parseLength
        pure rule { height = Just ln }

    parseRuleDepth rule = do
        skipKeyword (codesFromStr "depth")
        ln <- parseLength
        pure rule { depth = Just ln }

parseModeIndependentCommand :: TeXParser s e m ModeIndependentCommand
parseModeIndependentCommand =
    PC.choice
        [ Assign <$> parseAssignment
        , satisfyEquals T.RelaxTok $> Relax
        , satisfyEquals T.IgnoreSpacesTok
            >> skipOptionalSpaces $> IgnoreSpaces
        , satisfyEquals T.PenaltyTok
            >> (AddPenalty <$> parseTeXInt)
        , satisfyEquals T.KernTok
            >> (AddKern <$> parseLength)
        , satisfyEquals T.MathKernTok
            >> (AddMathKern <$> parseMathLength)
        , parseRemoveItem
        , satisfyEquals T.SetAfterAssignmentTokenTok
              >> (SetAfterAssignmentToken <$> parseLexToken)
        , satisfyEquals T.AddToAfterGroupTokensTok
              >> (AddToAfterGroupTokens <$> parseLexToken)
        , parseMessage
        , parseOpenInput
        , satisfyEquals T.CloseInputTok
              >> (ModifyFileStream FileInput Close <$> parseTeXInt)
        , parseOpenOutput
        , parseCloseOutput
        , parseWriteToStream
        , satisfyEquals T.DoSpecialTok
              >> (DoSpecial <$> parseExpandedGeneralText)
        , parseAddShiftedBox
        , AddBox NaturalPlacement <$> parseBox
        , parseChangeScope
        ]

parseChangeScope :: TeXParser s e m ModeIndependentCommand
parseChangeScope = satisfyThen $
    \t -> if
        | primTokHasCategory Code.BeginGroup t -> Just $
            ChangeScope T.Positive CharCommandTrigger
        | primTokHasCategory Code.EndGroup t -> Just $
            ChangeScope T.Negative CharCommandTrigger
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
    PC.choice
        [ parseSpecifiedGlue
        , parsePresetGlue T.Fil
        , parsePresetGlue T.Fill
        , parsePresetGlue T.StretchOrShrink
        , parsePresetGlue T.FilNeg
        ]
  where
    -- \hskip 10pt and such.
    parseSpecifiedGlue =
        satisfyEquals (T.ModedCommand axis T.SpecifiedGlueTok)
        >> parseGlue

    parsePresetGlue t =
        satisfyEquals (T.ModedCommand axis (T.PresetGlueTok t))
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
    satisfyEquals T.OpenInputTok
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
    PC.option Deferred $ satisfyEquals T.ImmediateTok $> Immediate

parseOpenOutput :: TeXParser s e m ModeIndependentCommand
parseOpenOutput = do
    writePolicy <- parseOptionalImmediate
    satisfyEquals T.OpenOutputTok
    parseModifyFileStream (FileOutput writePolicy)

parseCloseOutput :: TeXParser s e m ModeIndependentCommand
parseCloseOutput = do
    writePolicy <- parseOptionalImmediate
    satisfyEquals T.CloseOutputTok
        >> (ModifyFileStream (FileOutput writePolicy) Close <$> parseTeXInt)

parseWriteToStream :: TeXParser s e m ModeIndependentCommand
parseWriteToStream = do
    writePolicy <- parseOptionalImmediate
    satisfyEquals T.WriteTok
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
        [ ModeIndependentCommand <$> parseModeIndependentCommand
        , satisfyEquals T.ShowTokenTok
            >> (ShowToken <$> parseLexToken)
        , satisfyEquals T.ShowBoxTok
            >> (ShowBox <$> parseTeXInt)
        , satisfyEquals T.ShowListsTok
            $> ShowLists
        , satisfyEquals T.ShowTheInternalQuantityTok
            >> (ShowTheInternalQuantity <$> parseInternalQuantity)
        , satisfyEquals T.ShipOutTok
            >> (ShipOut <$> parseBox)
        , satisfyEquals T.MarkTok
            >> (AddMark <$> parseGeneralText)
          -- , parseInsert
          -- , parseVAdjust
        , skipSatisfied isSpace
            $> AddSpace
        , parseStartParagraph
        , satisfyEquals T.EndParagraphTok $> EndParagraph
          -- , parseAlign mode

        , HModeCommand <$> parseHModeCommand
        , VModeCommand <$> parseVModeCommand
        ]
  where
    parseHModeCommand =
        PC.choice
            [ satisfyEquals T.ControlSpaceTok $> AddControlSpace
            , AddCharacter <$> parseCharCodeRef
            , parseAddAccentedCharacter
            , satisfyEquals T.ItalicCorrectionTok $> AddItalicCorrection
            , parseAddDiscretionaryText
            , satisfyEquals T.DiscretionaryHyphenTok $> AddDiscretionaryHyphen
            , skipSatisfied (primTokHasCategory Code.MathShift) $> EnterMathMode
            , AddHGlue <$> parseModedGlue Horizontal
            , AddHLeaders <$> parseLeadersSpec Horizontal
            , AddHRule <$> parseModedRule Horizontal
            , AddUnwrappedFetchedHBox <$> parseFetchedBoxRef Horizontal
            ]

    parseVModeCommand =
        PC.choice
            [ satisfyEquals T.EndTok $> End
            , satisfyEquals T.DumpTok $> Dump
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
            T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Code.Letter)) ->
                Just $ CharRef c
            T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Code.Other)) ->
                Just $ CharRef c
            T.IntRefTok T.CharQuantity i -> Just $ CharTokenRef i
            _ -> Nothing

    parseAddControlCharacter = satisfyEquals T.ControlCharTok
        >> (CharCodeNrRef <$> parseTeXInt)

parseAddAccentedCharacter :: TeXParser s e m HModeCommand
parseAddAccentedCharacter =
    do
    satisfyEquals T.AccentTok
    AddAccentedCharacter <$> parseTeXInt
        <*> PC.many parseNonSetBoxAssignment
        <*> PC.optional parseCharCodeRef
  where
    -- ⟨optional assignments⟩ stands for zero or more ⟨assignment⟩ commands
    -- other than \setbox.
    parseNonSetBoxAssignment =
        parseAssignment >>= \case
            Assignment (SetBoxRegister _ _) _ -> P.failure (Just (P.Label ('S' :| "etBoxRegister"))) mempty
            a -> pure a

parseAddDiscretionaryText :: TeXParser s e m HModeCommand
parseAddDiscretionaryText =
    satisfyEquals T.DiscretionaryTextTok
    >> AddDiscretionaryText
        <$> parseGeneralText
        <*> parseGeneralText
        <*> parseGeneralText
