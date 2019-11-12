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

parseInternalQuantity :: TeXParser s e m InternalQuantity
parseInternalQuantity = tryChoice
    [ InternalTeXIntQuantity    <$> parseHeaded headToParseInternalTeXInt
    , InternalLengthQuantity    <$> parseHeaded headToParseInternalLength
    , InternalGlueQuantity      <$> parseHeaded headToParseInternalGlue
    , InternalMathGlueQuantity  <$> parseHeaded headToParseInternalMathGlue
    , FontQuantity              <$> parseHeaded headToParseFontRef
    , TokenListVariableQuantity <$> parseHeaded headToParseTokenListVariable
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
parseBoxOrRule = tryChoice
    [ BoxOrRuleBox <$> parseHeaded headToParseBox
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
        PC.optional $ tryChoice
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
    P.anySingle >>= choiceFlap
        [ fmap Assign <$> headToParseAssignment
        , \case
            T.RelaxTok ->
                pure Relax
            T.IgnoreSpacesTok ->
                do
                skipOptionalSpaces
                pure IgnoreSpaces
            T.PenaltyTok ->
                AddPenalty <$> parseTeXInt
            T.KernTok ->
                AddKern <$> parseLength
            T.MathKernTok ->
                AddMathKern <$> parseMathLength
            T.SetAfterAssignmentTokenTok ->
                SetAfterAssignmentToken <$> parseLexToken
            T.AddToAfterGroupTokensTok ->
                AddToAfterGroupTokens <$> parseLexToken
            T.CloseInputTok ->
                ModifyFileStream FileInput Close <$> parseTeXInt
            T.DoSpecialTok ->
                DoSpecial <$> parseExpandedGeneralText
            T.RemoveItemTok i ->
                pure (RemoveItem i)
            T.MessageTok str ->
                Message str <$> parseExpandedGeneralText
            T.OpenInputTok ->
                parseModifyFileStream FileInput
            T.ImmediateTok ->
                tryChoice
                    [ parseHeaded (headToParseOpenOutput Immediate)
                    , parseHeaded (headToParseCloseOutput Immediate)
                    , parseHeaded (headToParseWriteToStream Immediate)
                    ]
            T.ModedCommand axis (T.ShiftedBoxTok direction) ->
                do
                placement <- ShiftedPlacement axis direction <$> parseLength
                AddBox placement <$> parseHeaded headToParseBox
            -- Change scope.
            T.ChangeScopeCSTok sign ->
                pure $ ChangeScope sign CSCommandTrigger
            t
                | primTokHasCategory Code.BeginGroup t ->
                    pure $ ChangeScope T.Positive CharCommandTrigger
                | primTokHasCategory Code.EndGroup t ->
                    pure $ ChangeScope T.Negative CharCommandTrigger
            _ ->
                empty
        , headToParseOpenOutput Deferred
        , headToParseCloseOutput Deferred
        , headToParseWriteToStream Deferred
        , fmap (AddBox NaturalPlacement) <$> headToParseBox
        ]
  where
    parseModifyFileStream fileStreamType =
        do
        n <- parseTeXInt
        skipOptionalEquals
        fn <- parseFileName
        pure $ ModifyFileStream fileStreamType (Open fn) n

    headToParseOpenOutput writePolicy = \case
        T.OpenOutputTok ->
            parseModifyFileStream (FileOutput writePolicy)
        _ ->
            empty

    headToParseCloseOutput writePolicy = \case
        T.CloseOutputTok ->
            ModifyFileStream (FileOutput writePolicy) Close <$> parseTeXInt
        _ ->
            empty

    headToParseWriteToStream writePolicy = \case
        T.WriteTok ->
            do
            n <- parseTeXInt
            txt <- case writePolicy of
                Immediate -> ImmediateWriteText <$> parseExpandedGeneralText
                Deferred  -> DeferredWriteText <$> parseGeneralText
            pure $ WriteToStream n txt
        _ ->
            empty

satisfyThenGetMode :: T.ModedCommandPrimitiveToken -> TeXParser s e m Axis
satisfyThenGetMode validTok = satisfyThen $ \case
    T.ModedCommand axis seenTok | seenTok == validTok ->
        Just axis
    _ ->
        Nothing

parseModedGlue :: Axis -> TeXParser s e m Glue
parseModedGlue axis =
    tryChoice
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

parseCommand :: TeXParser s e m Command
parseCommand =
    tryChoice
        [ ModeIndependentCommand <$> parseModeIndependentCommand
        , satisfyEquals T.ShowTokenTok >> (ShowToken <$> parseLexToken)
        , satisfyEquals T.ShowBoxTok >> (ShowBox <$> parseTeXInt)
        , satisfyEquals T.ShowListsTok $> ShowLists
        , satisfyEquals T.ShowTheInternalQuantityTok >> (ShowTheInternalQuantity <$> parseInternalQuantity)
        , satisfyEquals T.ShipOutTok >> (ShipOut <$> parseHeaded headToParseBox)
        , satisfyEquals T.MarkTok >> (AddMark <$> parseGeneralText)
          -- , parseInsert
          -- , parseVAdjust
        , skipSatisfied isSpace $> AddSpace
        , parseStartParagraph
        , satisfyEquals T.EndParagraphTok $> EndParagraph
          -- , parseAlign mode

        , HModeCommand <$> parseHModeCommand
        , VModeCommand <$> parseVModeCommand
        ]
  where
    parseHModeCommand =
        tryChoice
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
        tryChoice
            [ satisfyEquals T.EndTok $> End
            , satisfyEquals T.DumpTok $> Dump
            , AddVGlue <$> parseModedGlue Vertical
            , AddVLeaders <$> parseLeadersSpec Vertical
            , AddVRule <$> parseModedRule Vertical
            , AddUnwrappedFetchedVBox <$> parseFetchedBoxRef Vertical
            ]

parseCharCodeRef :: TeXParser s e m CharCodeRef
parseCharCodeRef =
    tryChoice [ parseAddCharacterCharOrTok, parseAddControlCharacter ]
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
        parseHeaded headToParseAssignment >>= \case
            Assignment (SetBoxRegister _ _) _ -> P.failure (Just (P.Label ('S' :| "etBoxRegister"))) mempty
            a -> pure a

parseAddDiscretionaryText :: TeXParser s e m HModeCommand
parseAddDiscretionaryText =
    satisfyEquals T.DiscretionaryTextTok
    >> AddDiscretionaryText
        <$> parseGeneralText
        <*> parseGeneralText
        <*> parseGeneralText
