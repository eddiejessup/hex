{-# LANGUAGE RankNTypes #-}

module HeX.Parse.Command where

import           HeXlude

import qualified Control.Monad.Combinators as PC
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

headToParseModeIndependentCommand :: T.PrimitiveToken -> TeXParser s e m ModeIndependentCommand
headToParseModeIndependentCommand =
    choiceFlap
        [ \case
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
        , fmap Assign <$> headToParseAssignment
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

parseCommand :: TeXParser s e m Command
parseCommand =
    P.anySingle >>= choiceFlap
        [ \case
            T.ShowTokenTok ->
                ShowToken <$> parseLexToken
            T.ShowBoxTok ->
                ShowBox <$> parseTeXInt
            T.ShowListsTok ->
                pure ShowLists
            T.ShowTheInternalQuantityTok ->
                ShowTheInternalQuantity <$> parseInternalQuantity
            T.ShipOutTok ->
                ShipOut <$> parseHeaded headToParseBox
            T.MarkTok ->
                AddMark <$> parseGeneralText
            T.StartParagraphTok _indent ->
                pure $ StartParagraph _indent
            T.EndParagraphTok ->
                pure EndParagraph
            t | isSpace t ->
                pure AddSpace
            _ ->
                empty
          -- , parseInsert
          -- , parseVAdjust
          -- , parseAlign mode
        , fmap ModeIndependentCommand <$> headToParseModeIndependentCommand
        , fmap HModeCommand <$> headToParseHModeCommand
        , fmap VModeCommand <$> headToParseVModeCommand
        ]
  where
    headToParseHModeCommand =
        choiceFlap
            [ \case
                T.ControlSpaceTok ->
                    pure AddControlSpace
                T.ItalicCorrectionTok ->
                    pure AddItalicCorrection
                T.DiscretionaryHyphenTok ->
                    pure AddDiscretionaryHyphen
                t | primTokHasCategory Code.MathShift t ->
                    pure EnterMathMode
                T.AccentTok ->
                    AddAccentedCharacter
                        <$> parseTeXInt
                        <*> PC.many parseNonSetBoxAssignment
                        <*> PC.optional (parseHeaded headToParseCharCodeRef)
                T.DiscretionaryTextTok ->
                    AddDiscretionaryText
                        <$> parseGeneralText
                        <*> parseGeneralText
                        <*> parseGeneralText
                _ ->
                    empty
            , fmap AddCharacter <$> headToParseCharCodeRef
            , fmap AddHGlue <$> headToParseModedGlue Horizontal
            , fmap AddHLeaders <$> headToParseLeadersSpec Horizontal
            , fmap AddHRule <$> headToParseModedRule Horizontal
            , fmap AddUnwrappedFetchedHBox <$> headToParseFetchedBoxRef Horizontal
            ]

    headToParseVModeCommand =
        choiceFlap
            [ \case
                T.EndTok ->
                    pure End
                T.DumpTok ->
                    pure Dump
                _ ->
                    empty
            , fmap AddVGlue <$> headToParseModedGlue Vertical
            , fmap AddVLeaders <$> headToParseLeadersSpec Vertical
            , fmap AddVRule <$> headToParseModedRule Vertical
            , fmap AddUnwrappedFetchedVBox <$> headToParseFetchedBoxRef Vertical
            ]

    headToParseCharCodeRef = \case
        -- Add character.
        T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Code.Letter)) ->
            pure $ CharRef c
        T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Code.Other)) ->
            pure $ CharRef c
        T.IntRefTok T.CharQuantity i ->
            pure $ CharTokenRef i
        T.ControlCharTok ->
            CharCodeNrRef <$> parseTeXInt
        _ ->
            empty
        -- /Add character.

    -- ⟨optional assignments⟩ stands for zero or more ⟨assignment⟩ commands
    -- other than \setbox.
    parseNonSetBoxAssignment =
        parseHeaded headToParseAssignment >>= \case
            Assignment (SetBoxRegister _ _) _ -> P.failure (Just (P.Label ('S' :| "etBoxRegister"))) mempty
            a -> pure a

    headToParseModedGlue :: Axis -> T.PrimitiveToken -> TeXParser s e m Glue
    headToParseModedGlue axis = \case
        T.ModedCommand tokenAxis modedTok | tokenAxis == axis ->
            case modedTok of
                -- \hskip 10pt and such.
                T.SpecifiedGlueTok | tokenAxis == axis ->
                    parseGlue
                T.PresetGlueTok presetTok ->
                    pure $ case presetTok of
                        -- \{v,h}fil:    0pt plus 1fil
                        T.Fil ->
                            noLengthGlue (Just oneFilFlex) Nothing
                        -- \{v,h}fill:   0pt plus 1fill
                        T.Fill ->
                            noLengthGlue (Just oneFillFlex) Nothing
                        -- \{v,h}ss:     0pt plus 1fil minus 1fil
                        T.StretchOrShrink ->
                            noLengthGlue (Just oneFilFlex) (Just oneFilFlex)
                        -- \{v,h}filneg: 0pt plus -1fil
                        T.FilNeg ->
                            noLengthGlue (Just minusOneFilFlex) Nothing
                _ ->
                    empty
        _ ->
            empty
      where
        noLengthGlue = ExplicitGlue zeroLength

    headToParseLeadersSpec :: Axis -> T.PrimitiveToken -> TeXParser s e m LeadersSpec
    headToParseLeadersSpec axis = \case
            T.LeadersTok leaders ->
                LeadersSpec leaders <$> parseBoxOrRule <*> parseHeaded (headToParseModedGlue axis)
            _ ->
                empty

    parseBoxOrRule :: TeXParser s e m BoxOrRule
    parseBoxOrRule = tryChoice
        [ BoxOrRuleBox <$> parseHeaded headToParseBox
        , BoxOrRuleRule Horizontal <$> parseHeaded (headToParseModedRule Horizontal)
        , BoxOrRuleRule Vertical <$> parseHeaded (headToParseModedRule Vertical)
        ]

    headToParseModedRule :: Axis -> T.PrimitiveToken -> TeXParser s e m Rule
    headToParseModedRule axis = \case
        T.ModedCommand tokenAxis T.RuleTok | axis == tokenAxis ->
            parseRule
        _ ->
            empty

    headToParseFetchedBoxRef :: Axis -> T.PrimitiveToken -> TeXParser s e m FetchedBoxRef
    headToParseFetchedBoxRef tgtAxis = \case
        T.ModedCommand tokenAxis (T.UnwrappedFetchedBoxTok fetchMode) | tgtAxis == tokenAxis ->
            do
            n <- parseTeXInt
            pure $ FetchedBoxRef n fetchMode
        _ ->
            empty
