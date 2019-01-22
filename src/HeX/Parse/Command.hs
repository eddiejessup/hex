{-# LANGUAGE DuplicateRecordFields #-}

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
    P.choice [ addSpace
             , addRule mode
             , startParagraph
             , endParagraph
             , ModeIndependentCommand <$> parseModeIndependentCommand mode
             ]

parseModeIndependentCommand :: Axis -> SimpExpandParser ModeIndependentCommand
parseModeIndependentCommand mode =
    P.choice [ relax
             , ignorespaces
             , addPenalty
             , addKern
             , addSpecifiedGlue mode
             , Assign <$> parseAssignment
             ]

checkModeAndToken
    :: Axis
    -> (T.ModedCommandPrimitiveToken -> Bool)
    -> T.PrimitiveToken
    -> Bool
checkModeAndToken m1 chk (T.ModedCommand m2 tok) = (m1 == m2) && chk tok
checkModeAndToken _  _   _                       = False

-- \relax.
relax :: SimpExpandParser ModeIndependentCommand
relax = skipSatisfiedEquals T.RelaxTok $> Relax

-- \ignorespaces.
ignorespaces :: SimpExpandParser ModeIndependentCommand
ignorespaces =
    skipSatisfiedEquals T.IgnoreSpacesTok >> skipOptionalSpaces $> IgnoreSpaces

-- \penalty 100.
addPenalty :: SimpExpandParser ModeIndependentCommand
addPenalty = skipSatisfiedEquals T.AddPenaltyTok >> (AddPenalty <$> parseNumber)

-- \kern 100.
addKern :: SimpExpandParser ModeIndependentCommand
addKern = skipSatisfiedEquals T.AddKernTok >> (AddKern <$> parseLength)

-- \hskip 10pt and such.
addSpecifiedGlue :: Axis -> SimpExpandParser ModeIndependentCommand
addSpecifiedGlue mode =
    do
    skipSatisfied $ checkModeAndToken mode (== T.AddSpecifiedGlueTok)
    AddGlue <$> parseGlue

-- ' '.
addSpace :: SimpExpandParser AllModesCommand
addSpace = skipSatisfied isSpace $> AddSpace

-- \hrule and such.
addRule :: Axis -> SimpExpandParser AllModesCommand
addRule mode =
    do
    skipSatisfied $ checkModeAndToken mode (== T.AddRuleTok)
    rule <- parseRuleSpecification Rule { width = Nothing, height = Nothing, depth = Nothing }
    pure $ AddRule rule
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

startParagraph :: SimpExpandParser AllModesCommand
startParagraph =
    satisfyThen parToCom
  where
    parToCom (T.StartParagraphTok _indent) =
        Just $ StartParagraph _indent
    parToCom _ =
        Nothing

endParagraph :: SimpExpandParser AllModesCommand
endParagraph = skipSatisfiedEquals T.EndParagraphTok $> EndParagraph

-- HMode.

parseHModeCommand :: SimpExpandParser HModeCommand
parseHModeCommand =
    P.choice [leaveHMode, addCharacter] <|>
    (HAllModesCommand <$> parseAllModeCommand Horizontal)

leaveHMode :: SimpExpandParser HModeCommand
leaveHMode =
    skipSatisfied endsHMode $> LeaveHMode
  where
    endsHMode T.EndTok                    = True
    endsHMode (T.ModedCommand Vertical _) = True
    -- endsHMode Dump                     = True
    endsHMode _                           = False

addCharacter :: SimpExpandParser HModeCommand
addCharacter =
    (AddCharacter . CharRef) <$> satisfyThen charToCode
  where
    charToCode (T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Lex.Letter))) =
      Just c
    charToCode (T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Lex.Other))) =
      Just c
    charToCode _ =
      Nothing

-- VMode.

parseVModeCommand :: SimpExpandParser VModeCommand
parseVModeCommand =
    P.choice [enterHMode, end] <|>
    (VAllModesCommand <$> parseAllModeCommand Vertical)

-- \end.
end :: SimpExpandParser VModeCommand
end = skipSatisfiedEquals T.EndTok $> End

enterHMode :: SimpExpandParser VModeCommand
enterHMode = skipSatisfied startsHMode $> EnterHMode
  where
    startsHMode (T.ModedCommand Horizontal _) = True
    startsHMode x
        | primTokHasCategory Lex.Letter x = True
        | primTokHasCategory Lex.Other x  = True
        | otherwise                       = False
    -- TODO:
    -- - \char
    -- - TokenForCharacter
    -- - AddAccentedCharacter
    -- - AddItalicCorrection
    -- - AddDiscretionaryText
    -- - AddDiscretionaryHyphen
    -- - ToggleMathMode
