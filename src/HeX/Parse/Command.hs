{-# LANGUAGE DuplicateRecordFields #-}

module HeX.Parse.Command where

import qualified Text.Megaparsec               as P
import           Text.Megaparsec                ( (<|>) )
import           Data.Functor                   ( ($>) )

import           HeX.Concept
import qualified HeX.Lex                       as Lex
import           HeX.Categorise                 ( CharCode )

import           HeX.Parse.Helpers
import qualified HeX.Parse.Token               as T
import           HeX.Parse.Common
import           HeX.Parse.Glue
import           HeX.Parse.Length
import           HeX.Parse.Number
import           HeX.Parse.Stream
import           HeX.Parse.Assignment

-- AST.

data CharSource
  = ExplicitChar
  | CodeChar
  | TokenChar
  deriving (Show)

data ModeIndependentCommand
  = Assign Assignment
  | Relax
  | IgnoreSpaces
  | AddPenalty Number
  | AddKern Length
  | AddGlue Glue
  deriving (Show)

data AllModesCommand
  -- \| LeftBrace
  -- \| RightBrace
  -- \| BeginGroup
  -- \| EndGroup
  -- \| ShowToken Token
  -- \| ShowBox Int
  -- \| ShowLists
  -- \| ShowInternalQuantity InternalQuantity
  -- \| ShipOut Box
  -- \| SetAfterAssignmentToken Token
  -- \| AddToAfterGroupTokens Tokens
  -- \| Message MessageStream GeneralText
  -- \| OpenInput { streamNr :: Int, fileName :: String }
  -- \| CloseInput { streamNr :: Int }
  -- \| OpenOutput { streamNr :: Int, fileName :: String, immediate :: Bool }
  -- \| CloseOutput { streamNr :: Int, immediate :: Bool }
  -- \| Write { streamNr :: Int, contents :: GeneralText, immediate :: Bool }
  -- \| AddWhatsit GeneralText
  -- \| RemoveLastPenalty
  -- \| RemoveLastKern
  -- \| RemoveLastGlue
  -- \| AddMark GeneralText
  -- -- Note: this *is* an all-modes command. It can happen in non-vertical modes,
  -- -- then can 'migrate' out.
  -- \| AddInsertion {nr :: Int, contents :: VModeMaterial}
  -- \| AddLeaders {type :: LeadersType, template :: BoxOrRule, glue :: Glue}
  = AddSpace
  -- \| AddBox Box
  -- \| AddShiftedBox Distance Box
  -- \| AddFetchedBox { register :: Int, unwrap, pop :: Bool } -- \box, \copy, \un{v,h}{box,copy}
  | AddRule { width, height, depth :: Maybe Length }
  -- \| AddAlignedMaterial DesiredLength AlignmentMaterial
  | StartParagraph { indent :: Bool }
  | EndParagraph
  | ModeIndependentCommand ModeIndependentCommand
  deriving (Show)

data VModeCommand
  = VAllModesCommand AllModesCommand
  | EnterHMode
  | End
  -- \| Dump
  deriving (Show)

data HModeCommand
  = HAllModesCommand AllModesCommand
  | LeaveHMode
  -- \| EnterMathMode
  -- \| AddAdjustment VModeMaterial
  -- \| AddControlSpace
  | AddCharacter { method :: CharSource
                 , char :: CharCode }
  -- \| AddAccentedCharacter { accentCode :: Int, targetCode :: Maybe Int, assignments :: [Assignment]}
  -- \| AddItalicCorrection
  -- \| AddDiscretionaryText { preBreak, postBreak, noBreak :: GeneralText }
  deriving (Show)

-- Entry-points.

type ExtractResult c = Either (ParseErrorBundle ExpandedStream) (P.State ExpandedStream, c)

extractResult :: SimpExpandParser c -> ExpandedStream -> ExtractResult c
extractResult p stream = do
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
  P.choice
    [ addSpace
    , addRule mode
    , startParagraph
    , endParagraph
    , ModeIndependentCommand <$> parseModeIndependentCommand mode ]

parseModeIndependentCommand :: Axis -> SimpExpandParser ModeIndependentCommand
parseModeIndependentCommand mode =
  P.choice
    [ relax
    , ignorespaces
    , addPenalty
    , addKern
    , addSpecifiedGlue mode
    , Assign <$> parseAssignment ]

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
ignorespaces = do
  skipSatisfiedEquals T.IgnoreSpacesTok
  skipOptionalSpaces
  pure IgnoreSpaces

-- \penalty 100.
addPenalty :: SimpExpandParser ModeIndependentCommand
addPenalty = do
  skipSatisfiedEquals T.AddPenaltyTok
  AddPenalty <$> parseNumber

-- \kern 100.
addKern :: SimpExpandParser ModeIndependentCommand
addKern = do
  skipSatisfiedEquals T.AddKernTok
  AddKern <$> parseLength

-- \hskip 10pt and such.
addSpecifiedGlue :: Axis -> SimpExpandParser ModeIndependentCommand
addSpecifiedGlue mode = do
  skipSatisfied $ checkModeAndToken mode (== T.AddSpecifiedGlueTok)
  AddGlue <$> parseGlue

-- ' '.
addSpace :: SimpExpandParser AllModesCommand
addSpace = skipSatisfied isSpace $> AddSpace

-- \hrule and such.
addRule :: Axis -> SimpExpandParser AllModesCommand
addRule mode = do
  skipSatisfied $ checkModeAndToken mode (== T.AddRuleTok)
  let cmd = AddRule {width = Nothing, height = Nothing, depth = Nothing}
  parseRuleSpecification cmd
  where
    parseRuleSpecification cmd = do
      skipOptionalSpaces
      x <-
        P.optional $
        P.try $
        P.choice [parseRuleWidth cmd, parseRuleHeight cmd, parseRuleDepth cmd]
      case x of
        Just newCmd -> parseRuleSpecification newCmd
        Nothing -> pure cmd
    parseRuleWidth cmd = do
      skipKeyword "width"
      ln <- parseLength
      pure cmd {width = Just ln}
    parseRuleHeight cmd = do
      skipKeyword "height"
      ln <- parseLength
      pure cmd {height = Just ln}
    parseRuleDepth cmd = do
      skipKeyword "depth"
      ln <- parseLength
      pure cmd {depth = Just ln}

startParagraph :: SimpExpandParser AllModesCommand
startParagraph = satisfyThen parToCom
  where
    parToCom (T.StartParagraphTok _indent) =
      Just StartParagraph {indent = _indent}
    parToCom _ = Nothing

endParagraph :: SimpExpandParser AllModesCommand
endParagraph = const EndParagraph <$> skipSatisfiedEquals T.EndParagraphTok

-- HMode.

parseHModeCommand :: SimpExpandParser HModeCommand
parseHModeCommand =
  P.choice [leaveHMode, addCharacter] <|>
  (HAllModesCommand <$> parseAllModeCommand Horizontal)

leaveHMode :: SimpExpandParser HModeCommand
leaveHMode = skipSatisfied endsHMode $> LeaveHMode
  where
    endsHMode T.EndTok = True
    endsHMode (T.ModedCommand Vertical _) = True
    -- endsHMode Dump = True
    endsHMode _ = False

addCharacter :: SimpExpandParser HModeCommand
addCharacter = do
  c <- satisfyThen charToCode
  pure AddCharacter {method = ExplicitChar, char = c}
  where
    charToCode (T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Lex.Letter)))
      = Just c
    charToCode (T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c Lex.Other)))
      = Just c
    charToCode _ = Nothing

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
      | primTokHasCategory Lex.Other x = True
      | otherwise = False
    -- TODO:
    -- - \char
    -- - TokenForCharacter
    -- - AddAccentedCharacter
    -- - AddItalicCorrection
    -- - AddDiscretionaryText
    -- - AddDiscretionaryHyphen
    -- - ToggleMathMode
