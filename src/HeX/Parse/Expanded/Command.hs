{-# LANGUAGE DuplicateRecordFields #-}

module HeX.Parse.Expanded.Command where

import qualified Text.Megaparsec               as P
import           Text.Megaparsec                ( (<|>) )
import           Data.Functor                   ( ($>) )

import qualified HeX.Lex                       as Lex
import           HeX.Categorise                 ( CharCode )

import           HeX.Parse.Helpers

import qualified HeX.Parse.Resolved.Token      as R

import           HeX.Parse.Expanded.Common
import           HeX.Parse.Expanded.Glue
import           HeX.Parse.Expanded.Length
import           HeX.Parse.Expanded.Number
import           HeX.Parse.Expanded.Stream
import           HeX.Parse.Expanded.Assignment

-- AST.

data CharSource
  = ExplicitChar
  | CodeChar
  | TokenChar
  deriving (Show)

data AllModesCommand
  = Assign Assignment
  | Relax
  -- | LeftBrace
  -- | RightBrace
  -- | BeginGroup
  -- | EndGroup
  -- | ShowToken Token
  -- | ShowBox Int
  -- | ShowLists
  -- | ShowInternalQuantity InternalQuantity
  -- | ShipOut Box
  | IgnoreSpaces
  -- | SetAfterAssignmentToken Token
  -- | AddToAfterGroupTokens Tokens
  -- | Message MessageStream GeneralText
  -- | OpenInput { streamNr :: Int, fileName :: String }
  -- | CloseInput { streamNr :: Int }
  -- | OpenOutput { streamNr :: Int, fileName :: String, immediate :: Bool }
  -- | CloseOutput { streamNr :: Int, immediate :: Bool }
  -- | Write { streamNr :: Int, contents :: GeneralText, immediate :: Bool }
  -- | AddWhatsit GeneralText
  | AddPenalty Number
  | AddKern Length
  -- | RemoveLastPenalty
  -- | RemoveLastKern
  -- | RemoveLastGlue
  -- | AddMark GeneralText
  -- -- Note: this *is* an all-modes command. It can happen in non-vertical modes,
  -- -- then can 'migrate' out.
  -- | AddInsertion {nr :: Int, contents :: VModeMaterial}
  | AddGlue Glue
  -- | AddLeaders {type :: LeadersType, template :: BoxOrRule, glue :: Glue}
  | AddSpace
  -- | AddBox Box
  -- | AddShiftedBox Distance Box
  -- | AddFetchedBox { register :: Int, unwrap, pop :: Bool } -- \box, \copy, \un{v,h}{box,copy}
  | AddRule { width, height, depth :: Maybe Length }
  -- | AddAlignedMaterial DesiredLength AlignmentMaterial
  | StartParagraph { indent :: Bool }
  | EndParagraph
  deriving (Show)

data VModeCommand
  = VAllModesCommand AllModesCommand
  | EnterHMode
  | End
  -- | Dump
  deriving (Show)

data HModeCommand
  = HAllModesCommand AllModesCommand
  | LeaveHMode
  -- | EnterMathMode
  -- | AddAdjustment VModeMaterial
  -- | AddControlSpace
  | AddCharacter { method :: CharSource
                 , char :: CharCode }
  -- | AddAccentedCharacter { accentCode :: Int, targetCode :: Maybe Int, assignments :: [Assignment]}
  -- | AddItalicCorrection
  -- | AddDiscretionaryText { preBreak, postBreak, noBreak :: GeneralText }
  deriving (Show)

-- Entry-points.
extractHModeCommand ::
     ExpandedStream
  -> (P.State ExpandedStream, Either (ParseError ExpandedStream) HModeCommand)
extractHModeCommand = easyRunParser parseHModeCommand

extractVModeCommand ::
     ExpandedStream
  -> (P.State ExpandedStream, Either (ParseError ExpandedStream) VModeCommand)
extractVModeCommand = easyRunParser parseVModeCommand

-- Parse.

type AllModeCommandParser = SimpExpandParser AllModesCommand
type HModeCommandParser = SimpExpandParser HModeCommand
type VModeCommandParser = SimpExpandParser VModeCommand

-- All-mode Commands.

parseAllModeCommand :: R.Axis -> AllModeCommandParser
parseAllModeCommand mode =
  P.choice
    [ relax
    , ignorespaces
    , addPenalty
    , addKern
    , addSpecifiedGlue mode
    , addSpace
    , addRule mode
    , startParagraph
    , endParagraph
    , Assign <$> parseAssignment ]

checkModeAndToken ::
     R.Axis
  -> (R.ModedCommandPrimitiveToken -> Bool)
  -> R.PrimitiveToken
  -> Bool
checkModeAndToken m1 chk (R.ModedCommand m2 tok) = (m1 == m2) && chk tok
checkModeAndToken _ _ _ = False

-- \relax.
relax :: AllModeCommandParser
relax = skipSatisfiedEquals R.Relax $> Relax

-- \ignorespaces.
ignorespaces :: AllModeCommandParser
ignorespaces = do
  skipSatisfiedEquals R.IgnoreSpaces
  skipOptionalSpaces
  pure IgnoreSpaces

-- \penalty 100.
addPenalty :: AllModeCommandParser
addPenalty = do
  skipSatisfiedEquals R.AddPenalty
  AddPenalty <$> parseNumber

-- \kern 100.
addKern :: AllModeCommandParser
addKern = do
  skipSatisfiedEquals R.AddKern
  AddKern <$> parseLength

-- \hskip 10pt and such.
addSpecifiedGlue :: R.Axis -> AllModeCommandParser
addSpecifiedGlue mode = do
  skipSatisfied $ checkModeAndToken mode (== R.AddSpecifiedGlue)
  AddGlue <$> parseGlue

-- ' '.
addSpace :: AllModeCommandParser
addSpace = skipSatisfied isSpace $> AddSpace

-- \hrule and such.
addRule :: R.Axis -> AllModeCommandParser
addRule mode = do
  skipSatisfied $ checkModeAndToken mode (== R.AddRule)
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

startParagraph :: AllModeCommandParser
startParagraph = satisfyThen parToCom
  where
    parToCom (R.StartParagraph _indent) =
      Just StartParagraph {indent = _indent}
    parToCom _ = Nothing

endParagraph :: AllModeCommandParser
endParagraph = const EndParagraph <$> skipSatisfiedEquals R.EndParagraph

-- HMode.

parseHModeCommand :: SimpExpandParser HModeCommand
parseHModeCommand =
  P.choice [leaveHMode, addCharacter] <|>
  (HAllModesCommand <$> parseAllModeCommand R.Horizontal)

leaveHMode :: HModeCommandParser
leaveHMode = skipSatisfied endsHMode $> LeaveHMode
  where
    endsHMode R.End = True
    endsHMode (R.ModedCommand R.Vertical _) = True
    -- endsHMode Dump = True
    endsHMode _ = False

addCharacter :: HModeCommandParser
addCharacter = do
  c <- satisfyThen charToCode
  pure AddCharacter {method = ExplicitChar, char = c}
  where
    charToCode (R.CharCat Lex.CharCat {cat = Lex.Letter, char = c}) =
      Just c
    charToCode (R.CharCat Lex.CharCat {cat = Lex.Other, char = c}) = Just c
    charToCode _ = Nothing

-- VMode.

parseVModeCommand :: SimpExpandParser VModeCommand
parseVModeCommand =
  P.choice [enterHMode, end] <|>
  (VAllModesCommand <$> parseAllModeCommand R.Vertical)

-- \end.
end :: VModeCommandParser
end = skipSatisfiedEquals R.End $> End

enterHMode :: VModeCommandParser
enterHMode = skipSatisfied startsHMode $> EnterHMode
  where
    startsHMode (R.ModedCommand R.Horizontal _) = True
    startsHMode x
      | isLetter x = True
      | isOther x = True
      | otherwise = False
    -- TODO:
    -- - \char
    -- - TokenForCharacter
    -- - AddAccentedCharacter
    -- - AddItalicCorrection
    -- - AddDiscretionaryText
    -- - AddDiscretionaryHyphen
    -- - ToggleMathMode
