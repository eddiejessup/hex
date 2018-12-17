{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HeX.Config.Parameters where

import           HeX.BreakList                  ( Glue(..), noFlex )
import qualified HeX.Unit                      as Unit

-- type IntParamVal = Int
type LengthParameterValue = Int
-- type MathGlueParameterValue = MathGlue
type GlueParameterValue = Glue
-- type TokenListParameterValue = [Token]

newtype IntParamVal a = IntParamVal {unIntParam :: Int}
  deriving (Eq, Enum, Ord, Show, Num, Real, Integral)

newtype LenParamVal a = LenParamVal {unLenParam :: Int}
  deriving (Eq, Enum, Ord, Show, Num, Real, Integral)

newtype GlueParamVal a = GlueParamVal {unGlueParam :: Glue}
  deriving (Show)

-- newtype MathGlueParamVal a = MathGlueParamVal {unGlueParam :: Int}
--   deriving (Show)

-- newtype TokenListParamVal a = TokenListParamVal {unTokenListParam :: [Token]}
--   deriving (Show)

data PreTolerance
data Tolerance
data HBadness
data VBadness
data LinePenalty
data HyphenPenalty
data ExHyphenPenalty
data BinOpPenalty
data RelPenalty
data ClubPenalty
data WidowPenalty
data DisplayWidowPenalty
data BrokenPenalty
data PreDisplayPenalty
data PostDisplayPenalty
data InterlinePenalty
data FloatingPenalty
data OutputPenalty
data DoubleHyphenDemerits
data FinalHyphenDemerits
data AdjDemerits
data Looseness
data Pausing
data HoldingInserts
data TracingOnline
data TracingMacros
data TracingStats
data TracingParagraphs
data TracingPages
data TracingOutput
data TracingLostChars
data TracingCommands
data TracingRestores
data Language
data UCHyph
data LeftHyphenMin
data RightHyphenMin
data GlobalDefs
data DefaultHyphenChar
data DefaultSkewChar
data EscapeChar
data EndLineChar
data NewLineChar
data MaxDeadCycles
data HangAfter
data Fam
data Mag
data DelimiterFactor
data Time
data Day
data Month
data Year
data ShowBoxBreadth
data ShowBoxDepth
data ErrorContextLines

data HFuzz
data VFuzz
data OverfullRule
data EmergencyStretch
data HSize
data VSize
data MaxDepth
data SplitMaxDepth
data BoxMaxDepth
data LineSkipLimit
data DelimiterShortfall
data NullDelimiterSpace
data ScriptSpace
data MathSurround
data PreDisplaySize
data DisplayWidth
data DisplayIndent
data ParIndent
data HangIndent
data HOffset
data VOffset

data BaselineSkip
data LineSkip
data ParSkip
data AboveDisplaySkip
data AboveDisplayShortSkip
data BelowDisplaySkip
data BelowDisplayShortSkip
data LeftSkip
data RightSkip
data TopSkip
data SplitTopSkip
data TabSkip
data SpaceSkip
data XSpaceSkip
data ParFillSkip

-- data ThinMuSkip
-- data MedMuSkip
-- data ThickMuSkip

-- data Output
-- data EveryPar
-- data EveryMath
-- data EveryDisplay
-- data EveryHBox
-- data EveryVBox
-- data EveryJob
-- data EveryCR
-- data ErrHelp

-- Special parameters.

data SpaceFactor
data PrevGraf
data DeadCycles
data InsertPenalties

data PrevDepth
data PageGoal
data PageTotal
data PageStretch
data PageFilStretch
data PageFillStretch
data PageFilllStretch
data PageShrink
data PageDepth

data ParamConfig
  = ParamConfig { preTolerance :: IntParamVal PreTolerance
                , tolerance :: IntParamVal Tolerance
                , hBadness :: IntParamVal HBadness
                , vBadness :: IntParamVal VBadness
                , linePenalty :: IntParamVal LinePenalty
                , hyphenPenalty :: IntParamVal HyphenPenalty
                , exHyphenPenalty :: IntParamVal ExHyphenPenalty
                , binOpPenalty :: IntParamVal BinOpPenalty
                , relPenalty :: IntParamVal RelPenalty
                , clubPenalty :: IntParamVal ClubPenalty
                , widowPenalty :: IntParamVal WidowPenalty
                , displayWidowPenalty :: IntParamVal DisplayWidowPenalty
                , brokenPenalty :: IntParamVal BrokenPenalty
                , preDisplayPenalty :: IntParamVal PreDisplayPenalty
                , postDisplayPenalty :: IntParamVal PostDisplayPenalty
                , interlinePenalty :: IntParamVal InterlinePenalty
                , floatingPenalty :: IntParamVal FloatingPenalty
                , outputPenalty :: IntParamVal OutputPenalty
                , doubleHyphenDemerits :: IntParamVal DoubleHyphenDemerits
                , finalHyphenDemerits :: IntParamVal FinalHyphenDemerits
                , adjDemerits :: IntParamVal AdjDemerits
                , looseness :: IntParamVal Looseness
                , pausing :: IntParamVal Pausing
                , holdingInserts :: IntParamVal HoldingInserts
                , tracingOnline :: IntParamVal TracingOnline
                , tracingMacros :: IntParamVal TracingMacros
                , tracingStats :: IntParamVal TracingStats
                , tracingParagraphs :: IntParamVal TracingParagraphs
                , tracingPages :: IntParamVal TracingPages
                , tracingOutput :: IntParamVal TracingOutput
                , tracingLostChars :: IntParamVal TracingLostChars
                , tracingCommands :: IntParamVal TracingCommands
                , tracingRestores :: IntParamVal TracingRestores
                , language :: IntParamVal Language
                , uCHyph :: IntParamVal UCHyph
                , leftHyphenMin :: IntParamVal LeftHyphenMin
                , rightHyphenMin :: IntParamVal RightHyphenMin
                , globalDefs :: IntParamVal GlobalDefs
                , defaultHyphenChar :: IntParamVal DefaultHyphenChar
                , defaultSkewChar :: IntParamVal DefaultSkewChar
                , escapeChar :: IntParamVal EscapeChar
                , endLineChar :: IntParamVal EndLineChar
                , newLineChar :: IntParamVal NewLineChar
                , maxDeadCycles :: IntParamVal MaxDeadCycles
                , hangAfter :: IntParamVal HangAfter
                , fam :: IntParamVal Fam
                , mag :: IntParamVal Mag
                , delimiterFactor :: IntParamVal DelimiterFactor
                , time :: IntParamVal Time
                , day :: IntParamVal Day
                , month :: IntParamVal Month
                , year :: IntParamVal Year
                , showBoxBreadth :: IntParamVal ShowBoxBreadth
                , showBoxDepth :: IntParamVal ShowBoxDepth
                , errorContextLines :: IntParamVal ErrorContextLines

                , hFuzz :: LenParamVal HFuzz
                , vFuzz :: LenParamVal VFuzz
                , overfullRule :: LenParamVal OverfullRule
                , emergencyStretch :: LenParamVal EmergencyStretch
                , hSize :: LenParamVal HSize
                , vSize :: LenParamVal VSize
                , maxDepth :: LenParamVal MaxDepth
                , splitMaxDepth :: LenParamVal SplitMaxDepth
                , boxMaxDepth :: LenParamVal BoxMaxDepth
                , lineSkipLimit :: LenParamVal LineSkipLimit
                , delimiterShortfall :: LenParamVal DelimiterShortfall
                , nullDelimiterSpace :: LenParamVal NullDelimiterSpace
                , scriptSpace :: LenParamVal ScriptSpace
                , mathSurround :: LenParamVal MathSurround
                , preDisplaySize :: LenParamVal PreDisplaySize
                , displayWidth :: LenParamVal DisplayWidth
                , displayIndent :: LenParamVal DisplayIndent
                , parIndent :: LenParamVal ParIndent
                , hangIndent :: LenParamVal HangIndent
                , hOffset :: LenParamVal HOffset
                , vOffset :: LenParamVal VOffset

                , baselineSkip :: GlueParamVal BaselineSkip
                , lineSkip :: GlueParamVal LineSkip
                , parSkip :: GlueParamVal ParSkip
                , aboveDisplaySkip :: GlueParamVal AboveDisplaySkip
                , aboveDisplayShortSkip :: GlueParamVal AboveDisplayShortSkip
                , belowDisplaySkip :: GlueParamVal BelowDisplaySkip
                , belowDisplayShortSkip :: GlueParamVal BelowDisplayShortSkip
                , leftSkip :: GlueParamVal LeftSkip
                , rightSkip :: GlueParamVal RightSkip
                , topSkip :: GlueParamVal TopSkip
                , splitTopSkip :: GlueParamVal SplitTopSkip
                , tabSkip :: GlueParamVal TabSkip
                , spaceSkip :: GlueParamVal SpaceSkip
                , xSpaceSkip :: GlueParamVal XSpaceSkip
                , parFillSkip :: GlueParamVal ParFillSkip

                -- , thinMuSkip :: MathGlueParam ThinMuSkip
                -- , medMuSkip :: MathGlueParam MedMuSkip
                -- , thickMuSkip :: MathGlueParam ThickMuSkip

                -- , output :: TokenListParam Output
                -- , everyPar :: TokenListParam EveryPar
                -- , everyMath :: TokenListParam EveryMath
                -- , everyDisplay :: TokenListParam EveryDisplay
                -- , everyHBox :: TokenListParam EveryHBox
                -- , everyVBox :: TokenListParam EveryVBox
                -- , everyJob :: TokenListParam EveryJob
                -- , everyCR :: TokenListParam EveryCR
                -- , errHelp :: TokenListParam ErrHelp

                , spaceFactor :: IntParamVal SpaceFactor
                , prevGraf :: IntParamVal PrevGraf
                , deadCycles :: IntParamVal DeadCycles
                , insertPenalties :: IntParamVal InsertPenalties

                , prevDepth :: LenParamVal PrevDepth
                , pageGoal :: LenParamVal PageGoal
                , pageTotal :: LenParamVal PageTotal
                , pageStretch :: LenParamVal PageStretch
                , pageFilStretch :: LenParamVal PageFilStretch
                , pageFillStretch :: LenParamVal PageFillStretch
                , pageFilllStretch :: LenParamVal PageFilllStretch
                , pageShrink :: LenParamVal PageShrink
                , pageDepth :: LenParamVal PageDepth
                }

newParamConfig :: ParamConfig
newParamConfig
  = ParamConfig { preTolerance = IntParamVal 0
                , tolerance = IntParamVal 10000
                , hBadness = IntParamVal 0
                , vBadness = IntParamVal 0
                , linePenalty = IntParamVal 0
                , hyphenPenalty = IntParamVal 0
                , exHyphenPenalty = IntParamVal 0
                , binOpPenalty = IntParamVal 0
                , relPenalty = IntParamVal 0
                , clubPenalty = IntParamVal 0
                , widowPenalty = IntParamVal 0
                , displayWidowPenalty = IntParamVal 0
                , brokenPenalty = IntParamVal 0
                , preDisplayPenalty = IntParamVal 0
                , postDisplayPenalty = IntParamVal 0
                , interlinePenalty = IntParamVal 0
                , floatingPenalty = IntParamVal 0
                , outputPenalty = IntParamVal 0
                , doubleHyphenDemerits = IntParamVal 0
                , finalHyphenDemerits = IntParamVal 0
                , adjDemerits = IntParamVal 0
                , looseness = IntParamVal 0
                , pausing = IntParamVal 0
                , holdingInserts = IntParamVal 0
                , tracingOnline = IntParamVal 0
                , tracingMacros = IntParamVal 0
                , tracingStats = IntParamVal 0
                , tracingParagraphs = IntParamVal 0
                , tracingPages = IntParamVal 0
                , tracingOutput = IntParamVal 0
                , tracingLostChars = IntParamVal 0
                , tracingCommands = IntParamVal 0
                , tracingRestores = IntParamVal 0
                , language = IntParamVal 0
                , uCHyph = IntParamVal 0
                , leftHyphenMin = IntParamVal 0
                , rightHyphenMin = IntParamVal 0
                , globalDefs = IntParamVal 0
                , defaultHyphenChar = IntParamVal 0
                , defaultSkewChar = IntParamVal 0
                , escapeChar = IntParamVal 92  -- '\'
                , endLineChar = IntParamVal 13 -- '\r'
                , newLineChar = IntParamVal 0
                , maxDeadCycles = IntParamVal 25
                , hangAfter = IntParamVal 1
                , fam = IntParamVal 0
                , mag = IntParamVal 1000
                , delimiterFactor = IntParamVal 0
                , time = IntParamVal 1
                , day = IntParamVal 1
                , month = IntParamVal 1
                , year = IntParamVal 1970
                , showBoxBreadth = IntParamVal 0
                , showBoxDepth = IntParamVal 0
                , errorContextLines = IntParamVal 0

                , hFuzz = LenParamVal 0
                , vFuzz = LenParamVal 0
                , overfullRule = LenParamVal 0
                , emergencyStretch = LenParamVal 0
                , hSize = LenParamVal 0
                , vSize = LenParamVal 0
                , maxDepth = LenParamVal 0
                , splitMaxDepth = LenParamVal 0
                , boxMaxDepth = LenParamVal 0
                , lineSkipLimit = LenParamVal 0
                , delimiterShortfall = LenParamVal 0
                , nullDelimiterSpace = LenParamVal 0
                , scriptSpace = LenParamVal 0
                , mathSurround = LenParamVal 0
                , preDisplaySize = LenParamVal 0
                , displayWidth = LenParamVal 0
                , displayIndent = LenParamVal 0
                , parIndent = LenParamVal 0
                , hangIndent = LenParamVal 0
                , hOffset = LenParamVal 0
                , vOffset = LenParamVal 0

                , baselineSkip = GlueParamVal mempty
                , lineSkip = GlueParamVal mempty
                , parSkip = GlueParamVal mempty
                , aboveDisplaySkip = GlueParamVal mempty
                , aboveDisplayShortSkip = GlueParamVal mempty
                , belowDisplaySkip = GlueParamVal mempty
                , belowDisplayShortSkip = GlueParamVal mempty
                , leftSkip = GlueParamVal mempty
                , rightSkip = GlueParamVal mempty
                , topSkip = GlueParamVal mempty
                , splitTopSkip = GlueParamVal mempty
                , tabSkip = GlueParamVal mempty
                , spaceSkip = GlueParamVal mempty
                , xSpaceSkip = GlueParamVal mempty
                , parFillSkip = GlueParamVal mempty

                -- , thinMuSkip :: MathGlueParamVal mempty
                -- , medMuSkip :: MathGlueParamVal mempty
                -- , thickMuSkip :: MathGlueParamVal mempty

                -- , output :: TokenListParamVal mempty
                -- , everyPar :: TokenListParamVal mempty
                -- , everyMath :: TokenListParamVal mempty
                -- , everyDisplay :: TokenListParamVal mempty
                -- , everyHBox :: TokenListParamVal mempty
                -- , everyVBox :: TokenListParamVal mempty
                -- , everyJob :: TokenListParamVal mempty
                -- , everyCR :: TokenListParamVal mempty
                -- , errHelp :: TokenListParamVal mempty

                , spaceFactor = IntParamVal 0
                , prevGraf = IntParamVal 0
                , deadCycles = IntParamVal 0
                , insertPenalties = IntParamVal 0

                , prevDepth = LenParamVal $ -Unit.oneKPt
                , pageGoal = LenParamVal 0
                , pageTotal = LenParamVal 0
                , pageStretch = LenParamVal 0
                , pageFilStretch = LenParamVal 0
                , pageFillStretch = LenParamVal 0
                , pageFilllStretch = LenParamVal 0
                , pageShrink = LenParamVal 0
                , pageDepth = LenParamVal 0
                }

usableParamConfig :: ParamConfig
usableParamConfig
  = newParamConfig { tolerance = IntParamVal 500
                   , linePenalty = IntParamVal 10
                   , mag = IntParamVal 1000
                   , hSize = LenParamVal 30750000
                   , vSize = LenParamVal 37500000
                   , lineSkipLimit = LenParamVal 0
                   , parIndent = LenParamVal $ Unit.toScaledPointApprox (20 :: Int) Unit.Point
                   , baselineSkip = GlueParamVal $ Glue (Unit.toScaledPointApprox (12 :: Int) Unit.Point) noFlex noFlex
                   , lineSkip = GlueParamVal $ Glue (Unit.toScaledPointApprox (1 :: Int) Unit.Point) noFlex noFlex
                   }
