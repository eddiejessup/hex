{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HeX.Config.Parameters where

import           HeX.BreakList                  ( Glue(..), noFlex )
import qualified HeX.Unit                      as Unit

type IntegerVariable = Int
type LengthVariable = Int
-- type MathGlueVariable = MathGlue
type GlueVariable = Glue
-- type TokenListVariable = [Token]

newtype PreTolerance = PreTolerance {unPreTolerance :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype Tolerance = Tolerance {unTolerance :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype HBadness = HBadness {unHBadness :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype VBadness = VBadness {unVBadness :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype LinePenalty = LinePenalty {unLinePenalty :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype HyphenPenalty = HyphenPenalty {unHyphenPenalty :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype ExHyphenPenalty = ExHyphenPenalty {unExHyphenPenalty :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype BinOpPenalty = BinOpPenalty {unBinOpPenalty :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype RelPenalty = RelPenalty {unRelPenalty :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype ClubPenalty = ClubPenalty {unClubPenalty :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype WidowPenalty = WidowPenalty {unWidowPenalty :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype DisplayWidowPenalty = DisplayWidowPenalty {unDisplayWidowPenalty :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype BrokenPenalty = BrokenPenalty {unBrokenPenalty :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype PreDisplayPenalty = PreDisplayPenalty {unPreDisplayPenalty :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype PostDisplayPenalty = PostDisplayPenalty {unPostDisplayPenalty :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype InterlinePenalty = InterlinePenalty {unInterlinePenalty :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype FloatingPenalty = FloatingPenalty {unFloatingPenalty :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype OutputPenalty = OutputPenalty {unOutputPenalty :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype DoubleHyphenDemerits = DoubleHyphenDemerits {unDoubleHyphenDemerits :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype FinalHyphenDemerits = FinalHyphenDemerits {unFinalHyphenDemerits :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype AdjDemerits = AdjDemerits {unAdjDemerits :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype Looseness = Looseness {unLooseness :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype Pausing = Pausing {unPausing :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype HoldingInserts = HoldingInserts {unHoldingInserts :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype TracingOnline = TracingOnline {unTracingOnline :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype TracingMacros = TracingMacros {unTracingMacros :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype TracingStats = TracingStats {unTracingStats :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype TracingParagraphs = TracingParagraphs {unTracingParagraphs :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype TracingPages = TracingPages {unTracingPages :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype TracingOutput = TracingOutput {unTracingOutput :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype TracingLostChars = TracingLostChars {unTracingLostChars :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype TracingCommands = TracingCommands {unTracingCommands :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype TracingRestores = TracingRestores {unTracingRestores :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype Language = Language {unLanguage :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype UCHyph = UCHyph {unUCHyph :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype LeftHyphenMin = LeftHyphenMin {unLeftHyphenMin :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype RightHyphenMin = RightHyphenMin {unRightHyphenMin :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype GlobalDefs = GlobalDefs {unGlobalDefs :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype DefaultHyphenChar = DefaultHyphenChar {unDefaultHyphenChar :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype DefaultSkewChar = DefaultSkewChar {unDefaultSkewChar :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype EscapeChar = EscapeChar {unEscapeChar :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype EndLineChar = EndLineChar {unEndLineChar :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype NewLineChar = NewLineChar {unNewLineChar :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype MaxDeadCycles = MaxDeadCycles {unMaxDeadCycles :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype HangAfter = HangAfter {unHangAfter :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype Fam = Fam {unFam :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype Mag = Mag {unMag :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype DelimiterFactor = DelimiterFactor {unDelimiterFactor :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype Time = Time {unTime :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype Day = Day {unDay :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype Month = Month {unMonth :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype Year = Year {unYear :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype ShowBoxBreadth = ShowBoxBreadth {unShowBoxBreadth :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype ShowBoxDepth = ShowBoxDepth {unShowBoxDepth :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype ErrorContextLines = ErrorContextLines {unErrorContextLines :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)

newtype HFuzz = HFuzz { unHFuzz :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype VFuzz = VFuzz { unVFuzz :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype OverfullRule = OverfullRule { unOverfullRule :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype EmergencyStretch = EmergencyStretch { unEmergencyStretch :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype HSize = HSize { unHSize :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype VSize = VSize { unVSize :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype MaxDepth = MaxDepth { unMaxDepth :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype SplitMaxDepth = SplitMaxDepth { unSplitMaxDepth :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype BoxMaxDepth = BoxMaxDepth { unBoxMaxDepth :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype LineSkipLimit = LineSkipLimit { unLineSkipLimit :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype DelimiterShortfall = DelimiterShortfall { unDelimiterShortfall :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype NullDelimiterSpace = NullDelimiterSpace { unNullDelimiterSpace :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype ScriptSpace = ScriptSpace { unScriptSpace :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype MathSurround = MathSurround { unMathSurround :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype PreDisplaySize = PreDisplaySize { unPreDisplaySize :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype DisplayWidth = DisplayWidth { unDisplayWidth :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype DisplayIndent = DisplayIndent { unDisplayIndent :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype ParIndent = ParIndent { unParIndent :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype HangIndent = HangIndent { unHangIndent :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype HOffset = HOffset { unHOffset :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype VOffset = VOffset { unVOffset :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)

newtype BaselineSkip = BaselineSkip { unBaselineSkip :: GlueVariable} deriving (Show)
newtype LineSkip = LineSkip { unLineSkip :: GlueVariable} deriving (Show)
newtype ParSkip = ParSkip { unParSkip :: GlueVariable} deriving (Show)
newtype AboveDisplaySkip = AboveDisplaySkip { unAboveDisplaySkip :: GlueVariable} deriving (Show)
newtype AboveDisplayShortSkip = AboveDisplayShortSkip { unAboveDisplayShortSkip :: GlueVariable} deriving (Show)
newtype BelowDisplaySkip = BelowDisplaySkip { unBelowDisplaySkip :: GlueVariable} deriving (Show)
newtype BelowDisplayShortSkip = BelowDisplayShortSkip { unBelowDisplayShortSkip :: GlueVariable} deriving (Show)
newtype LeftSkip = LeftSkip { unLeftSkip :: GlueVariable} deriving (Show)
newtype RightSkip = RightSkip { unRightSkip :: GlueVariable} deriving (Show)
newtype TopSkip = TopSkip { unTopSkip :: GlueVariable} deriving (Show)
newtype SplitTopSkip = SplitTopSkip { unSplitTopSkip :: GlueVariable} deriving (Show)
newtype TabSkip = TabSkip { unTabSkip :: GlueVariable} deriving (Show)
newtype SpaceSkip = SpaceSkip { unSpaceSkip :: GlueVariable} deriving (Show)
newtype XSpaceSkip = XSpaceSkip { unXSpaceSkip :: GlueVariable} deriving (Show)
newtype ParFillSkip = ParFillSkip { unParFillSkip :: GlueVariable} deriving (Show)

-- newtype ThinMuSkip = ThinMuSkip { unThinMuSkip :: MathGlueVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
-- newtype MedMuSkip = MedMuSkip { unMedMuSkip :: MathGlueVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
-- newtype ThickMuSkip = ThickMuSkip { unThickMuSkip :: MathGlueVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)

-- newtype Output = Output { unOutput :: TokenListVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
-- newtype EveryPar = EveryPar { unEveryPar :: TokenListVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
-- newtype EveryMath = EveryMath { unEveryMath :: TokenListVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
-- newtype EveryDisplay = EveryDisplay { unEveryDisplay :: TokenListVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
-- newtype EveryHBox = EveryHBox { unEveryHBox :: TokenListVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
-- newtype EveryVBox = EveryVBox { unEveryVBox :: TokenListVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
-- newtype EveryJob = EveryJob { unEveryJob :: TokenListVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
-- newtype EveryCR = EveryCR { unEveryCR :: TokenListVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
-- newtype ErrHelp = ErrHelp { unErrHelp :: TokenListVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)

-- Special parameters.

newtype SpaceFactor = SpaceFactor { unSpaceFactor :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype PrevGraf = PrevGraf { unPrevGraf :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype DeadCycles = DeadCycles { unDeadCycles :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype InsertPenalties = InsertPenalties { unInsertPenalties :: IntegerVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)

newtype PrevDepth = PrevDepth { unPrevDepth :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype PageGoal = PageGoal { unPageGoal :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype PageTotal = PageTotal { unPageTotal :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype PageStretch = PageStretch { unPageStretch :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype PageFilStretch = PageFilStretch { unPageFilStretch :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype PageFillStretch = PageFillStretch { unPageFillStretch :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype PageFilllStretch = PageFilllStretch { unPageFilllStretch :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype PageShrink = PageShrink { unPageShrink :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)
newtype PageDepth = PageDepth { unPageDepth :: LengthVariable} deriving (Eq, Enum, Ord, Show, Num, Real, Integral)

data ParamConfig
  = ParamConfig { preTolerance :: PreTolerance
                , tolerance :: Tolerance
                , hBadness :: HBadness
                , vBadness :: VBadness
                , linePenalty :: LinePenalty
                , hyphenPenalty :: HyphenPenalty
                , exHyphenPenalty :: ExHyphenPenalty
                , binOpPenalty :: BinOpPenalty
                , relPenalty :: RelPenalty
                , clubPenalty :: ClubPenalty
                , widowPenalty :: WidowPenalty
                , displayWidowPenalty :: DisplayWidowPenalty
                , brokenPenalty :: BrokenPenalty
                , preDisplayPenalty :: PreDisplayPenalty
                , postDisplayPenalty :: PostDisplayPenalty
                , interlinePenalty :: InterlinePenalty
                , floatingPenalty :: FloatingPenalty
                , outputPenalty :: OutputPenalty
                , doubleHyphenDemerits :: DoubleHyphenDemerits
                , finalHyphenDemerits :: FinalHyphenDemerits
                , adjDemerits :: AdjDemerits
                , looseness :: Looseness
                , pausing :: Pausing
                , holdingInserts :: HoldingInserts
                , tracingOnline :: TracingOnline
                , tracingMacros :: TracingMacros
                , tracingStats :: TracingStats
                , tracingParagraphs :: TracingParagraphs
                , tracingPages :: TracingPages
                , tracingOutput :: TracingOutput
                , tracingLostChars :: TracingLostChars
                , tracingCommands :: TracingCommands
                , tracingRestores :: TracingRestores
                , language :: Language
                , uCHyph :: UCHyph
                , leftHyphenMin :: LeftHyphenMin
                , rightHyphenMin :: RightHyphenMin
                , globalDefs :: GlobalDefs
                , defaultHyphenChar :: DefaultHyphenChar
                , defaultSkewChar :: DefaultSkewChar
                , escapeChar :: EscapeChar
                , endLineChar :: EndLineChar
                , newLineChar :: NewLineChar
                , maxDeadCycles :: MaxDeadCycles
                , hangAfter :: HangAfter
                , fam :: Fam
                , mag :: Mag
                , delimiterFactor :: DelimiterFactor
                , time :: Time
                , day :: Day
                , month :: Month
                , year :: Year
                , showBoxBreadth :: ShowBoxBreadth
                , showBoxDepth :: ShowBoxDepth
                , errorContextLines :: ErrorContextLines

                , hFuzz :: HFuzz
                , vFuzz :: VFuzz
                , overfullRule :: OverfullRule
                , emergencyStretch :: EmergencyStretch
                , hSize :: HSize
                , vSize :: VSize
                , maxDepth :: MaxDepth
                , splitMaxDepth :: SplitMaxDepth
                , boxMaxDepth :: BoxMaxDepth
                , lineSkipLimit :: LineSkipLimit
                , delimiterShortfall :: DelimiterShortfall
                , nullDelimiterSpace :: NullDelimiterSpace
                , scriptSpace :: ScriptSpace
                , mathSurround :: MathSurround
                , preDisplaySize :: PreDisplaySize
                , displayWidth :: DisplayWidth
                , displayIndent :: DisplayIndent
                , parIndent :: ParIndent
                , hangIndent :: HangIndent
                , hOffset :: HOffset
                , vOffset :: VOffset

                , baselineSkip :: BaselineSkip
                , lineSkip :: LineSkip
                , parSkip :: ParSkip
                , aboveDisplaySkip :: AboveDisplaySkip
                , aboveDisplayShortSkip :: AboveDisplayShortSkip
                , belowDisplaySkip :: BelowDisplaySkip
                , belowDisplayShortSkip :: BelowDisplayShortSkip
                , leftSkip :: LeftSkip
                , rightSkip :: RightSkip
                , topSkip :: TopSkip
                , splitTopSkip :: SplitTopSkip
                , tabSkip :: TabSkip
                , spaceSkip :: SpaceSkip
                , xSpaceSkip :: XSpaceSkip
                , parFillSkip :: ParFillSkip

                -- , thinMuSkip :: ThinMuSkip
                -- , medMuSkip :: MedMuSkip
                -- , thickMuSkip :: ThickMuSkip

                -- , output :: Output
                -- , everyPar :: EveryPar
                -- , everyMath :: EveryMath
                -- , everyDisplay :: EveryDisplay
                -- , everyHBox :: EveryHBox
                -- , everyVBox :: EveryVBox
                -- , everyJob :: EveryJob
                -- , everyCR :: EveryCR
                -- , errHelp :: ErrHelp

                , spaceFactor :: SpaceFactor
                , prevGraf :: PrevGraf
                , deadCycles :: DeadCycles
                , insertPenalties :: InsertPenalties

                , prevDepth :: PrevDepth
                , pageGoal :: PageGoal
                , pageTotal :: PageTotal
                , pageStretch :: PageStretch
                , pageFilStretch :: PageFilStretch
                , pageFillStretch :: PageFillStretch
                , pageFilllStretch :: PageFilllStretch
                , pageShrink :: PageShrink
                , pageDepth :: PageDepth
                }

newParamConfig :: ParamConfig
newParamConfig
  = ParamConfig { preTolerance = PreTolerance 0
                , tolerance = Tolerance 10000
                , hBadness = HBadness 0
                , vBadness = VBadness 0
                , linePenalty = LinePenalty 0
                , hyphenPenalty = HyphenPenalty 0
                , exHyphenPenalty = ExHyphenPenalty 0
                , binOpPenalty = BinOpPenalty 0
                , relPenalty = RelPenalty 0
                , clubPenalty = ClubPenalty 0
                , widowPenalty = WidowPenalty 0
                , displayWidowPenalty = DisplayWidowPenalty 0
                , brokenPenalty = BrokenPenalty 0
                , preDisplayPenalty = PreDisplayPenalty 0
                , postDisplayPenalty = PostDisplayPenalty 0
                , interlinePenalty = InterlinePenalty 0
                , floatingPenalty = FloatingPenalty 0
                , outputPenalty = OutputPenalty 0
                , doubleHyphenDemerits = DoubleHyphenDemerits 0
                , finalHyphenDemerits = FinalHyphenDemerits 0
                , adjDemerits = AdjDemerits 0
                , looseness = Looseness 0
                , pausing = Pausing 0
                , holdingInserts = HoldingInserts 0
                , tracingOnline = TracingOnline 0
                , tracingMacros = TracingMacros 0
                , tracingStats = TracingStats 0
                , tracingParagraphs = TracingParagraphs 0
                , tracingPages = TracingPages 0
                , tracingOutput = TracingOutput 0
                , tracingLostChars = TracingLostChars 0
                , tracingCommands = TracingCommands 0
                , tracingRestores = TracingRestores 0
                , language = Language 0
                , uCHyph = UCHyph 0
                , leftHyphenMin = LeftHyphenMin 0
                , rightHyphenMin = RightHyphenMin 0
                , globalDefs = GlobalDefs 0
                , defaultHyphenChar = DefaultHyphenChar 0
                , defaultSkewChar = DefaultSkewChar 0
                , escapeChar = EscapeChar 92  -- '\'
                , endLineChar = EndLineChar 13 -- '\r'
                , newLineChar = NewLineChar 0
                , maxDeadCycles = MaxDeadCycles 25
                , hangAfter = HangAfter 1
                , fam = Fam 0
                , mag = Mag 1000
                , delimiterFactor = DelimiterFactor 0
                , time = Time 1
                , day = Day 1
                , month = Month 1
                , year = Year 1970
                , showBoxBreadth = ShowBoxBreadth 0
                , showBoxDepth = ShowBoxDepth 0
                , errorContextLines = ErrorContextLines 0

                , hFuzz = HFuzz 0
                , vFuzz = VFuzz 0
                , overfullRule = OverfullRule 0
                , emergencyStretch = EmergencyStretch 0
                , hSize = HSize 0
                , vSize = VSize 0
                , maxDepth = MaxDepth 0
                , splitMaxDepth = SplitMaxDepth 0
                , boxMaxDepth = BoxMaxDepth 0
                , lineSkipLimit = LineSkipLimit 0
                , delimiterShortfall = DelimiterShortfall 0
                , nullDelimiterSpace = NullDelimiterSpace 0
                , scriptSpace = ScriptSpace 0
                , mathSurround = MathSurround 0
                , preDisplaySize = PreDisplaySize 0
                , displayWidth = DisplayWidth 0
                , displayIndent = DisplayIndent 0
                , parIndent = ParIndent 0
                , hangIndent = HangIndent 0
                , hOffset = HOffset 0
                , vOffset = VOffset 0

                , baselineSkip = BaselineSkip mempty
                , lineSkip = LineSkip mempty
                , parSkip = ParSkip mempty
                , aboveDisplaySkip = AboveDisplaySkip mempty
                , aboveDisplayShortSkip = AboveDisplayShortSkip mempty
                , belowDisplaySkip = BelowDisplaySkip mempty
                , belowDisplayShortSkip = BelowDisplayShortSkip mempty
                , leftSkip = LeftSkip mempty
                , rightSkip = RightSkip mempty
                , topSkip = TopSkip mempty
                , splitTopSkip = SplitTopSkip mempty
                , tabSkip = TabSkip mempty
                , spaceSkip = SpaceSkip mempty
                , xSpaceSkip = XSpaceSkip mempty
                , parFillSkip = ParFillSkip mempty

                -- , thinMuSkip :: ThinMuSkip
                -- , medMuSkip :: MedMuSkip
                -- , thickMuSkip :: ThickMuSkip

                -- , output :: Output
                -- , everyPar :: EveryPar
                -- , everyMath :: EveryMath
                -- , everyDisplay :: EveryDisplay
                -- , everyHBox :: EveryHBox
                -- , everyVBox :: EveryVBox
                -- , everyJob :: EveryJob
                -- , everyCR :: EveryCR
                -- , errHelp :: ErrHelp

                , spaceFactor = SpaceFactor 0
                , prevGraf = PrevGraf 0
                , deadCycles = DeadCycles 0
                , insertPenalties = InsertPenalties 0

                , prevDepth = PrevDepth $ -Unit.oneKPt
                , pageGoal = PageGoal 0
                , pageTotal = PageTotal 0
                , pageStretch = PageStretch 0
                , pageFilStretch = PageFilStretch 0
                , pageFillStretch = PageFillStretch 0
                , pageFilllStretch = PageFilllStretch 0
                , pageShrink = PageShrink 0
                , pageDepth = PageDepth 0
                }

usableParamConfig
  = newParamConfig { tolerance = Tolerance 500
                   , linePenalty = LinePenalty 10
                   , mag = Mag 1000
                   , hSize = HSize 30750000
                   , vSize = VSize 37500000
                   , lineSkipLimit = LineSkipLimit 0
                   , parIndent = ParIndent $ Unit.toScaledPointApprox (20 :: Int) Unit.Point
                   , baselineSkip = BaselineSkip $ Glue (Unit.toScaledPointApprox (12 :: Int) Unit.Point) noFlex noFlex
                   , lineSkip = LineSkip $ Glue (Unit.toScaledPointApprox (1 :: Int) Unit.Point) noFlex noFlex
  }
