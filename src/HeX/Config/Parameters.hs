module HeX.Config.Parameters where

import           HeXlude

import qualified Data.HashMap.Strict as HMap

import           HeX.BreakList.Glue  ( Glue(..), MathGlue, fixedGlue )
import           HeX.Parse.Token
import qualified HeX.Unit            as Unit

newtype IntParamVal a = IntParamVal { unIntParam :: TeXIntVal }
    deriving ( Eq, Enum, Ord, Show, Num, Real, Integral )

newtype LenParamVal a = LenParamVal { unLenParam :: LenVal }
    deriving ( Eq, Enum, Ord, Show, Num, Real, Integral )

newtype GlueParamVal a = GlueParamVal { unGlueParam :: Glue }
    deriving ( Show )

newtype MathGlueParamVal a = MathGlueParamVal { unMathGlueParam :: MathGlue }
    deriving ( Show )

newtype TokenListParamVal a =
    TokenListParamVal { unTokenListParam :: BalancedText }
    deriving ( Show )

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

data ThinMuSkip

data MedMuSkip

data ThickMuSkip

data Output

data EveryPar

data EveryMath

data EveryDisplay

data EveryHBox

data EveryVBox

data EveryJob

data EveryCR

data ErrHelp

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

newTeXIntParameters :: HMap.HashMap TeXIntParameter TeXIntVal
newTeXIntParameters =
    HMap.fromList [ (Tolerance, 10000)
                  , (EscapeChar, 92)  -- '\'
                  , (EndLineChar, 13)  -- '\r'
                  , (MaxDeadCycles, 25)
                  , (HangAfter, 1)
                  , (Mag, 1000)
                  , (Time, 1)
                  , (Day, 1)
                  , (Month, 1)
                  , (Year, 1970)
                  ]

newLengthParameters :: HMap.HashMap LengthParameter LenVal
newLengthParameters = HMap.empty

newGlueParameters :: HMap.HashMap GlueParameter Glue
newGlueParameters = HMap.empty

newMathGlueParameters :: HMap.HashMap MathGlueParameter MathGlue
newMathGlueParameters = HMap.empty

newTokenListParameters :: HMap.HashMap TokenListParameter BalancedText
newTokenListParameters = HMap.empty

newSpecialTeXInts :: HMap.HashMap SpecialTeXInt TeXIntVal
newSpecialTeXInts = HMap.empty

newSpecialLengths :: HMap.HashMap SpecialLength TeXIntVal
newSpecialLengths = HMap.fromList [ (PrevDepth, fromIntegral $ -Unit.oneKPt) ]

usableTeXIntParameters :: HMap.HashMap TeXIntParameter TeXIntVal
usableTeXIntParameters =
    let vm = HMap.fromList [ (Tolerance, 500), (LinePenalty, 10), (Mag, 1000) ]
    in
        HMap.union vm newTeXIntParameters

usableLengthParameters :: HMap.HashMap LengthParameter LenVal
usableLengthParameters =
    let vm = HMap.fromList [ (HSize, 30750000)
                           , (VSize, 37500000)
                           , ( ParIndent
                             , Unit.toScaledPointApprox (20 :: Int) Unit.Point
                             )
                           ]
    in
        HMap.union vm newLengthParameters

usableGlueParameters :: HMap.HashMap GlueParameter Glue
usableGlueParameters =
    let vm = HMap.fromList [ ( BaselineSkip
                             , fixedGlue $
                                   Unit.toScaledPointApprox (12 :: Int)
                                                            Unit.Point
                             )
                           , ( LineSkip
                             , fixedGlue $
                                   Unit.toScaledPointApprox (1 :: Int)
                                                            Unit.Point
                             )
                           ]
    in
        HMap.union vm newGlueParameters
