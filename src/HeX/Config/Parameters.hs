module HeX.Config.Parameters where

import           HeXlude

import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map

import qualified HeX.BreakList.Glue  as BL.G
import           HeX.Parse.Token
import           HeX.Quantity

newtype IntParamVal a = IntParamVal { unIntParam :: TeXInt }
    deriving ( Eq, Enum, Ord, Show, Num, Real, Integral )

newtype LenParamVal a = LenParamVal { unLenParam :: Length }
    deriving ( Eq, Enum, Ord, Show, Num, Real, Integral )

newtype GlueParamVal a = GlueParamVal { unGlueParam :: BL.G.Glue Length }
    deriving ( Show )

newtype MathGlueParamVal a = MathGlueParamVal { unMathGlueParam :: BL.G.Glue MathLength }
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

newTeXIntParameters :: Map TeXIntParameter TeXInt
newTeXIntParameters =
    Map.fromList
        [ (Tolerance, 10000)
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

newLengthParameters :: Map LengthParameter Length
newLengthParameters = mempty

newGlueParameters :: Map GlueParameter (BL.G.Glue Length)
newGlueParameters = mempty

newMathGlueParameters :: Map MathGlueParameter (BL.G.Glue MathLength)
newMathGlueParameters = mempty

newTokenListParameters :: Map TokenListParameter BalancedText
newTokenListParameters = mempty

newSpecialTeXInts :: Map SpecialTeXInt TeXInt
newSpecialTeXInts = mempty

newSpecialLengths :: Map SpecialLength Length
newSpecialLengths = Map.fromList [ (PrevDepth, fromIntegral $ -oneKPt) ]

usableTeXIntParameters :: Map TeXIntParameter TeXInt
usableTeXIntParameters =
    let vm = Map.fromList [ (Tolerance, 500), (LinePenalty, 10), (Mag, 1000) ]
    in
        Map.union vm newTeXIntParameters

usableLengthParameters :: Map LengthParameter Length
usableLengthParameters =
    let vm = Map.fromList [ (HSize, 30750000)
                           , (VSize, 37500000)
                           , ( ParIndent
                             , toScaledPointApprox (20 :: Int) Point
                             )
                           ]
    in
        Map.union vm newLengthParameters

usableGlueParameters :: Map GlueParameter (BL.G.Glue Length)
usableGlueParameters =
    let vm = Map.fromList [ ( BaselineSkip
                             , BL.G.fixedGlue $
                                   toScaledPointApprox (12 :: Int)
                                                            Point
                             )
                           , ( LineSkip
                             , BL.G.fixedGlue $
                                   toScaledPointApprox (1 :: Int)
                                                            Point
                             )
                           ]
    in
        Map.union vm newGlueParameters
