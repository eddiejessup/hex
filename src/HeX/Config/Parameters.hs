{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HeX.Config.Parameters where

import           HeX.Type
import qualified HeX.Lex                       as Lex
import           HeX.BreakList                  ( Glue(..), noFlex )
import qualified HeX.Unit                      as Unit
import           HeX.Parse.Token

newtype IntParamVal a = IntParamVal {unIntParam :: IntVal}
    deriving (Eq, Enum, Ord, Show, Num, Real, Integral)

newtype LenParamVal a = LenParamVal {unLenParam :: LenVal}
    deriving (Eq, Enum, Ord, Show, Num, Real, Integral)

newtype GlueParamVal a = GlueParamVal {unGlueParam :: Glue}
    deriving (Show)

newtype MathGlueParamVal a = MathGlueParamVal {unMathGlueParam :: Glue}
    deriving (Show)

newtype TokenListParamVal a = TokenListParamVal {unTokenListParam :: [Lex.Token]}
    deriving (Show)

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

data ParamConfig = ParamConfig
    { preTolerance          :: IntParamVal PreTolerance
    , tolerance             :: IntParamVal Tolerance
    , hBadness              :: IntParamVal HBadness
    , vBadness              :: IntParamVal VBadness
    , linePenalty           :: IntParamVal LinePenalty
    , hyphenPenalty         :: IntParamVal HyphenPenalty
    , exHyphenPenalty       :: IntParamVal ExHyphenPenalty
    , binOpPenalty          :: IntParamVal BinOpPenalty
    , relPenalty            :: IntParamVal RelPenalty
    , clubPenalty           :: IntParamVal ClubPenalty
    , widowPenalty          :: IntParamVal WidowPenalty
    , displayWidowPenalty   :: IntParamVal DisplayWidowPenalty
    , brokenPenalty         :: IntParamVal BrokenPenalty
    , preDisplayPenalty     :: IntParamVal PreDisplayPenalty
    , postDisplayPenalty    :: IntParamVal PostDisplayPenalty
    , interlinePenalty      :: IntParamVal InterlinePenalty
    , floatingPenalty       :: IntParamVal FloatingPenalty
    , outputPenalty         :: IntParamVal OutputPenalty
    , doubleHyphenDemerits  :: IntParamVal DoubleHyphenDemerits
    , finalHyphenDemerits   :: IntParamVal FinalHyphenDemerits
    , adjDemerits           :: IntParamVal AdjDemerits
    , looseness             :: IntParamVal Looseness
    , pausing               :: IntParamVal Pausing
    , holdingInserts        :: IntParamVal HoldingInserts
    , tracingOnline         :: IntParamVal TracingOnline
    , tracingMacros         :: IntParamVal TracingMacros
    , tracingStats          :: IntParamVal TracingStats
    , tracingParagraphs     :: IntParamVal TracingParagraphs
    , tracingPages          :: IntParamVal TracingPages
    , tracingOutput         :: IntParamVal TracingOutput
    , tracingLostChars      :: IntParamVal TracingLostChars
    , tracingCommands       :: IntParamVal TracingCommands
    , tracingRestores       :: IntParamVal TracingRestores
    , language              :: IntParamVal Language
    , uCHyph                :: IntParamVal UCHyph
    , leftHyphenMin         :: IntParamVal LeftHyphenMin
    , rightHyphenMin        :: IntParamVal RightHyphenMin
    , globalDefs            :: IntParamVal GlobalDefs
    , defaultHyphenChar     :: IntParamVal DefaultHyphenChar
    , defaultSkewChar       :: IntParamVal DefaultSkewChar
    , escapeChar            :: IntParamVal EscapeChar
    , endLineChar           :: IntParamVal EndLineChar
    , newLineChar           :: IntParamVal NewLineChar
    , maxDeadCycles         :: IntParamVal MaxDeadCycles
    , hangAfter             :: IntParamVal HangAfter
    , fam                   :: IntParamVal Fam
    , mag                   :: IntParamVal Mag
    , delimiterFactor       :: IntParamVal DelimiterFactor
    , time                  :: IntParamVal Time
    , day                   :: IntParamVal Day
    , month                 :: IntParamVal Month
    , year                  :: IntParamVal Year
    , showBoxBreadth        :: IntParamVal ShowBoxBreadth
    , showBoxDepth          :: IntParamVal ShowBoxDepth
    , errorContextLines     :: IntParamVal ErrorContextLines

    , hFuzz                 :: LenParamVal HFuzz
    , vFuzz                 :: LenParamVal VFuzz
    , overfullRule          :: LenParamVal OverfullRule
    , emergencyStretch      :: LenParamVal EmergencyStretch
    , hSize                 :: LenParamVal HSize
    , vSize                 :: LenParamVal VSize
    , maxDepth              :: LenParamVal MaxDepth
    , splitMaxDepth         :: LenParamVal SplitMaxDepth
    , boxMaxDepth           :: LenParamVal BoxMaxDepth
    , lineSkipLimit         :: LenParamVal LineSkipLimit
    , delimiterShortfall    :: LenParamVal DelimiterShortfall
    , nullDelimiterSpace    :: LenParamVal NullDelimiterSpace
    , scriptSpace           :: LenParamVal ScriptSpace
    , mathSurround          :: LenParamVal MathSurround
    , preDisplaySize        :: LenParamVal PreDisplaySize
    , displayWidth          :: LenParamVal DisplayWidth
    , displayIndent         :: LenParamVal DisplayIndent
    , parIndent             :: LenParamVal ParIndent
    , hangIndent            :: LenParamVal HangIndent
    , hOffset               :: LenParamVal HOffset
    , vOffset               :: LenParamVal VOffset

    , baselineSkip          :: GlueParamVal BaselineSkip
    , lineSkip              :: GlueParamVal LineSkip
    , parSkip               :: GlueParamVal ParSkip
    , aboveDisplaySkip      :: GlueParamVal AboveDisplaySkip
    , aboveDisplayShortSkip :: GlueParamVal AboveDisplayShortSkip
    , belowDisplaySkip      :: GlueParamVal BelowDisplaySkip
    , belowDisplayShortSkip :: GlueParamVal BelowDisplayShortSkip
    , leftSkip              :: GlueParamVal LeftSkip
    , rightSkip             :: GlueParamVal RightSkip
    , topSkip               :: GlueParamVal TopSkip
    , splitTopSkip          :: GlueParamVal SplitTopSkip
    , tabSkip               :: GlueParamVal TabSkip
    , spaceSkip             :: GlueParamVal SpaceSkip
    , xSpaceSkip            :: GlueParamVal XSpaceSkip
    , parFillSkip           :: GlueParamVal ParFillSkip

    , thinMuSkip            :: MathGlueParamVal ThinMuSkip
    , medMuSkip             :: MathGlueParamVal MedMuSkip
    , thickMuSkip           :: MathGlueParamVal ThickMuSkip

    , output                :: TokenListParamVal Output
    , everyPar              :: TokenListParamVal EveryPar
    , everyMath             :: TokenListParamVal EveryMath
    , everyDisplay          :: TokenListParamVal EveryDisplay
    , everyHBox             :: TokenListParamVal EveryHBox
    , everyVBox             :: TokenListParamVal EveryVBox
    , everyJob              :: TokenListParamVal EveryJob
    , everyCR               :: TokenListParamVal EveryCR
    , errHelp               :: TokenListParamVal ErrHelp

    , spaceFactor           :: IntParamVal SpaceFactor
    , prevGraf              :: IntParamVal PrevGraf
    , deadCycles            :: IntParamVal DeadCycles
    , insertPenalties       :: IntParamVal InsertPenalties

    , prevDepth             :: LenParamVal PrevDepth
    , pageGoal              :: LenParamVal PageGoal
    , pageTotal             :: LenParamVal PageTotal
    , pageStretch           :: LenParamVal PageStretch
    , pageFilStretch        :: LenParamVal PageFilStretch
    , pageFillStretch       :: LenParamVal PageFillStretch
    , pageFilllStretch      :: LenParamVal PageFilllStretch
    , pageShrink            :: LenParamVal PageShrink
    , pageDepth             :: LenParamVal PageDepth
    }

newParamConfig :: ParamConfig
newParamConfig              = ParamConfig
    { preTolerance          = IntParamVal 0
    , tolerance             = IntParamVal 10000
    , hBadness              = IntParamVal 0
    , vBadness              = IntParamVal 0
    , linePenalty           = IntParamVal 0
    , hyphenPenalty         = IntParamVal 0
    , exHyphenPenalty       = IntParamVal 0
    , binOpPenalty          = IntParamVal 0
    , relPenalty            = IntParamVal 0
    , clubPenalty           = IntParamVal 0
    , widowPenalty          = IntParamVal 0
    , displayWidowPenalty   = IntParamVal 0
    , brokenPenalty         = IntParamVal 0
    , preDisplayPenalty     = IntParamVal 0
    , postDisplayPenalty    = IntParamVal 0
    , interlinePenalty      = IntParamVal 0
    , floatingPenalty       = IntParamVal 0
    , outputPenalty         = IntParamVal 0
    , doubleHyphenDemerits  = IntParamVal 0
    , finalHyphenDemerits   = IntParamVal 0
    , adjDemerits           = IntParamVal 0
    , looseness             = IntParamVal 0
    , pausing               = IntParamVal 0
    , holdingInserts        = IntParamVal 0
    , tracingOnline         = IntParamVal 0
    , tracingMacros         = IntParamVal 0
    , tracingStats          = IntParamVal 0
    , tracingParagraphs     = IntParamVal 0
    , tracingPages          = IntParamVal 0
    , tracingOutput         = IntParamVal 0
    , tracingLostChars      = IntParamVal 0
    , tracingCommands       = IntParamVal 0
    , tracingRestores       = IntParamVal 0
    , language              = IntParamVal 0
    , uCHyph                = IntParamVal 0
    , leftHyphenMin         = IntParamVal 0
    , rightHyphenMin        = IntParamVal 0
    , globalDefs            = IntParamVal 0
    , defaultHyphenChar     = IntParamVal 0
    , defaultSkewChar       = IntParamVal 0
    , escapeChar            = IntParamVal 92  -- '\'
    , endLineChar           = IntParamVal 13  -- '\r'
    , newLineChar           = IntParamVal 0
    , maxDeadCycles         = IntParamVal 25
    , hangAfter             = IntParamVal 1
    , fam                   = IntParamVal 0
    , mag                   = IntParamVal 1000
    , delimiterFactor       = IntParamVal 0
    , time                  = IntParamVal 1
    , day                   = IntParamVal 1
    , month                 = IntParamVal 1
    , year                  = IntParamVal 1970
    , showBoxBreadth        = IntParamVal 0
    , showBoxDepth          = IntParamVal 0
    , errorContextLines     = IntParamVal 0

    , hFuzz                 = LenParamVal 0
    , vFuzz                 = LenParamVal 0
    , overfullRule          = LenParamVal 0
    , emergencyStretch      = LenParamVal 0
    , hSize                 = LenParamVal 0
    , vSize                 = LenParamVal 0
    , maxDepth              = LenParamVal 0
    , splitMaxDepth         = LenParamVal 0
    , boxMaxDepth           = LenParamVal 0
    , lineSkipLimit         = LenParamVal 0
    , delimiterShortfall    = LenParamVal 0
    , nullDelimiterSpace    = LenParamVal 0
    , scriptSpace           = LenParamVal 0
    , mathSurround          = LenParamVal 0
    , preDisplaySize        = LenParamVal 0
    , displayWidth          = LenParamVal 0
    , displayIndent         = LenParamVal 0
    , parIndent             = LenParamVal 0
    , hangIndent            = LenParamVal 0
    , hOffset               = LenParamVal 0
    , vOffset               = LenParamVal 0

    , baselineSkip          = GlueParamVal mempty
    , lineSkip              = GlueParamVal mempty
    , parSkip               = GlueParamVal mempty
    , aboveDisplaySkip      = GlueParamVal mempty
    , aboveDisplayShortSkip = GlueParamVal mempty
    , belowDisplaySkip      = GlueParamVal mempty
    , belowDisplayShortSkip = GlueParamVal mempty
    , leftSkip              = GlueParamVal mempty
    , rightSkip             = GlueParamVal mempty
    , topSkip               = GlueParamVal mempty
    , splitTopSkip          = GlueParamVal mempty
    , tabSkip               = GlueParamVal mempty
    , spaceSkip             = GlueParamVal mempty
    , xSpaceSkip            = GlueParamVal mempty
    , parFillSkip           = GlueParamVal mempty

    , thinMuSkip            = MathGlueParamVal mempty
    , medMuSkip             = MathGlueParamVal mempty
    , thickMuSkip           = MathGlueParamVal mempty

    , output                = TokenListParamVal mempty
    , everyPar              = TokenListParamVal mempty
    , everyMath             = TokenListParamVal mempty
    , everyDisplay          = TokenListParamVal mempty
    , everyHBox             = TokenListParamVal mempty
    , everyVBox             = TokenListParamVal mempty
    , everyJob              = TokenListParamVal mempty
    , everyCR               = TokenListParamVal mempty
    , errHelp               = TokenListParamVal mempty

    , spaceFactor           = IntParamVal 0
    , prevGraf              = IntParamVal 0
    , deadCycles            = IntParamVal 0
    , insertPenalties       = IntParamVal 0

    , prevDepth             = LenParamVal $ fromIntegral $ -Unit.oneKPt
    , pageGoal              = LenParamVal 0
    , pageTotal             = LenParamVal 0
    , pageStretch           = LenParamVal 0
    , pageFilStretch        = LenParamVal 0
    , pageFillStretch       = LenParamVal 0
    , pageFilllStretch      = LenParamVal 0
    , pageShrink            = LenParamVal 0
    , pageDepth             = LenParamVal 0
    }

usableParamConfig :: ParamConfig
usableParamConfig           = newParamConfig
    { tolerance             = IntParamVal 500
    , linePenalty           = IntParamVal 10
    , mag                   = IntParamVal 1000
    , hSize                 = LenParamVal 30750000
    , vSize                 = LenParamVal 37500000
    , lineSkipLimit         = LenParamVal 0
    , parIndent             = LenParamVal $ Unit.toScaledPointApprox (20 :: Int) Unit.Point
    , baselineSkip          = GlueParamVal $ Glue (Unit.toScaledPointApprox (12 :: Int) Unit.Point) noFlex noFlex
    , lineSkip              = GlueParamVal $ Glue (Unit.toScaledPointApprox (1 :: Int) Unit.Point) noFlex noFlex
    }

getIntParam :: IntegerParameter -> ParamConfig -> IntVal
getIntParam p c = f c
  where
    f = case p of
        PreTolerance          -> unIntParam . preTolerance
        Tolerance             -> unIntParam . tolerance
        HBadness              -> unIntParam . hBadness
        VBadness              -> unIntParam . vBadness
        LinePenalty           -> unIntParam . linePenalty
        HyphenPenalty         -> unIntParam . hyphenPenalty
        ExHyphenPenalty       -> unIntParam . exHyphenPenalty
        BinOpPenalty          -> unIntParam . binOpPenalty
        RelPenalty            -> unIntParam . relPenalty
        ClubPenalty           -> unIntParam . clubPenalty
        WidowPenalty          -> unIntParam . widowPenalty
        DisplayWidowPenalty   -> unIntParam . displayWidowPenalty
        BrokenPenalty         -> unIntParam . brokenPenalty
        PreDisplayPenalty     -> unIntParam . preDisplayPenalty
        PostDisplayPenalty    -> unIntParam . postDisplayPenalty
        InterlinePenalty      -> unIntParam . interlinePenalty
        FloatingPenalty       -> unIntParam . floatingPenalty
        OutputPenalty         -> unIntParam . outputPenalty
        DoubleHyphenDemerits  -> unIntParam . doubleHyphenDemerits
        FinalHyphenDemerits   -> unIntParam . finalHyphenDemerits
        AdjDemerits           -> unIntParam . adjDemerits
        Looseness             -> unIntParam . looseness
        Pausing               -> unIntParam . pausing
        HoldingInserts        -> unIntParam . holdingInserts
        TracingOnline         -> unIntParam . tracingOnline
        TracingMacros         -> unIntParam . tracingMacros
        TracingStats          -> unIntParam . tracingStats
        TracingParagraphs     -> unIntParam . tracingParagraphs
        TracingPages          -> unIntParam . tracingPages
        TracingOutput         -> unIntParam . tracingOutput
        TracingLostChars      -> unIntParam . tracingLostChars
        TracingCommands       -> unIntParam . tracingCommands
        TracingRestores       -> unIntParam . tracingRestores
        Language              -> unIntParam . language
        UCHyph                -> unIntParam . uCHyph
        LeftHyphenMin         -> unIntParam . leftHyphenMin
        RightHyphenMin        -> unIntParam . rightHyphenMin
        GlobalDefs            -> unIntParam . globalDefs
        DefaultHyphenChar     -> unIntParam . defaultHyphenChar
        DefaultSkewChar       -> unIntParam . defaultSkewChar
        EscapeChar            -> unIntParam . escapeChar
        EndLineChar           -> unIntParam . endLineChar
        NewLineChar           -> unIntParam . newLineChar
        MaxDeadCycles         -> unIntParam . maxDeadCycles
        HangAfter             -> unIntParam . hangAfter
        Fam                   -> unIntParam . fam
        Mag                   -> unIntParam . mag
        DelimiterFactor       -> unIntParam . delimiterFactor
        Time                  -> unIntParam . time
        Day                   -> unIntParam . day
        Month                 -> unIntParam . month
        Year                  -> unIntParam . year
        ShowBoxBreadth        -> unIntParam . showBoxBreadth
        ShowBoxDepth          -> unIntParam . showBoxDepth
        ErrorContextLines     -> unIntParam . errorContextLines

setIntParam :: IntegerParameter -> IntVal -> ParamConfig -> ParamConfig
setIntParam p v c =
    let vp = IntParamVal v
    in case p of
        PreTolerance          -> c{preTolerance=vp}
        Tolerance             -> c{tolerance=vp}
        HBadness              -> c{hBadness=vp}
        VBadness              -> c{vBadness=vp}
        LinePenalty           -> c{linePenalty=vp}
        HyphenPenalty         -> c{hyphenPenalty=vp}
        ExHyphenPenalty       -> c{exHyphenPenalty=vp}
        BinOpPenalty          -> c{binOpPenalty=vp}
        RelPenalty            -> c{relPenalty=vp}
        ClubPenalty           -> c{clubPenalty=vp}
        WidowPenalty          -> c{widowPenalty=vp}
        DisplayWidowPenalty   -> c{displayWidowPenalty=vp}
        BrokenPenalty         -> c{brokenPenalty=vp}
        PreDisplayPenalty     -> c{preDisplayPenalty=vp}
        PostDisplayPenalty    -> c{postDisplayPenalty=vp}
        InterlinePenalty      -> c{interlinePenalty=vp}
        FloatingPenalty       -> c{floatingPenalty=vp}
        OutputPenalty         -> c{outputPenalty=vp}
        DoubleHyphenDemerits  -> c{doubleHyphenDemerits=vp}
        FinalHyphenDemerits   -> c{finalHyphenDemerits=vp}
        AdjDemerits           -> c{adjDemerits=vp}
        Looseness             -> c{looseness=vp}
        Pausing               -> c{pausing=vp}
        HoldingInserts        -> c{holdingInserts=vp}
        TracingOnline         -> c{tracingOnline=vp}
        TracingMacros         -> c{tracingMacros=vp}
        TracingStats          -> c{tracingStats=vp}
        TracingParagraphs     -> c{tracingParagraphs=vp}
        TracingPages          -> c{tracingPages=vp}
        TracingOutput         -> c{tracingOutput=vp}
        TracingLostChars      -> c{tracingLostChars=vp}
        TracingCommands       -> c{tracingCommands=vp}
        TracingRestores       -> c{tracingRestores=vp}
        Language              -> c{language=vp}
        UCHyph                -> c{uCHyph=vp}
        LeftHyphenMin         -> c{leftHyphenMin=vp}
        RightHyphenMin        -> c{rightHyphenMin=vp}
        GlobalDefs            -> c{globalDefs=vp}
        DefaultHyphenChar     -> c{defaultHyphenChar=vp}
        DefaultSkewChar       -> c{defaultSkewChar=vp}
        EscapeChar            -> c{escapeChar=vp}
        EndLineChar           -> c{endLineChar=vp}
        NewLineChar           -> c{newLineChar=vp}
        MaxDeadCycles         -> c{maxDeadCycles=vp}
        HangAfter             -> c{hangAfter=vp}
        Fam                   -> c{fam=vp}
        Mag                   -> c{mag=vp}
        DelimiterFactor       -> c{delimiterFactor=vp}
        Time                  -> c{time=vp}
        Day                   -> c{day=vp}
        Month                 -> c{month=vp}
        Year                  -> c{year=vp}
        ShowBoxBreadth        -> c{showBoxBreadth=vp}
        ShowBoxDepth          -> c{showBoxDepth=vp}
        ErrorContextLines     -> c{errorContextLines=vp}

getLenParam :: LengthParameter -> ParamConfig -> LenVal
getLenParam p c = f c
  where
    f = case p of
        HFuzz                 -> unLenParam . hFuzz
        VFuzz                 -> unLenParam . vFuzz
        OverfullRule          -> unLenParam . overfullRule
        EmergencyStretch      -> unLenParam . emergencyStretch
        HSize                 -> unLenParam . hSize
        VSize                 -> unLenParam . vSize
        MaxDepth              -> unLenParam . maxDepth
        SplitMaxDepth         -> unLenParam . splitMaxDepth
        BoxMaxDepth           -> unLenParam . boxMaxDepth
        LineSkipLimit         -> unLenParam . lineSkipLimit
        DelimiterShortfall    -> unLenParam . delimiterShortfall
        NullDelimiterSpace    -> unLenParam . nullDelimiterSpace
        ScriptSpace           -> unLenParam . scriptSpace
        MathSurround          -> unLenParam . mathSurround
        PreDisplaySize        -> unLenParam . preDisplaySize
        DisplayWidth          -> unLenParam . displayWidth
        DisplayIndent         -> unLenParam . displayIndent
        ParIndent             -> unLenParam . parIndent
        HangIndent            -> unLenParam . hangIndent
        HOffset               -> unLenParam . hOffset
        VOffset               -> unLenParam . vOffset

setLenParam :: LengthParameter -> LenVal -> ParamConfig -> ParamConfig
setLenParam p v c =
    let vp = LenParamVal v
    in case p of
        HFuzz                 -> c{hFuzz=vp}
        VFuzz                 -> c{vFuzz=vp}
        OverfullRule          -> c{overfullRule=vp}
        EmergencyStretch      -> c{emergencyStretch=vp}
        HSize                 -> c{hSize=vp}
        VSize                 -> c{vSize=vp}
        MaxDepth              -> c{maxDepth=vp}
        SplitMaxDepth         -> c{splitMaxDepth=vp}
        BoxMaxDepth           -> c{boxMaxDepth=vp}
        LineSkipLimit         -> c{lineSkipLimit=vp}
        DelimiterShortfall    -> c{delimiterShortfall=vp}
        NullDelimiterSpace    -> c{nullDelimiterSpace=vp}
        ScriptSpace           -> c{scriptSpace=vp}
        MathSurround          -> c{mathSurround=vp}
        PreDisplaySize        -> c{preDisplaySize=vp}
        DisplayWidth          -> c{displayWidth=vp}
        DisplayIndent         -> c{displayIndent=vp}
        ParIndent             -> c{parIndent=vp}
        HangIndent            -> c{hangIndent=vp}
        HOffset               -> c{hOffset=vp}
        VOffset               -> c{vOffset=vp}

getGlueParam :: GlueParameter -> ParamConfig -> Glue
getGlueParam p c = f c
  where
    f = case p of
        BaselineSkip          -> unGlueParam . baselineSkip
        LineSkip              -> unGlueParam . lineSkip
        ParSkip               -> unGlueParam . parSkip
        AboveDisplaySkip      -> unGlueParam . aboveDisplaySkip
        AboveDisplayShortSkip -> unGlueParam . aboveDisplayShortSkip
        BelowDisplaySkip      -> unGlueParam . belowDisplaySkip
        BelowDisplayShortSkip -> unGlueParam . belowDisplayShortSkip
        LeftSkip              -> unGlueParam . leftSkip
        RightSkip             -> unGlueParam . rightSkip
        TopSkip               -> unGlueParam . topSkip
        SplitTopSkip          -> unGlueParam . splitTopSkip
        TabSkip               -> unGlueParam . tabSkip
        SpaceSkip             -> unGlueParam . spaceSkip
        XSpaceSkip            -> unGlueParam . xSpaceSkip
        ParFillSkip           -> unGlueParam . parFillSkip

setGlueParam :: GlueParameter -> Glue -> ParamConfig -> ParamConfig
setGlueParam p v c =
    let vp = GlueParamVal v
    in case p of
        BaselineSkip          -> c{baselineSkip=vp}
        LineSkip              -> c{lineSkip=vp}
        ParSkip               -> c{parSkip=vp}
        AboveDisplaySkip      -> c{aboveDisplaySkip=vp}
        AboveDisplayShortSkip -> c{aboveDisplayShortSkip=vp}
        BelowDisplaySkip      -> c{belowDisplaySkip=vp}
        BelowDisplayShortSkip -> c{belowDisplayShortSkip=vp}
        LeftSkip              -> c{leftSkip=vp}
        RightSkip             -> c{rightSkip=vp}
        TopSkip               -> c{topSkip=vp}
        SplitTopSkip          -> c{splitTopSkip=vp}
        TabSkip               -> c{tabSkip=vp}
        SpaceSkip             -> c{spaceSkip=vp}
        XSpaceSkip            -> c{xSpaceSkip=vp}
        ParFillSkip           -> c{parFillSkip=vp}

-- setMathGlueParam :: MathGlueParameter -> MathGlue -> ParamConfig -> ParamConfig
-- setMathGlueParam p v c = let vp = MathGlueParamVal v in case p of

-- setTokenListParam :: TokenListParameter -> [Token] -> ParamConfig -> ParamConfig
-- setTokenListParam p v c = let vp = TokenListParamVal v in case p of

getSpecialInt :: SpecialInteger -> ParamConfig -> IntVal
getSpecialInt p c = f c
  where
    f = case p of
        SpaceFactor           -> unIntParam . spaceFactor
        PrevGraf              -> unIntParam . prevGraf
        DeadCycles            -> unIntParam . deadCycles
        InsertPenalties       -> unIntParam . insertPenalties

setSpecialInt :: SpecialInteger -> IntVal -> ParamConfig -> ParamConfig
setSpecialInt p v c =
    let vp = IntParamVal v
    in case p of
        SpaceFactor           -> c{spaceFactor=vp}
        PrevGraf              -> c{prevGraf=vp}
        DeadCycles            -> c{deadCycles=vp}
        InsertPenalties       -> c{insertPenalties=vp}

getSpecialLen :: SpecialLength -> ParamConfig -> LenVal
getSpecialLen p c = f c
  where
    f = case p of
        PrevDepth             -> unLenParam . prevDepth
        PageGoal              -> unLenParam . pageGoal
        PageTotal             -> unLenParam . pageTotal
        PageStretch           -> unLenParam . pageStretch
        PageFilStretch        -> unLenParam . pageFilStretch
        PageFillStretch       -> unLenParam . pageFillStretch
        PageFilllStretch      -> unLenParam . pageFilllStretch
        PageShrink            -> unLenParam . pageShrink
        PageDepth             -> unLenParam . pageDepth

setSpecialLen :: SpecialLength -> LenVal -> ParamConfig -> ParamConfig
setSpecialLen p v c =
    let vp = LenParamVal v
    in case p of
        PrevDepth             -> c{prevDepth=vp}
        PageGoal              -> c{pageGoal=vp}
        PageTotal             -> c{pageTotal=vp}
        PageStretch           -> c{pageStretch=vp}
        PageFilStretch        -> c{pageFilStretch=vp}
        PageFillStretch       -> c{pageFillStretch=vp}
        PageFilllStretch      -> c{pageFilllStretch=vp}
        PageShrink            -> c{pageShrink=vp}
        PageDepth             -> c{pageDepth=vp}
