module HeX.Parse.Resolve where

import qualified Data.HashMap.Strict           as HMap

import           HeX.Concept
import qualified HeX.Lex                       as Lex
import           HeX.Parse.Token

theFontNr :: Int
theFontNr = 1

data ExpansionMode = Expanding | NotExpanding
  deriving (Show)

type CSMap = HMap.HashMap Lex.ControlSequenceLike ResolvedToken

resolveToken :: CSMap -> ExpansionMode -> Lex.Token -> ResolvedToken
resolveToken _csMap Expanding (Lex.ControlSequenceToken cs)
  = HMap.lookupDefault (PrimitiveToken ResolutionError) (Lex.ControlSequenceProper cs) _csMap
resolveToken _csMap NotExpanding t@(Lex.ControlSequenceToken _)
  = PrimitiveToken $ UnexpandedTok t
-- TODO: Active characters.
resolveToken _ _ t@(Lex.CharCatToken _)
  = PrimitiveToken $ UnexpandedTok t

_cs :: String -> Lex.ControlSequenceLike
_cs = Lex.ControlSequenceProper . Lex.ControlSequence

_pt :: PrimitiveToken -> ResolvedToken
_pt = PrimitiveToken

defaultCSMap :: CSMap
defaultCSMap = HMap.fromList
  [ (_cs "relax"       , _pt RelaxTok)
  , (_cs "ignorespaces", _pt IgnoreSpacesTok)
  , (_cs "uppercase"   , SyntaxCommandHead $ ChangeCaseTok Upward)
  , (_cs "lowercase"   , SyntaxCommandHead $ ChangeCaseTok Downward)
  , (_cs "penalty"     , _pt AddPenaltyTok)
  , (_cs "kern"        , _pt AddKernTok)
  , (_cs "vskip"       , _pt $ ModedCommand Vertical AddSpecifiedGlueTok)
  , (_cs "hskip"       , _pt $ ModedCommand Horizontal AddSpecifiedGlueTok)
  , (_cs "hfil"        , _pt $ ModedCommand Horizontal $ AddPresetGlueTok Fil)
  , (_cs "vfil"        , _pt $ ModedCommand Vertical $ AddPresetGlueTok Fil)
  , (_cs "hfill"       , _pt $ ModedCommand Horizontal $ AddPresetGlueTok Fill)
  , (_cs "vfill"       , _pt $ ModedCommand Vertical $ AddPresetGlueTok Fill)
  , (_cs "hfilneg"     , _pt $ ModedCommand Horizontal $ AddPresetGlueTok FilNeg)
  , (_cs "vfilneg"     , _pt $ ModedCommand Vertical $ AddPresetGlueTok FilNeg)
  , (_cs "hss"         , _pt $ ModedCommand Horizontal $ AddPresetGlueTok StretchOrShrink)
  , (_cs "vss"         , _pt $ ModedCommand Vertical $ AddPresetGlueTok StretchOrShrink)
  , (_cs "indent"      , _pt $ StartParagraphTok { indent = True })
  , (_cs "noindent"    , _pt $ StartParagraphTok { indent = False })
  , (_cs "par"         , _pt EndParagraphTok)
  , (_cs "hrule"       , _pt $ ModedCommand Vertical AddRuleTok)
  , (_cs "vrule"       , _pt $ ModedCommand Horizontal AddRuleTok)
  , (_cs "font"        , _pt FontTok)
  , (_cs "csname"      , SyntaxCommandHead CSNameTok)
  , (_cs "endcsname"   , _pt $ SyntaxCommandArg EndCSNameTok)
    -- Temporary pragmatism.
  , (_cs "selectfont"  , _pt $ TokenForFont theFontNr)
  , (_cs "end"         , _pt EndTok)
    -- Macro prefixes.
  , (_cs "global"      , _pt GlobalTok)
  , (_cs "long"        , _pt LongTok)
  , (_cs "outer"       , _pt OuterTok)
    -- Macro def types.
  , (_cs "def"         , _pt $ DefineMacroTok { global = False, expand = False })
  , (_cs "edef"        , _pt $ DefineMacroTok { global = False, expand = True })
  , (_cs "gdef"        , _pt $ DefineMacroTok { global = True, expand = False })
  , (_cs "xdef"        , _pt $ DefineMacroTok { global = True, expand = True })
    -- Integer parameters.
  , (_cs "pretolerance"         , _pt $ IntParamVarTok PreTolerance)
  , (_cs "tolerance"            , _pt $ IntParamVarTok Tolerance)
  , (_cs "hbadness"             , _pt $ IntParamVarTok HBadness)
  , (_cs "vbadness"             , _pt $ IntParamVarTok VBadness)
  , (_cs "linepenalty"          , _pt $ IntParamVarTok LinePenalty)
  , (_cs "hyphenpenalty"        , _pt $ IntParamVarTok HyphenPenalty)
  , (_cs "exhyphenpenalty"      , _pt $ IntParamVarTok ExHyphenPenalty)
  , (_cs "binoppenalty"         , _pt $ IntParamVarTok BinOpPenalty)
  , (_cs "relpenalty"           , _pt $ IntParamVarTok RelPenalty)
  , (_cs "clubpenalty"          , _pt $ IntParamVarTok ClubPenalty)
  , (_cs "widowpenalty"         , _pt $ IntParamVarTok WidowPenalty)
  , (_cs "displaywidowpenalty"  , _pt $ IntParamVarTok DisplayWidowPenalty)
  , (_cs "brokenpenalty"        , _pt $ IntParamVarTok BrokenPenalty)
  , (_cs "predisplaypenalty"    , _pt $ IntParamVarTok PreDisplayPenalty)
  , (_cs "postdisplaypenalty"   , _pt $ IntParamVarTok PostDisplayPenalty)
  , (_cs "interlinepenalty"     , _pt $ IntParamVarTok InterlinePenalty)
  , (_cs "floatingpenalty"      , _pt $ IntParamVarTok FloatingPenalty)
  , (_cs "outputpenalty"        , _pt $ IntParamVarTok OutputPenalty)
  , (_cs "doublehyphendemerits" , _pt $ IntParamVarTok DoubleHyphenDemerits)
  , (_cs "finalhyphendemerits"  , _pt $ IntParamVarTok FinalHyphenDemerits)
  , (_cs "adjdemerits"          , _pt $ IntParamVarTok AdjDemerits)
  , (_cs "looseness"            , _pt $ IntParamVarTok Looseness)
  , (_cs "pausing"              , _pt $ IntParamVarTok Pausing)
  , (_cs "holdinginserts"       , _pt $ IntParamVarTok HoldingInserts)
  , (_cs "tracingonline"        , _pt $ IntParamVarTok TracingOnline)
  , (_cs "tracingmacros"        , _pt $ IntParamVarTok TracingMacros)
  , (_cs "tracingstats"         , _pt $ IntParamVarTok TracingStats)
  , (_cs "tracingparagraphs"    , _pt $ IntParamVarTok TracingParagraphs)
  , (_cs "tracingpages"         , _pt $ IntParamVarTok TracingPages)
  , (_cs "tracingoutput"        , _pt $ IntParamVarTok TracingOutput)
  , (_cs "tracinglostchars"     , _pt $ IntParamVarTok TracingLostChars)
  , (_cs "tracingcommands"      , _pt $ IntParamVarTok TracingCommands)
  , (_cs "tracingrestores"      , _pt $ IntParamVarTok TracingRestores)
  , (_cs "language"             , _pt $ IntParamVarTok Language)
  , (_cs "uchyph"               , _pt $ IntParamVarTok UCHyph)
  , (_cs "lefthyphenmin"        , _pt $ IntParamVarTok LeftHyphenMin)
  , (_cs "righthyphenmin"       , _pt $ IntParamVarTok RightHyphenMin)
  , (_cs "globaldefs"           , _pt $ IntParamVarTok GlobalDefs)
  , (_cs "defaulthyphenchar"    , _pt $ IntParamVarTok DefaultHyphenChar)
  , (_cs "defaultskewchar"      , _pt $ IntParamVarTok DefaultSkewChar)
  , (_cs "escapechar"           , _pt $ IntParamVarTok EscapeChar)
  , (_cs "endlinechar"          , _pt $ IntParamVarTok EndLineChar)
  , (_cs "newlinechar"          , _pt $ IntParamVarTok NewLineChar)
  , (_cs "maxdeadcycles"        , _pt $ IntParamVarTok MaxDeadCycles)
  , (_cs "hangafter"            , _pt $ IntParamVarTok HangAfter)
  , (_cs "fam"                  , _pt $ IntParamVarTok Fam)
  , (_cs "mag"                  , _pt $ IntParamVarTok Mag)
  , (_cs "delimiterfactor"      , _pt $ IntParamVarTok DelimiterFactor)
  , (_cs "time"                 , _pt $ IntParamVarTok Time)
  , (_cs "day"                  , _pt $ IntParamVarTok Day)
  , (_cs "month"                , _pt $ IntParamVarTok Month)
  , (_cs "year"                 , _pt $ IntParamVarTok Year)
  , (_cs "showboxbreadth"       , _pt $ IntParamVarTok ShowBoxBreadth)
  , (_cs "showboxdepth"         , _pt $ IntParamVarTok ShowBoxDepth)
  , (_cs "errorcontextlines"    , _pt $ IntParamVarTok ErrorContextLines)
  -- Length parameters.
  , (_cs "hfuzz"                , _pt $ LenParamVarTok HFuzz)
  , (_cs "vfuzz"                , _pt $ LenParamVarTok VFuzz)
  , (_cs "overfullrule"         , _pt $ LenParamVarTok OverfullRule)
  , (_cs "emergencystretch"     , _pt $ LenParamVarTok EmergencyStretch)
  , (_cs "hsize"                , _pt $ LenParamVarTok HSize)
  , (_cs "vsize"                , _pt $ LenParamVarTok VSize)
  , (_cs "maxdepth"             , _pt $ LenParamVarTok MaxDepth)
  , (_cs "splitmaxdepth"        , _pt $ LenParamVarTok SplitMaxDepth)
  , (_cs "boxmaxdepth"          , _pt $ LenParamVarTok BoxMaxDepth)
  , (_cs "lineskiplimit"        , _pt $ LenParamVarTok LineSkipLimit)
  , (_cs "delimitershortfall"   , _pt $ LenParamVarTok DelimiterShortfall)
  , (_cs "nulldelimiterspace"   , _pt $ LenParamVarTok NullDelimiterSpace)
  , (_cs "scriptspace"          , _pt $ LenParamVarTok ScriptSpace)
  , (_cs "mathsurround"         , _pt $ LenParamVarTok MathSurround)
  , (_cs "predisplaysize"       , _pt $ LenParamVarTok PreDisplaySize)
  , (_cs "displaywidth"         , _pt $ LenParamVarTok DisplayWidth)
  , (_cs "displayindent"        , _pt $ LenParamVarTok DisplayIndent)
  , (_cs "parindent"            , _pt $ LenParamVarTok ParIndent)
  , (_cs "hangindent"           , _pt $ LenParamVarTok HangIndent)
  , (_cs "hoffset"              , _pt $ LenParamVarTok HOffset)
  , (_cs "voffset"              , _pt $ LenParamVarTok VOffset)
  -- Glue parameters.
  , (_cs "baselineskip"         , _pt $ GlueParamVarTok BaselineSkip)
  , (_cs "lineskip"             , _pt $ GlueParamVarTok LineSkip)
  , (_cs "parskip"              , _pt $ GlueParamVarTok ParSkip)
  , (_cs "abovedisplayskip"     , _pt $ GlueParamVarTok AboveDisplaySkip)
  , (_cs "abovedisplayshortskip", _pt $ GlueParamVarTok AboveDisplayShortSkip)
  , (_cs "belowdisplayskip"     , _pt $ GlueParamVarTok BelowDisplaySkip)
  , (_cs "belowdisplayshortskip", _pt $ GlueParamVarTok BelowDisplayShortSkip)
  , (_cs "leftskip"             , _pt $ GlueParamVarTok LeftSkip)
  , (_cs "rightskip"            , _pt $ GlueParamVarTok RightSkip)
  , (_cs "topskip"              , _pt $ GlueParamVarTok TopSkip)
  , (_cs "splittopskip"         , _pt $ GlueParamVarTok SplitTopSkip)
  , (_cs "tabskip"              , _pt $ GlueParamVarTok TabSkip)
  , (_cs "spaceskip"            , _pt $ GlueParamVarTok SpaceSkip)
  , (_cs "xspaceskip"           , _pt $ GlueParamVarTok XSpaceSkip)
  , (_cs "parfillskip"          , _pt $ GlueParamVarTok ParFillSkip)
  -- Math-glue parameters.
  , (_cs "thinmuskip"           , _pt $ MathGlueParamVarTok ThinMuSkip)
  , (_cs "medmuskip"            , _pt $ MathGlueParamVarTok MedMuSkip)
  , (_cs "thickmuskip"          , _pt $ MathGlueParamVarTok ThickMuSkip
    )
  -- Token list parameters.
  , (_cs "output"               , _pt $ TokenListParamVarTok Output)
  , (_cs "everypar"             , _pt $ TokenListParamVarTok EveryPar)
  , (_cs "everymath"            , _pt $ TokenListParamVarTok EveryMath)
  , (_cs "everydisplay"         , _pt $ TokenListParamVarTok EveryDisplay)
  , (_cs "everyhbox"            , _pt $ TokenListParamVarTok EveryHBox)
  , (_cs "everyvbox"            , _pt $ TokenListParamVarTok EveryVBox)
  , (_cs "everyjob"             , _pt $ TokenListParamVarTok EveryJob)
  , (_cs "everycr"              , _pt $ TokenListParamVarTok EveryCR)
  , (_cs "errhelp"              , _pt $ TokenListParamVarTok ErrHelp)
  -- Special integers.
  , (_cs "spacefactor"          , _pt $ SpecialIntegerTok SpaceFactor)
  , (_cs "prevgraf"             , _pt $ SpecialIntegerTok PrevGraf)
  , (_cs "deadcycles"           , _pt $ SpecialIntegerTok DeadCycles)
  , (_cs "insertpenalties"      , _pt $ SpecialIntegerTok InsertPenalties)
  -- Special lengths.
  , (_cs "prevdepth"            , _pt $ SpecialLengthTok PrevDepth)
  , (_cs "pagegoal"             , _pt $ SpecialLengthTok PageGoal)
  , (_cs "pagetotal"            , _pt $ SpecialLengthTok PageTotal)
  , (_cs "pagestretch"          , _pt $ SpecialLengthTok PageStretch)
  , (_cs "pagefilstretch"       , _pt $ SpecialLengthTok PageFilStretch)
  , (_cs "pagefillstretch"      , _pt $ SpecialLengthTok PageFillStretch)
  , (_cs "pagefilllstretch"     , _pt $ SpecialLengthTok PageFilllStretch)
  , (_cs "pageshrink"           , _pt $ SpecialLengthTok PageShrink)
  , (_cs "pagedepth"            , _pt $ SpecialLengthTok PageDepth)
  ]
