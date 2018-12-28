module HeX.Parse.Resolved.Resolve where

import qualified Data.HashMap.Strict           as HMap

import qualified HeX.Lex                       as Lex
import           HeX.Parse.Resolved.Token
import           HeX.Parse.Resolved.Parameter

theFontNr :: Int
theFontNr = 1

type CSMap = HMap.HashMap Lex.ControlSequenceLike ResolvedToken

resolveToken :: CSMap -> Lex.Token -> Maybe ResolvedToken
resolveToken _csMap (Lex.ControlSequenceToken cs)
  = HMap.lookup (Lex.ControlSequenceProper cs) _csMap
-- TODO: Active characters.
resolveToken _ (Lex.CharCatToken cc)
  = pure $ PrimitiveToken $ CharCat cc

_cs :: String -> Lex.ControlSequenceLike
_cs = Lex.ControlSequenceProper . Lex.ControlSequence

_pt :: PrimitiveToken -> ResolvedToken
_pt = PrimitiveToken

defaultCSMap :: CSMap
defaultCSMap = HMap.fromList
  [ (_cs "relax"       , _pt Relax)
  , (_cs "ignorespaces", _pt IgnoreSpaces)
  , (_cs "uppercase"   , SyntaxCommandHead $ ChangeCaseToken Upward)
  , (_cs "lowercase"   , SyntaxCommandHead $ ChangeCaseToken Downward)
  , (_cs "penalty"     , _pt AddPenalty)
  , (_cs "kern"        , _pt AddKern)
  , (_cs "vskip"       , _pt $ ModedCommand Vertical AddSpecifiedGlue)
  , (_cs "hskip"       , _pt $ ModedCommand Horizontal AddSpecifiedGlue)
  , (_cs "hfil"        , _pt $ ModedCommand Horizontal $ AddPresetGlue Fil)
  , (_cs "vfil"        , _pt $ ModedCommand Vertical $ AddPresetGlue Fil)
  , (_cs "hfill"       , _pt $ ModedCommand Horizontal $ AddPresetGlue Fill)
  , (_cs "vfill"       , _pt $ ModedCommand Vertical $ AddPresetGlue Fill)
  , (_cs "hfilneg"     , _pt $ ModedCommand Horizontal $ AddPresetGlue FilNeg)
  , (_cs "vfilneg"     , _pt $ ModedCommand Vertical $ AddPresetGlue FilNeg)
  , (_cs "hss"         , _pt $ ModedCommand Horizontal $ AddPresetGlue StretchOrShrink)
  , (_cs "vss"         , _pt $ ModedCommand Vertical $ AddPresetGlue StretchOrShrink)
  , (_cs "indent"      , _pt $ StartParagraph { indent = True })
  , (_cs "noindent"    , _pt $ StartParagraph { indent = False })
  , (_cs "par"         , _pt EndParagraph)
  , (_cs "hrule"       , _pt $ ModedCommand Vertical AddRule)
  , (_cs "vrule"       , _pt $ ModedCommand Horizontal AddRule)
  , (_cs "font"        , _pt MacroToFont)
  , (_cs "csname"      , SyntaxCommandHead CSName)
  , (_cs "endcsname"   , _pt $ SyntaxCommandArg EndCSName)
    -- Temporary pragmatism.
  , (_cs "selectfont"  , _pt $ TokenForFont theFontNr)
  , (_cs "end"         , _pt End)
    -- Macro prefixes.
  , (_cs "global"      , _pt Global)
  , (_cs "long"        , _pt Long)
  , (_cs "outer"       , _pt Outer)
    -- Macro def types.
  , (_cs "def"         , _pt $ DefineMacro { global = False, expand = False })
  , (_cs "edef"        , _pt $ DefineMacro { global = False, expand = True })
  , (_cs "gdef"        , _pt $ DefineMacro { global = True, expand = False })
  , (_cs "xdef"        , _pt $ DefineMacro { global = True, expand = True })
    -- Integer parameters.
  , (_cs "pretolerance"         , _pt $ IntParamVar PreTolerance)
  , (_cs "tolerance"            , _pt $ IntParamVar Tolerance)
  , (_cs "hbadness"             , _pt $ IntParamVar HBadness)
  , (_cs "vbadness"             , _pt $ IntParamVar VBadness)
  , (_cs "linepenalty"          , _pt $ IntParamVar LinePenalty)
  , (_cs "hyphenpenalty"        , _pt $ IntParamVar HyphenPenalty)
  , (_cs "exhyphenpenalty"      , _pt $ IntParamVar ExHyphenPenalty)
  , (_cs "binoppenalty"         , _pt $ IntParamVar BinOpPenalty)
  , (_cs "relpenalty"           , _pt $ IntParamVar RelPenalty)
  , (_cs "clubpenalty"          , _pt $ IntParamVar ClubPenalty)
  , (_cs "widowpenalty"         , _pt $ IntParamVar WidowPenalty)
  , (_cs "displaywidowpenalty"  , _pt $ IntParamVar DisplayWidowPenalty)
  , (_cs "brokenpenalty"        , _pt $ IntParamVar BrokenPenalty)
  , (_cs "predisplaypenalty"    , _pt $ IntParamVar PreDisplayPenalty)
  , (_cs "postdisplaypenalty"   , _pt $ IntParamVar PostDisplayPenalty)
  , (_cs "interlinepenalty"     , _pt $ IntParamVar InterlinePenalty)
  , (_cs "floatingpenalty"      , _pt $ IntParamVar FloatingPenalty)
  , (_cs "outputpenalty"        , _pt $ IntParamVar OutputPenalty)
  , (_cs "doublehyphendemerits" , _pt $ IntParamVar DoubleHyphenDemerits)
  , (_cs "finalhyphendemerits"  , _pt $ IntParamVar FinalHyphenDemerits)
  , (_cs "adjdemerits"          , _pt $ IntParamVar AdjDemerits)
  , (_cs "looseness"            , _pt $ IntParamVar Looseness)
  , (_cs "pausing"              , _pt $ IntParamVar Pausing)
  , (_cs "holdinginserts"       , _pt $ IntParamVar HoldingInserts)
  , (_cs "tracingonline"        , _pt $ IntParamVar TracingOnline)
  , (_cs "tracingmacros"        , _pt $ IntParamVar TracingMacros)
  , (_cs "tracingstats"         , _pt $ IntParamVar TracingStats)
  , (_cs "tracingparagraphs"    , _pt $ IntParamVar TracingParagraphs)
  , (_cs "tracingpages"         , _pt $ IntParamVar TracingPages)
  , (_cs "tracingoutput"        , _pt $ IntParamVar TracingOutput)
  , (_cs "tracinglostchars"     , _pt $ IntParamVar TracingLostChars)
  , (_cs "tracingcommands"      , _pt $ IntParamVar TracingCommands)
  , (_cs "tracingrestores"      , _pt $ IntParamVar TracingRestores)
  , (_cs "language"             , _pt $ IntParamVar Language)
  , (_cs "uchyph"               , _pt $ IntParamVar UCHyph)
  , (_cs "lefthyphenmin"        , _pt $ IntParamVar LeftHyphenMin)
  , (_cs "righthyphenmin"       , _pt $ IntParamVar RightHyphenMin)
  , (_cs "globaldefs"           , _pt $ IntParamVar GlobalDefs)
  , (_cs "defaulthyphenchar"    , _pt $ IntParamVar DefaultHyphenChar)
  , (_cs "defaultskewchar"      , _pt $ IntParamVar DefaultSkewChar)
  , (_cs "escapechar"           , _pt $ IntParamVar EscapeChar)
  , (_cs "endlinechar"          , _pt $ IntParamVar EndLineChar)
  , (_cs "newlinechar"          , _pt $ IntParamVar NewLineChar)
  , (_cs "maxdeadcycles"        , _pt $ IntParamVar MaxDeadCycles)
  , (_cs "hangafter"            , _pt $ IntParamVar HangAfter)
  , (_cs "fam"                  , _pt $ IntParamVar Fam)
  , (_cs "mag"                  , _pt $ IntParamVar Mag)
  , (_cs "delimiterfactor"      , _pt $ IntParamVar DelimiterFactor)
  , (_cs "time"                 , _pt $ IntParamVar Time)
  , (_cs "day"                  , _pt $ IntParamVar Day)
  , (_cs "month"                , _pt $ IntParamVar Month)
  , (_cs "year"                 , _pt $ IntParamVar Year)
  , (_cs "showboxbreadth"       , _pt $ IntParamVar ShowBoxBreadth)
  , (_cs "showboxdepth"         , _pt $ IntParamVar ShowBoxDepth)
  , (_cs "errorcontextlines"    , _pt $ IntParamVar ErrorContextLines)
  -- Length parameters.
  , (_cs "hfuzz"                , _pt $ LenParamVar HFuzz)
  , (_cs "vfuzz"                , _pt $ LenParamVar VFuzz)
  , (_cs "overfullrule"         , _pt $ LenParamVar OverfullRule)
  , (_cs "emergencystretch"     , _pt $ LenParamVar EmergencyStretch)
  , (_cs "hsize"                , _pt $ LenParamVar HSize)
  , (_cs "vsize"                , _pt $ LenParamVar VSize)
  , (_cs "maxdepth"             , _pt $ LenParamVar MaxDepth)
  , (_cs "splitmaxdepth"        , _pt $ LenParamVar SplitMaxDepth)
  , (_cs "boxmaxdepth"          , _pt $ LenParamVar BoxMaxDepth)
  , (_cs "lineskiplimit"        , _pt $ LenParamVar LineSkipLimit)
  , (_cs "delimitershortfall"   , _pt $ LenParamVar DelimiterShortfall)
  , (_cs "nulldelimiterspace"   , _pt $ LenParamVar NullDelimiterSpace)
  , (_cs "scriptspace"          , _pt $ LenParamVar ScriptSpace)
  , (_cs "mathsurround"         , _pt $ LenParamVar MathSurround)
  , (_cs "predisplaysize"       , _pt $ LenParamVar PreDisplaySize)
  , (_cs "displaywidth"         , _pt $ LenParamVar DisplayWidth)
  , (_cs "displayindent"        , _pt $ LenParamVar DisplayIndent)
  , (_cs "parindent"            , _pt $ LenParamVar ParIndent)
  , (_cs "hangindent"           , _pt $ LenParamVar HangIndent)
  , (_cs "hoffset"              , _pt $ LenParamVar HOffset)
  , (_cs "voffset"              , _pt $ LenParamVar VOffset)
  -- Glue parameters.
  , (_cs "baselineskip"         , _pt $ GlueParamVar BaselineSkip)
  , (_cs "lineskip"             , _pt $ GlueParamVar LineSkip)
  , (_cs "parskip"              , _pt $ GlueParamVar ParSkip)
  , (_cs "abovedisplayskip"     , _pt $ GlueParamVar AboveDisplaySkip)
  , (_cs "abovedisplayshortskip", _pt $ GlueParamVar AboveDisplayShortSkip)
  , (_cs "belowdisplayskip"     , _pt $ GlueParamVar BelowDisplaySkip)
  , (_cs "belowdisplayshortskip", _pt $ GlueParamVar BelowDisplayShortSkip)
  , (_cs "leftskip"             , _pt $ GlueParamVar LeftSkip)
  , (_cs "rightskip"            , _pt $ GlueParamVar RightSkip)
  , (_cs "topskip"              , _pt $ GlueParamVar TopSkip)
  , (_cs "splittopskip"         , _pt $ GlueParamVar SplitTopSkip)
  , (_cs "tabskip"              , _pt $ GlueParamVar TabSkip)
  , (_cs "spaceskip"            , _pt $ GlueParamVar SpaceSkip)
  , (_cs "xspaceskip"           , _pt $ GlueParamVar XSpaceSkip)
  , (_cs "parfillskip"          , _pt $ GlueParamVar ParFillSkip)
  -- Math-glue parameters.
  , (_cs "thinmuskip"           , _pt $ MathGlueParamVar ThinMuSkip)
  , (_cs "medmuskip"            , _pt $ MathGlueParamVar MedMuSkip)
  , (_cs "thickmuskip"          , _pt $ MathGlueParamVar ThickMuSkip
    )
  -- Token list parameters.
  , (_cs "output"               , _pt $ TokenListParamVar Output)
  , (_cs "everypar"             , _pt $ TokenListParamVar EveryPar)
  , (_cs "everymath"            , _pt $ TokenListParamVar EveryMath)
  , (_cs "everydisplay"         , _pt $ TokenListParamVar EveryDisplay)
  , (_cs "everyhbox"            , _pt $ TokenListParamVar EveryHBox)
  , (_cs "everyvbox"            , _pt $ TokenListParamVar EveryVBox)
  , (_cs "everyjob"             , _pt $ TokenListParamVar EveryJob)
  , (_cs "everycr"              , _pt $ TokenListParamVar EveryCR)
  , (_cs "errhelp"              , _pt $ TokenListParamVar ErrHelp)
  -- Special integers.
  , (_cs "spacefactor"          , _pt $ SpecialInteger SpaceFactor)
  , (_cs "prevgraf"             , _pt $ SpecialInteger PrevGraf)
  , (_cs "deadcycles"           , _pt $ SpecialInteger DeadCycles)
  , (_cs "insertpenalties"      , _pt $ SpecialInteger InsertPenalties)
  -- Special lengths.
  , (_cs "prevdepth"            , _pt $ SpecialLength PrevDepth)
  , (_cs "pagegoal"             , _pt $ SpecialLength PageGoal)
  , (_cs "pagetotal"            , _pt $ SpecialLength PageTotal)
  , (_cs "pagestretch"          , _pt $ SpecialLength PageStretch)
  , (_cs "pagefilstretch"       , _pt $ SpecialLength PageFilStretch)
  , (_cs "pagefillstretch"      , _pt $ SpecialLength PageFillStretch)
  , (_cs "pagefilllstretch"     , _pt $ SpecialLength PageFilllStretch)
  , (_cs "pageshrink"           , _pt $ SpecialLength PageShrink)
  , (_cs "pagedepth"            , _pt $ SpecialLength PageDepth)
  ]
