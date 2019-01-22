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
resolveToken _csMap Expanding (Lex.ControlSequenceToken cs) =
    HMap.lookupDefault (PrimitiveToken ResolutionError) (Lex.ControlSequenceProper cs) _csMap
resolveToken _csMap NotExpanding t@(Lex.ControlSequenceToken _) =
    PrimitiveToken $ UnexpandedTok t
-- TODO: Active characters.
resolveToken _ _ t@(Lex.CharCatToken _) =
    PrimitiveToken $ UnexpandedTok t

_cs :: String -> Lex.ControlSequenceLike
_cs = Lex.ControlSequenceProper . Lex.ControlSequence

_pt :: PrimitiveToken -> ResolvedToken
_pt = PrimitiveToken

defaultCSMap :: CSMap
defaultCSMap = HMap.fromList
    -- Heads of syntax commands.
    [ (_cs "uppercase"   , SyntaxCommandHead $ ChangeCaseTok Upward)
    , (_cs "lowercase"   , SyntaxCommandHead $ ChangeCaseTok Downward)
    , (_cs "csname"      , SyntaxCommandHead CSNameTok)
    -- Arguments of syntax commands.
    , (_cs "endcsname"   , _pt $ SyntaxCommandArg EndCSNameTok)
    -- Nothing special.
    , (_cs "relax"       , _pt RelaxTok)
    , (_cs "ignorespaces", _pt IgnoreSpacesTok)
    , (_cs "penalty"     , _pt AddPenaltyTok)
    , (_cs "kern"        , _pt AddKernTok)
    , (_cs "indent"      , _pt $ StartParagraphTok Indent)
    , (_cs "noindent"    , _pt $ StartParagraphTok DoNotIndent)
    , (_cs "par"         , _pt EndParagraphTok)
    -- Glue.
    , (_cs "vskip"       , _pt $ ModedCommand Vertical AddSpecifiedGlueTok)
    , (_cs "hskip"       , _pt $ ModedCommand Horizontal AddSpecifiedGlueTok)
    , (_cs "vfil"        , _pt $ ModedCommand Vertical $ AddPresetGlueTok Fil)
    , (_cs "hfil"        , _pt $ ModedCommand Horizontal $ AddPresetGlueTok Fil)
    , (_cs "vfill"       , _pt $ ModedCommand Vertical $ AddPresetGlueTok Fill)
    , (_cs "hfill"       , _pt $ ModedCommand Horizontal $ AddPresetGlueTok Fill)
    , (_cs "vfilneg"     , _pt $ ModedCommand Vertical $ AddPresetGlueTok FilNeg)
    , (_cs "hfilneg"     , _pt $ ModedCommand Horizontal $ AddPresetGlueTok FilNeg)
    , (_cs "vss"         , _pt $ ModedCommand Vertical $ AddPresetGlueTok StretchOrShrink)
    , (_cs "hss"         , _pt $ ModedCommand Horizontal $ AddPresetGlueTok StretchOrShrink)
    -- Other moded.
    , (_cs "halign"      , _pt $ ModedCommand Horizontal AddAlignedMaterial)
    , (_cs "valign"      , _pt $ ModedCommand Vertical AddAlignedMaterial)
    , (_cs "moveleft"    , _pt $ ModedCommand Horizontal $ AddShiftedBox Backward)
    , (_cs "moveright"   , _pt $ ModedCommand Horizontal $ AddShiftedBox Forward)
    , (_cs "raise"       , _pt $ ModedCommand Vertical $ AddShiftedBox Backward)
    , (_cs "lower"       , _pt $ ModedCommand Vertical $ AddShiftedBox Forward)
    , (_cs "unvbox"      , _pt $ ModedCommand Vertical $ AddUnwrappedFetchedBoxTok Pop)
    , (_cs "unhbox"      , _pt $ ModedCommand Horizontal $ AddUnwrappedFetchedBoxTok Pop)
    , (_cs "unvcopy"     , _pt $ ModedCommand Vertical $ AddUnwrappedFetchedBoxTok Lookup)
    , (_cs "unhcopy"     , _pt $ ModedCommand Horizontal $ AddUnwrappedFetchedBoxTok Lookup)
    , (_cs "hrule"       , _pt $ ModedCommand Vertical AddRuleTok)
    , (_cs "vrule"       , _pt $ ModedCommand Horizontal AddRuleTok)
    -- Final commands.
    , (_cs "end"         , _pt EndTok)
    , (_cs "dump"        , _pt DumpTok)
      -- Macro prefixes.
    , (_cs "long"        , _pt $ AssignPrefixTok LongTok)
    , (_cs "outer"       , _pt $ AssignPrefixTok OuterTok)
    , (_cs "global"      , _pt $ AssignPrefixTok GlobalTok)
      -- Macro def types.
    , (_cs "def"         , _pt $ DefineMacroTok Local InhibitDef)
    , (_cs "edef"        , _pt $ DefineMacroTok Local ExpandDef)
    , (_cs "gdef"        , _pt $ DefineMacroTok Global InhibitDef)
    , (_cs "xdef"        , _pt $ DefineMacroTok Global ExpandDef)
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
    -- Register reference type prefixes.
    , (_cs "count"            , _pt $ RegisterVariableTok RegInt)
    , (_cs "dimen"            , _pt $ RegisterVariableTok RegLen)
    , (_cs "skip"             , _pt $ RegisterVariableTok RegGlue)
    , (_cs "muskip"           , _pt $ RegisterVariableTok RegMathGlue)
    , (_cs "toks"             , _pt $ RegisterVariableTok RegTokenList)
    -- Short-hand definition heads.
    , (_cs "chardef"          , _pt $ ShortDefHeadTok CharQuantity)
    , (_cs "mathchardef"      , _pt $ ShortDefHeadTok MathCharQuantity)
    , (_cs "countdef"         , _pt $ ShortDefHeadTok IntegerQuantity)
    , (_cs "dimendef"         , _pt $ ShortDefHeadTok LengthQuantity)
    , (_cs "skipdef"          , _pt $ ShortDefHeadTok GlueQuantity)
    , (_cs "muskipdef"        , _pt $ ShortDefHeadTok MathGlueQuantity)
    , (_cs "toksdef"          , _pt $ ShortDefHeadTok TokenListQuantity)
    -- Modify variables.
    , (_cs "advance"          , _pt AdvanceVarTok)
    , (_cs "multiply"         , _pt $ ScaleVarTok Upward)
    , (_cs "divide"           , _pt $ ScaleVarTok Downward)
    -- Alias tokens.
    , (_cs "let"              , _pt $ LetTok)
    , (_cs "futurelet"        , _pt $ FutureLetTok)
      -- Code types.
    , (_cs "catcode"          , _pt $ CodeTypeTok CategoryCode)
    , (_cs "mathcode"         , _pt $ CodeTypeTok MathCode)
    , (_cs "lccode"           , _pt $ CodeTypeTok $ ChangeCaseCode Downward)
    , (_cs "uccode"           , _pt $ CodeTypeTok $ ChangeCaseCode Upward)
    , (_cs "sfcode"           , _pt $ CodeTypeTok SpaceFactorCode)
    , (_cs "delcode"          , _pt $ CodeTypeTok DelimiterCode)
    -- Font range.
    , (_cs "textfont"         , _pt $ FontRangeTok TextSizeFontRange)
    , (_cs "scriptfont"       , _pt $ FontRangeTok ScriptSizeFontRange)
    , (_cs "scriptscriptfont" , _pt $ FontRangeTok ScriptScriptSizeFontRange)
    -- Internal integer.
    , (_cs "lastpenalty"      , _pt LastPenaltyTok)
    , (_cs "parshape"         , _pt ParagraphShapeTok)
    , (_cs "badness"          , _pt BadnessTok)
    , (_cs "inputlineno"      , _pt InputLineNrTok)
    -- Internal length.
    , (_cs "lastkern"         , _pt LastKernTok)
    , (_cs "fontdimen"        , _pt FontDimensionTok)
    , (_cs "ht"               , _pt $ BoxDimensionTok Height)
    , (_cs "wd"               , _pt $ BoxDimensionTok Width)
    , (_cs "dp"               , _pt $ BoxDimensionTok Depth)
    -- Internal glue.
    , (_cs "lastskip"         , _pt LastGlueTok)
    -- Streams.
    , (_cs "read"             , _pt ReadTok)
    -- Fonts.
    , (_cs "font"             , _pt FontTok)
    , (_cs "hyphenchar"       , _pt $ FontCharTok HyphenChar)
    , (_cs "skewchar"         , _pt $ FontCharTok SkewChar)
      -- Temporary pragmatism.
    , (_cs "selectfont"  , _pt $ FontRefToken theFontNr)
    ]
