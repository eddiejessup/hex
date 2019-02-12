module HeX.Parse.Resolve where

import qualified Data.HashMap.Strict           as HMap

import           HeX.Type
import qualified HeX.Lex                       as Lex
import           HeX.Parse.Token

data ExpansionMode = Expanding | NotExpanding
    deriving (Show, Eq)

type CSMap = HMap.HashMap Lex.ControlSequenceLike ResolvedToken

resolveToken :: CSMap -> ExpansionMode -> Lex.Token -> ResolvedToken
resolveToken _csMap Expanding (Lex.ControlSequenceToken cs) =
    let key = Lex.ControlSequenceProper cs
    in HMap.lookupDefault (primTok $ ResolutionError key) key _csMap
resolveToken _csMap NotExpanding t@(Lex.ControlSequenceToken _) =
    primTok $ UnexpandedTok t
-- TODO: Active characters.
resolveToken _ _ t@(Lex.CharCatToken _) =
    primTok $ UnexpandedTok t

_cs :: String -> Lex.ControlSequenceLike
_cs = Lex.ControlSequenceProper . Lex.ControlSequence

_nt :: NonConditionBlockToken -> ResolvedToken
_nt = NonConditionBlockToken

syntaxTok :: SyntaxCommandHeadToken -> ResolvedToken
syntaxTok = _nt . SyntaxCommandHeadToken

primTok :: PrimitiveToken -> ResolvedToken
primTok = _nt . PrimitiveToken

defaultCSMap :: CSMap
defaultCSMap = HMap.fromList
    -- Tokens used in the body of condition blocks.
    [ (_cs "else"        , ConditionBlockToken ElseTok)
    , (_cs "or"          , ConditionBlockToken OrTok)
    , (_cs "fi"          , ConditionBlockToken EndIfTok)
      -- Heads of syntax commands.
    , (_cs "uppercase"   , syntaxTok $ ChangeCaseTok Upward)
    , (_cs "lowercase"   , syntaxTok $ ChangeCaseTok Downward)
    , (_cs "csname"      , syntaxTok CSNameTok)
    -- Heads of conditions.
    , (_cs "ifnum"       , syntaxTok $ IfTok IfIntegerPairTestTok)
    , (_cs "ifdim"       , syntaxTok $ IfTok IfLengthPairTestTok)
    , (_cs "ifodd"       , syntaxTok $ IfTok IfIntegerOddTok)
    , (_cs "ifvmode"     , syntaxTok $ IfTok $ IfInModeTok VerticalMode)
    , (_cs "ifhmode"     , syntaxTok $ IfTok $ IfInModeTok HorizontalMode)
    , (_cs "ifmmode"     , syntaxTok $ IfTok $ IfInModeTok MathMode)
    , (_cs "ifinner"     , syntaxTok $ IfTok $ IfInModeTok InnerMode)
    , (_cs "if"          , syntaxTok $ IfTok $ IfTokenAttributesEqualTok CharCodeAttribute)
    , (_cs "ifcat"       , syntaxTok $ IfTok $ IfTokenAttributesEqualTok CatCodeAttribute)
    , (_cs "ifx"         , syntaxTok $ IfTok IfTokensEqualTok)
    , (_cs "ifvoid"      , syntaxTok $ IfTok $ IfBoxRegisterIsTok IsVoid)
    , (_cs "ifvbox"      , syntaxTok $ IfTok $ IfBoxRegisterIsTok HasVerticalBox)
    , (_cs "ifhbox"      , syntaxTok $ IfTok $ IfBoxRegisterIsTok HasHorizontalBox)
    , (_cs "ifeof"       , syntaxTok $ IfTok IfInputEndedTok)
    , (_cs "iftrue"      , syntaxTok $ IfTok $ IfConstTok True)
    , (_cs "iffalse"     , syntaxTok $ IfTok $ IfConstTok False)
    , (_cs "ifcase"      , syntaxTok $ IfTok CaseTok)
      -- Arguments of syntax commands.
    , (_cs "endcsname"   , primTok $ SyntaxCommandArg EndCSNameTok)
      -- Nothing special.
    , (_cs "relax"       , primTok RelaxTok)
    , (_cs "ignorespaces", primTok IgnoreSpacesTok)
    , (_cs "afterassignment", primTok SetAfterAssignmentTokenTok)
    , (_cs "aftergroup"  , primTok ToAfterGroupTokensTok)
    , (_cs "message"     , primTok $ MessageTok Out)
    , (_cs "errmessage"  , primTok $ MessageTok Err)
    , (_cs "immediate"   , primTok ImmediateTok)
    , (_cs "openin"      , primTok OpenInputTok)
    , (_cs "closein"     , primTok CloseInputTok)
    , (_cs "openout"     , primTok OpenOutputTok)
    , (_cs "closeout"    , primTok CloseOutputTok)
    , (_cs "write"       , primTok WriteTok)
    , (_cs "special"     , primTok DoSpecialTok)
    , (_cs "penalty"     , primTok PenaltyTok)
    , (_cs "kern"        , primTok KernTok)
    , (_cs "mkern"       , primTok MathKernTok)
    , (_cs "unpenalty"   , primTok $ RemoveItemTok PenaltyItem)
    , (_cs "unkern"      , primTok $ RemoveItemTok KernItem)
    , (_cs "unskip"      , primTok $ RemoveItemTok GlueItem)
    , (_cs "mark"        , primTok MarkTok)
    , (_cs "insert"      , primTok InsertionTok)
    , (_cs "leaders"     , primTok $ LeadersTok Aligned)
    , (_cs "cleaders"    , primTok $ LeadersTok Centered)
    , (_cs "xleaders"    , primTok $ LeadersTok Expanded)
    , (_cs "indent"      , primTok $ StartParagraphTok Indent)
    , (_cs "noindent"    , primTok $ StartParagraphTok DoNotIndent)
    , (_cs "par"         , primTok EndParagraphTok)
      -- Glue.
    , (_cs "vskip"       , primTok $ ModedCommand Vertical SpecifiedGlueTok)
    , (_cs "hskip"       , primTok $ ModedCommand Horizontal SpecifiedGlueTok)
    , (_cs "vfil"        , primTok $ ModedCommand Vertical $ PresetGlueTok Fil)
    , (_cs "hfil"        , primTok $ ModedCommand Horizontal $ PresetGlueTok Fil)
    , (_cs "vfill"       , primTok $ ModedCommand Vertical $ PresetGlueTok Fill)
    , (_cs "hfill"       , primTok $ ModedCommand Horizontal $ PresetGlueTok Fill)
    , (_cs "vfilneg"     , primTok $ ModedCommand Vertical $ PresetGlueTok FilNeg)
    , (_cs "hfilneg"     , primTok $ ModedCommand Horizontal $ PresetGlueTok FilNeg)
    , (_cs "vss"         , primTok $ ModedCommand Vertical $ PresetGlueTok StretchOrShrink)
    , (_cs "hss"         , primTok $ ModedCommand Horizontal $ PresetGlueTok StretchOrShrink)
      -- Other moded.
    , (_cs "halign"      , primTok $ ModedCommand Horizontal AlignedMaterialTok)
    , (_cs "valign"      , primTok $ ModedCommand Vertical AlignedMaterialTok)
    , (_cs "moveleft"    , primTok $ ModedCommand Horizontal $ ShiftedBoxTok Backward)
    , (_cs "moveright"   , primTok $ ModedCommand Horizontal $ ShiftedBoxTok Forward)
    , (_cs "raise"       , primTok $ ModedCommand Vertical $ ShiftedBoxTok Backward)
    , (_cs "lower"       , primTok $ ModedCommand Vertical $ ShiftedBoxTok Forward)
    , (_cs "unvbox"      , primTok $ ModedCommand Vertical $ UnwrappedFetchedBoxTok Pop)
    , (_cs "unhbox"      , primTok $ ModedCommand Horizontal $ UnwrappedFetchedBoxTok Pop)
    , (_cs "unvcopy"     , primTok $ ModedCommand Vertical $ UnwrappedFetchedBoxTok Lookup)
    , (_cs "unhcopy"     , primTok $ ModedCommand Horizontal $ UnwrappedFetchedBoxTok Lookup)
    , (_cs "hrule"       , primTok $ ModedCommand Vertical RuleTok)
    , (_cs "vrule"       , primTok $ ModedCommand Horizontal RuleTok)
      -- Final commands.
    , (_cs "end"         , primTok EndTok)
    , (_cs "dump"        , primTok DumpTok)
      -- Starters of Horizontal mode commands.
    , (_cs " "           , primTok ControlSpaceTok)
    , (_cs "char"        , primTok ControlCharTok)
    , (_cs "accent"      , primTok AccentTok)
    , (_cs "/"           , primTok ItalicCorrectionTok)
    , (_cs "discretionary", primTok DiscretionaryTextTok)
    , (_cs "-"           , primTok DiscretionaryHyphenTok)
      -- Macro prefixes.
    , (_cs "long"        , primTok $ AssignPrefixTok LongTok)
    , (_cs "outer"       , primTok $ AssignPrefixTok OuterTok)
    , (_cs "global"      , primTok $ AssignPrefixTok GlobalTok)
      -- Macro def types.
    , (_cs "def"         , primTok $ DefineMacroTok Local InhibitDef)
    , (_cs "edef"        , primTok $ DefineMacroTok Local ExpandDef)
    , (_cs "gdef"        , primTok $ DefineMacroTok Global InhibitDef)
    , (_cs "xdef"        , primTok $ DefineMacroTok Global ExpandDef)
      -- Integer parameters.
    , (_cs "pretolerance"         , primTok $ IntParamVarTok PreTolerance)
    , (_cs "tolerance"            , primTok $ IntParamVarTok Tolerance)
    , (_cs "hbadness"             , primTok $ IntParamVarTok HBadness)
    , (_cs "vbadness"             , primTok $ IntParamVarTok VBadness)
    , (_cs "linepenalty"          , primTok $ IntParamVarTok LinePenalty)
    , (_cs "hyphenpenalty"        , primTok $ IntParamVarTok HyphenPenalty)
    , (_cs "exhyphenpenalty"      , primTok $ IntParamVarTok ExHyphenPenalty)
    , (_cs "binoppenalty"         , primTok $ IntParamVarTok BinOpPenalty)
    , (_cs "relpenalty"           , primTok $ IntParamVarTok RelPenalty)
    , (_cs "clubpenalty"          , primTok $ IntParamVarTok ClubPenalty)
    , (_cs "widowpenalty"         , primTok $ IntParamVarTok WidowPenalty)
    , (_cs "displaywidowpenalty"  , primTok $ IntParamVarTok DisplayWidowPenalty)
    , (_cs "brokenpenalty"        , primTok $ IntParamVarTok BrokenPenalty)
    , (_cs "predisplaypenalty"    , primTok $ IntParamVarTok PreDisplayPenalty)
    , (_cs "postdisplaypenalty"   , primTok $ IntParamVarTok PostDisplayPenalty)
    , (_cs "interlinepenalty"     , primTok $ IntParamVarTok InterlinePenalty)
    , (_cs "floatingpenalty"      , primTok $ IntParamVarTok FloatingPenalty)
    , (_cs "outputpenalty"        , primTok $ IntParamVarTok OutputPenalty)
    , (_cs "doublehyphendemerits" , primTok $ IntParamVarTok DoubleHyphenDemerits)
    , (_cs "finalhyphendemerits"  , primTok $ IntParamVarTok FinalHyphenDemerits)
    , (_cs "adjdemerits"          , primTok $ IntParamVarTok AdjDemerits)
    , (_cs "looseness"            , primTok $ IntParamVarTok Looseness)
    , (_cs "pausing"              , primTok $ IntParamVarTok Pausing)
    , (_cs "holdinginserts"       , primTok $ IntParamVarTok HoldingInserts)
    , (_cs "tracingonline"        , primTok $ IntParamVarTok TracingOnline)
    , (_cs "tracingmacros"        , primTok $ IntParamVarTok TracingMacros)
    , (_cs "tracingstats"         , primTok $ IntParamVarTok TracingStats)
    , (_cs "tracingparagraphs"    , primTok $ IntParamVarTok TracingParagraphs)
    , (_cs "tracingpages"         , primTok $ IntParamVarTok TracingPages)
    , (_cs "tracingoutput"        , primTok $ IntParamVarTok TracingOutput)
    , (_cs "tracinglostchars"     , primTok $ IntParamVarTok TracingLostChars)
    , (_cs "tracingcommands"      , primTok $ IntParamVarTok TracingCommands)
    , (_cs "tracingrestores"      , primTok $ IntParamVarTok TracingRestores)
    , (_cs "language"             , primTok $ IntParamVarTok Language)
    , (_cs "uchyph"               , primTok $ IntParamVarTok UCHyph)
    , (_cs "lefthyphenmin"        , primTok $ IntParamVarTok LeftHyphenMin)
    , (_cs "righthyphenmin"       , primTok $ IntParamVarTok RightHyphenMin)
    , (_cs "globaldefs"           , primTok $ IntParamVarTok GlobalDefs)
    , (_cs "defaulthyphenchar"    , primTok $ IntParamVarTok DefaultHyphenChar)
    , (_cs "defaultskewchar"      , primTok $ IntParamVarTok DefaultSkewChar)
    , (_cs "escapechar"           , primTok $ IntParamVarTok EscapeChar)
    , (_cs "endlinechar"          , primTok $ IntParamVarTok EndLineChar)
    , (_cs "newlinechar"          , primTok $ IntParamVarTok NewLineChar)
    , (_cs "maxdeadcycles"        , primTok $ IntParamVarTok MaxDeadCycles)
    , (_cs "hangafter"            , primTok $ IntParamVarTok HangAfter)
    , (_cs "fam"                  , primTok $ IntParamVarTok Fam)
    , (_cs "mag"                  , primTok $ IntParamVarTok Mag)
    , (_cs "delimiterfactor"      , primTok $ IntParamVarTok DelimiterFactor)
    , (_cs "time"                 , primTok $ IntParamVarTok Time)
    , (_cs "day"                  , primTok $ IntParamVarTok Day)
    , (_cs "month"                , primTok $ IntParamVarTok Month)
    , (_cs "year"                 , primTok $ IntParamVarTok Year)
    , (_cs "showboxbreadth"       , primTok $ IntParamVarTok ShowBoxBreadth)
    , (_cs "showboxdepth"         , primTok $ IntParamVarTok ShowBoxDepth)
    , (_cs "errorcontextlines"    , primTok $ IntParamVarTok ErrorContextLines)
      -- Length parameters.
    , (_cs "hfuzz"                , primTok $ LenParamVarTok HFuzz)
    , (_cs "vfuzz"                , primTok $ LenParamVarTok VFuzz)
    , (_cs "overfullrule"         , primTok $ LenParamVarTok OverfullRule)
    , (_cs "emergencystretch"     , primTok $ LenParamVarTok EmergencyStretch)
    , (_cs "hsize"                , primTok $ LenParamVarTok HSize)
    , (_cs "vsize"                , primTok $ LenParamVarTok VSize)
    , (_cs "maxdepth"             , primTok $ LenParamVarTok MaxDepth)
    , (_cs "splitmaxdepth"        , primTok $ LenParamVarTok SplitMaxDepth)
    , (_cs "boxmaxdepth"          , primTok $ LenParamVarTok BoxMaxDepth)
    , (_cs "lineskiplimit"        , primTok $ LenParamVarTok LineSkipLimit)
    , (_cs "delimitershortfall"   , primTok $ LenParamVarTok DelimiterShortfall)
    , (_cs "nulldelimiterspace"   , primTok $ LenParamVarTok NullDelimiterSpace)
    , (_cs "scriptspace"          , primTok $ LenParamVarTok ScriptSpace)
    , (_cs "mathsurround"         , primTok $ LenParamVarTok MathSurround)
    , (_cs "predisplaysize"       , primTok $ LenParamVarTok PreDisplaySize)
    , (_cs "displaywidth"         , primTok $ LenParamVarTok DisplayWidth)
    , (_cs "displayindent"        , primTok $ LenParamVarTok DisplayIndent)
    , (_cs "parindent"            , primTok $ LenParamVarTok ParIndent)
    , (_cs "hangindent"           , primTok $ LenParamVarTok HangIndent)
    , (_cs "hoffset"              , primTok $ LenParamVarTok HOffset)
    , (_cs "voffset"              , primTok $ LenParamVarTok VOffset)
      -- Glue parameters.
    , (_cs "baselineskip"         , primTok $ GlueParamVarTok BaselineSkip)
    , (_cs "lineskip"             , primTok $ GlueParamVarTok LineSkip)
    , (_cs "parskip"              , primTok $ GlueParamVarTok ParSkip)
    , (_cs "abovedisplayskip"     , primTok $ GlueParamVarTok AboveDisplaySkip)
    , (_cs "abovedisplayshortskip", primTok $ GlueParamVarTok AboveDisplayShortSkip)
    , (_cs "belowdisplayskip"     , primTok $ GlueParamVarTok BelowDisplaySkip)
    , (_cs "belowdisplayshortskip", primTok $ GlueParamVarTok BelowDisplayShortSkip)
    , (_cs "leftskip"             , primTok $ GlueParamVarTok LeftSkip)
    , (_cs "rightskip"            , primTok $ GlueParamVarTok RightSkip)
    , (_cs "topskip"              , primTok $ GlueParamVarTok TopSkip)
    , (_cs "splittopskip"         , primTok $ GlueParamVarTok SplitTopSkip)
    , (_cs "tabskip"              , primTok $ GlueParamVarTok TabSkip)
    , (_cs "spaceskip"            , primTok $ GlueParamVarTok SpaceSkip)
    , (_cs "xspaceskip"           , primTok $ GlueParamVarTok XSpaceSkip)
    , (_cs "parfillskip"          , primTok $ GlueParamVarTok ParFillSkip)
      -- Math-glue parameters.
    , (_cs "thinmuskip"           , primTok $ MathGlueParamVarTok ThinMuSkip)
    , (_cs "medmuskip"            , primTok $ MathGlueParamVarTok MedMuSkip)
    , (_cs "thickmuskip"          , primTok $ MathGlueParamVarTok ThickMuSkip
      )
      -- Token list parameters.
    , (_cs "output"               , primTok $ TokenListParamVarTok Output)
    , (_cs "everypar"             , primTok $ TokenListParamVarTok EveryPar)
    , (_cs "everymath"            , primTok $ TokenListParamVarTok EveryMath)
    , (_cs "everydisplay"         , primTok $ TokenListParamVarTok EveryDisplay)
    , (_cs "everyhbox"            , primTok $ TokenListParamVarTok EveryHBox)
    , (_cs "everyvbox"            , primTok $ TokenListParamVarTok EveryVBox)
    , (_cs "everyjob"             , primTok $ TokenListParamVarTok EveryJob)
    , (_cs "everycr"              , primTok $ TokenListParamVarTok EveryCR)
    , (_cs "errhelp"              , primTok $ TokenListParamVarTok ErrHelp)
      -- Special integers.
    , (_cs "spacefactor"          , primTok $ SpecialIntegerTok SpaceFactorInteger)
    , (_cs "prevgraf"             , primTok $ SpecialIntegerTok PrevGrafInteger)
    , (_cs "deadcycles"           , primTok $ SpecialIntegerTok DeadCyclesInteger)
    , (_cs "insertpenalties"      , primTok $ SpecialIntegerTok InsertPenaltiesInteger)
      -- Special lengths.
    , (_cs "prevdepth"            , primTok $ SpecialLengthTok PrevDepth)
    , (_cs "pagegoal"             , primTok $ SpecialLengthTok PageGoal)
    , (_cs "pagetotal"            , primTok $ SpecialLengthTok PageTotal)
    , (_cs "pagestretch"          , primTok $ SpecialLengthTok PageStretch)
    , (_cs "pagefilstretch"       , primTok $ SpecialLengthTok PageFilStretch)
    , (_cs "pagefillstretch"      , primTok $ SpecialLengthTok PageFillStretch)
    , (_cs "pagefilllstretch"     , primTok $ SpecialLengthTok PageFilllStretch)
    , (_cs "pageshrink"           , primTok $ SpecialLengthTok PageShrink)
    , (_cs "pagedepth"            , primTok $ SpecialLengthTok PageDepth)
      -- Register reference type prefixes.
    , (_cs "count"            , primTok $ RegisterVariableTok RegInt)
    , (_cs "dimen"            , primTok $ RegisterVariableTok RegLen)
    , (_cs "skip"             , primTok $ RegisterVariableTok RegGlue)
    , (_cs "muskip"           , primTok $ RegisterVariableTok RegMathGlue)
    , (_cs "toks"             , primTok $ RegisterVariableTok RegTokenList)
      -- Short-hand definition heads.
    , (_cs "chardef"          , primTok $ ShortDefHeadTok CharQuantity)
    , (_cs "mathchardef"      , primTok $ ShortDefHeadTok MathCharQuantity)
    , (_cs "countdef"         , primTok $ ShortDefHeadTok $ RegQuantity RegInt)
    , (_cs "dimendef"         , primTok $ ShortDefHeadTok $ RegQuantity RegLen)
    , (_cs "skipdef"          , primTok $ ShortDefHeadTok $ RegQuantity RegGlue)
    , (_cs "muskipdef"        , primTok $ ShortDefHeadTok $ RegQuantity RegMathGlue)
    , (_cs "toksdef"          , primTok $ ShortDefHeadTok $ RegQuantity RegTokenList)
      -- Modify variables.
    , (_cs "advance"          , primTok AdvanceVarTok)
    , (_cs "multiply"         , primTok $ ScaleVarTok Upward)
    , (_cs "divide"           , primTok $ ScaleVarTok Downward)
      -- Code types.
    , (_cs "catcode"          , primTok $ CodeTypeTok CategoryCodeType)
    , (_cs "mathcode"         , primTok $ CodeTypeTok MathCodeType)
    , (_cs "lccode"           , primTok $ CodeTypeTok $ ChangeCaseCodeType Downward)
    , (_cs "uccode"           , primTok $ CodeTypeTok $ ChangeCaseCodeType Upward)
    , (_cs "sfcode"           , primTok $ CodeTypeTok SpaceFactorCodeType)
    , (_cs "delcode"          , primTok $ CodeTypeTok DelimiterCodeType)
      -- Alias tokens.
    , (_cs "let"              , primTok $ LetTok)
    , (_cs "futurelet"        , primTok $ FutureLetTok)
      -- Font range.
    , (_cs "textfont"         , primTok $ FontRangeTok TextSizeFontRange)
    , (_cs "scriptfont"       , primTok $ FontRangeTok ScriptSizeFontRange)
    , (_cs "scriptscriptfont" , primTok $ FontRangeTok ScriptScriptSizeFontRange)
      -- Internal integer.
    , (_cs "lastpenalty"      , primTok LastPenaltyTok)
    , (_cs "parshape"         , primTok ParagraphShapeTok)
    , (_cs "badness"          , primTok BadnessTok)
    , (_cs "inputlineno"      , primTok InputLineNrTok)
      -- Internal length.
    , (_cs "lastkern"         , primTok LastKernTok)
    , (_cs "fontdimen"        , primTok FontDimensionTok)
    , (_cs "ht"               , primTok $ BoxDimensionTok Height)
    , (_cs "wd"               , primTok $ BoxDimensionTok Width)
    , (_cs "dp"               , primTok $ BoxDimensionTok Depth)
      -- Internal glue.
    , (_cs "lastskip"         , primTok LastGlueTok)
      -- Specifying a box.
    , (_cs "box"              , primTok $ FetchedBoxTok Pop)
    , (_cs "copy"             , primTok $ FetchedBoxTok Lookup)
    , (_cs "lastbox"          , primTok LastBoxTok)
    , (_cs "vsplit"           , primTok SplitVBoxTok)
    , (_cs "hbox"             , primTok HBoxTok)
    , (_cs "vbox"             , primTok $ VBoxTok False)
    , (_cs "vtop"             , primTok $ VBoxTok True)
    , (_cs "setbox"           , primTok SetBoxRegisterTok)
      -- Stream.
    , (_cs "read"             , primTok ReadTok)
      -- Font.
    , (_cs "font"             , primTok FontTok)
    , (_cs "hyphenchar"       , primTok $ FontCharTok HyphenChar)
    , (_cs "skewchar"         , primTok $ FontCharTok SkewChar)
    -- Hyphenation.
    , (_cs "hyphenation"      , primTok HyphenationTok)
    , (_cs "patterns"         , primTok HyphenationPatternsTok)

      -- Interaction mode.
    , (_cs "errorstopmode"    , primTok $ InteractionModeTok ErrorStopMode)
    , (_cs "scrollmode"       , primTok $ InteractionModeTok ScrollMode)
    , (_cs "nonstopmode"      , primTok $ InteractionModeTok NonStopMode)
    , (_cs "batchmode"        , primTok $ InteractionModeTok BatchMode)
    ]
