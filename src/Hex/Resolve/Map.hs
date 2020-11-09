module Hex.Resolve.Map
  ( defaultCSMap,
    CSMap
  ) where

import qualified Data.HashMap.Strict as HMap
import qualified Hex.BreakList.Elem as BL.E
import qualified Hex.Config.Codes as Code
import qualified Hex.Lex as Lex
import Hex.Resolve.Token
import Hexlude

type CSMap = HMap.HashMap Lex.ControlSequenceLike ResolvedToken

cs :: [Char] -> Lex.ControlSequenceLike
cs =
  Lex.ControlSequenceProper
    . Lex.mkControlSequence
    . fmap Code.unsafeCodeFromChar

syntaxTok :: SyntaxCommandHeadToken -> ResolvedToken
syntaxTok = SyntaxCommandHeadToken

primTok :: PrimitiveToken -> ResolvedToken
primTok = PrimitiveToken

condTok :: ConditionHeadTok -> ResolvedToken
condTok e = syntaxTok $ ConditionTok $ ConditionHeadTok e

vModeTok :: ModedCommandPrimitiveToken -> ResolvedToken
vModeTok e = primTok $ ModedCommand Vertical e

hModeTok :: ModedCommandPrimitiveToken -> ResolvedToken
hModeTok e = primTok $ ModedCommand Horizontal e

defaultCSMap :: CSMap
defaultCSMap =
  HMap.fromList
    [ (cs "ifnum", condTok IfTeXIntPairTestTok),
      (cs "ifdim", condTok IfLengthPairTestTok),
      (cs "ifodd", condTok IfTeXIntOddTok),
      (cs "ifvmode", condTok $ IfInModeTok VerticalMode),
      (cs "ifhmode", condTok $ IfInModeTok HorizontalMode),
      (cs "ifmmode", condTok $ IfInModeTok MathMode),
      (cs "ifinner", condTok $ IfInModeTok InnerMode),
      (cs "if", condTok $ IfTokenAttributesEqualTok CharCodeAttribute),
      (cs "ifcat", condTok $ IfTokenAttributesEqualTok CatCodeAttribute),
      (cs "ifx", condTok IfTokensEqualTok),
      (cs "ifvoid", condTok $ IfBoxRegisterIsTok IsVoid),
      (cs "ifvbox", condTok $ IfBoxRegisterIsTok HasVerticalBox),
      (cs "ifhbox", condTok $ IfBoxRegisterIsTok HasHorizontalBox),
      (cs "ifeof", condTok IfInputEndedTok),
      (cs "iftrue", condTok $ IfConstTok True),
      (cs "iffalse", condTok $ IfConstTok False),
      (cs "ifcase", condTok CaseTok),
      -- Tokens used in the body of condition blocks.
      (cs "else", syntaxTok $ ConditionTok $ ConditionBodyTok Else),
      (cs "or", syntaxTok $ ConditionTok $ ConditionBodyTok Or),
      (cs "fi", syntaxTok $ ConditionTok $ ConditionBodyTok EndIf),
      (cs "number", syntaxTok NumberTok),
      (cs "romannumeral", syntaxTok RomanNumeralTok),
      (cs "string", syntaxTok StringTok),
      (cs "jobname", syntaxTok JobNameTok),
      (cs "fontname", syntaxTok FontNameTok),
      (cs "meaning", syntaxTok MeaningTok),
      (cs "csname", syntaxTok CSNameTok),
      (cs "expandafter", syntaxTok ExpandAfterTok),
      (cs "noexpand", syntaxTok NoExpandTok),
      (cs "topmark", syntaxTok $ MarkRegisterTok TopMark),
      (cs "firstmark", syntaxTok $ MarkRegisterTok FirstMark),
      (cs "botmark", syntaxTok $ MarkRegisterTok BottomMark),
      (cs "splitfirstmark", syntaxTok $ MarkRegisterTok SplitFirstMark),
      (cs "input", syntaxTok InputTok),
      (cs "endinput", syntaxTok EndInputTok),
      (cs "the", syntaxTok TheTok),
      (cs "uppercase", syntaxTok $ ChangeCaseTok Upward),
      (cs "lowercase", syntaxTok $ ChangeCaseTok Downward),
      -- Arguments of syntax commands.
      (cs "endcsname", primTok $ SyntaxCommandArg EndCSNameTok),
      -- Nothing special.
      (cs "relax", primTok RelaxTok),
      (cs "begingroup", primTok $ ChangeScopeCSTok Positive),
      (cs "endgroup", primTok $ ChangeScopeCSTok Negative),
      (cs "show", primTok ShowTokenTok),
      (cs "showbox", primTok ShowBoxTok),
      (cs "showlists", primTok ShowListsTok),
      (cs "showthe", primTok ShowTheInternalQuantityTok),
      (cs "shipout", primTok ShipOutTok),
      (cs "ignorespaces", primTok IgnoreSpacesTok),
      (cs "afterassignment", primTok SetAfterAssignmentTokenTok),
      (cs "aftergroup", primTok AddToAfterGroupTokensTok),
      (cs "message", primTok $ MessageTok StdOut),
      (cs "errmessage", primTok $ MessageTok StdErr),
      (cs "immediate", primTok ImmediateTok),
      (cs "openin", primTok OpenInputTok),
      (cs "closein", primTok CloseInputTok),
      (cs "openout", primTok OpenOutputTok),
      (cs "closeout", primTok CloseOutputTok),
      (cs "write", primTok WriteTok),
      (cs "special", primTok DoSpecialTok),
      (cs "penalty", primTok PenaltyTok),
      (cs "kern", primTok KernTok),
      (cs "mkern", primTok MathKernTok),
      (cs "unpenalty", primTok $ RemoveItemTok PenaltyItem),
      (cs "unkern", primTok $ RemoveItemTok KernItem),
      (cs "unskip", primTok $ RemoveItemTok GlueItem),
      (cs "mark", primTok MarkTok),
      (cs "insert", primTok InsertionTok),
      (cs "leaders", primTok $ LeadersTok Aligned),
      (cs "cleaders", primTok $ LeadersTok Centered),
      (cs "xleaders", primTok $ LeadersTok Expanded),
      (cs "indent", primTok $ StartParagraphTok Indent),
      (cs "noindent", primTok $ StartParagraphTok DoNotIndent),
      (cs "par", primTok EndParagraphTok),
      -- Glue.
      (cs "vskip", vModeTok SpecifiedGlueTok),
      (cs "hskip", hModeTok SpecifiedGlueTok),
      (cs "vfil", vModeTok $ PresetGlueTok Fil),
      (cs "hfil", hModeTok $ PresetGlueTok Fil),
      (cs "vfill", vModeTok $ PresetGlueTok Fill),
      (cs "hfill", hModeTok $ PresetGlueTok Fill),
      (cs "vfilneg", vModeTok $ PresetGlueTok FilNeg),
      (cs "hfilneg", hModeTok $ PresetGlueTok FilNeg),
      (cs "vss", vModeTok $ PresetGlueTok StretchOrShrink),
      (cs "hss", hModeTok $ PresetGlueTok StretchOrShrink),
      -- Other moded.
      (cs "halign", hModeTok AlignedMaterialTok),
      (cs "valign", vModeTok AlignedMaterialTok),
      (cs "moveleft", hModeTok $ ShiftedBoxTok Backward),
      (cs "moveright", hModeTok $ ShiftedBoxTok Forward),
      (cs "raise", vModeTok $ ShiftedBoxTok Backward),
      (cs "lower", vModeTok $ ShiftedBoxTok Forward),
      (cs "unvbox", vModeTok $ UnwrappedFetchedBoxTok Pop),
      (cs "unhbox", hModeTok $ UnwrappedFetchedBoxTok Pop),
      (cs "unvcopy", vModeTok $ UnwrappedFetchedBoxTok Lookup),
      (cs "unhcopy", hModeTok $ UnwrappedFetchedBoxTok Lookup),
      (cs "hrule", vModeTok RuleTok),
      (cs "vrule", hModeTok RuleTok),
      -- Final commands.
      (cs "end", primTok EndTok),
      (cs "dump", primTok DumpTok),
      -- Starters of Horizontal mode commands.
      (cs " ", primTok ControlSpaceTok),
      (cs "char", primTok ControlCharTok),
      (cs "accent", primTok AccentTok),
      (cs "/", primTok ItalicCorrectionTok),
      (cs "discretionary", primTok DiscretionaryTextTok),
      (cs "-", primTok DiscretionaryHyphenTok),
      -- Macro prefixes.
      (cs "long", primTok $ AssignPrefixTok LongTok),
      (cs "outer", primTok $ AssignPrefixTok OuterTok),
      (cs "global", primTok $ AssignPrefixTok GlobalTok),
      -- Macro def types.
      (cs "def", primTok $ DefineMacroTok Local InhibitDef),
      (cs "edef", primTok $ DefineMacroTok Local ExpandDef),
      (cs "gdef", primTok $ DefineMacroTok Global InhibitDef),
      (cs "xdef", primTok $ DefineMacroTok Global ExpandDef),
      -- TeXInt parameters.
      (cs "pretolerance", primTok $ IntParamVarTok PreTolerance),
      (cs "tolerance", primTok $ IntParamVarTok Tolerance),
      (cs "hbadness", primTok $ IntParamVarTok HBadness),
      (cs "vbadness", primTok $ IntParamVarTok VBadness),
      (cs "linepenalty", primTok $ IntParamVarTok LinePenalty),
      (cs "hyphenpenalty", primTok $ IntParamVarTok HyphenPenalty),
      (cs "exhyphenpenalty", primTok $ IntParamVarTok ExHyphenPenalty),
      (cs "binoppenalty", primTok $ IntParamVarTok BinOpPenalty),
      (cs "relpenalty", primTok $ IntParamVarTok RelPenalty),
      (cs "clubpenalty", primTok $ IntParamVarTok ClubPenalty),
      (cs "widowpenalty", primTok $ IntParamVarTok WidowPenalty),
      (cs "displaywidowpenalty", primTok $ IntParamVarTok DisplayWidowPenalty),
      (cs "brokenpenalty", primTok $ IntParamVarTok BrokenPenalty),
      (cs "predisplaypenalty", primTok $ IntParamVarTok PreDisplayPenalty),
      (cs "postdisplaypenalty", primTok $ IntParamVarTok PostDisplayPenalty),
      (cs "interlinepenalty", primTok $ IntParamVarTok InterlinePenalty),
      (cs "floatingpenalty", primTok $ IntParamVarTok FloatingPenalty),
      (cs "outputpenalty", primTok $ IntParamVarTok OutputPenalty),
      (cs "doublehyphendemerits", primTok $ IntParamVarTok DoubleHyphenDemerits),
      (cs "finalhyphendemerits", primTok $ IntParamVarTok FinalHyphenDemerits),
      (cs "adjdemerits", primTok $ IntParamVarTok AdjDemerits),
      (cs "looseness", primTok $ IntParamVarTok Looseness),
      (cs "pausing", primTok $ IntParamVarTok Pausing),
      (cs "holdinginserts", primTok $ IntParamVarTok HoldingInserts),
      (cs "tracingonline", primTok $ IntParamVarTok TracingOnline),
      (cs "tracingmacros", primTok $ IntParamVarTok TracingMacros),
      (cs "tracingstats", primTok $ IntParamVarTok TracingStats),
      (cs "tracingparagraphs", primTok $ IntParamVarTok TracingParagraphs),
      (cs "tracingpages", primTok $ IntParamVarTok TracingPages),
      (cs "tracingoutput", primTok $ IntParamVarTok TracingOutput),
      (cs "tracinglostchars", primTok $ IntParamVarTok TracingLostChars),
      (cs "tracingcommands", primTok $ IntParamVarTok TracingCommands),
      (cs "tracingrestores", primTok $ IntParamVarTok TracingRestores),
      (cs "language", primTok $ IntParamVarTok Language),
      (cs "uchyph", primTok $ IntParamVarTok UCHyph),
      (cs "lefthyphenmin", primTok $ IntParamVarTok LeftHyphenMin),
      (cs "righthyphenmin", primTok $ IntParamVarTok RightHyphenMin),
      (cs "globaldefs", primTok $ IntParamVarTok GlobalDefs),
      (cs "defaulthyphenchar", primTok $ IntParamVarTok DefaultHyphenChar),
      (cs "defaultskewchar", primTok $ IntParamVarTok DefaultSkewChar),
      (cs "escapechar", primTok $ IntParamVarTok EscapeChar),
      (cs "endlinechar", primTok $ IntParamVarTok EndLineChar),
      (cs "newlinechar", primTok $ IntParamVarTok NewLineChar),
      (cs "maxdeadcycles", primTok $ IntParamVarTok MaxDeadCycles),
      (cs "hangafter", primTok $ IntParamVarTok HangAfter),
      (cs "fam", primTok $ IntParamVarTok Fam),
      (cs "mag", primTok $ IntParamVarTok Mag),
      (cs "delimiterfactor", primTok $ IntParamVarTok DelimiterFactor),
      (cs "time", primTok $ IntParamVarTok Time),
      (cs "day", primTok $ IntParamVarTok Day),
      (cs "month", primTok $ IntParamVarTok Month),
      (cs "year", primTok $ IntParamVarTok Year),
      (cs "showboxbreadth", primTok $ IntParamVarTok ShowBoxBreadth),
      (cs "showboxdepth", primTok $ IntParamVarTok ShowBoxDepth),
      (cs "errorcontextlines", primTok $ IntParamVarTok ErrorContextLines),
      -- Length parameters.
      (cs "hfuzz", primTok $ LenParamVarTok HFuzz),
      (cs "vfuzz", primTok $ LenParamVarTok VFuzz),
      (cs "overfullrule", primTok $ LenParamVarTok OverfullRule),
      (cs "emergencystretch", primTok $ LenParamVarTok EmergencyStretch),
      (cs "hsize", primTok $ LenParamVarTok HSize),
      (cs "vsize", primTok $ LenParamVarTok VSize),
      (cs "maxdepth", primTok $ LenParamVarTok MaxDepth),
      (cs "splitmaxdepth", primTok $ LenParamVarTok SplitMaxDepth),
      (cs "boxmaxdepth", primTok $ LenParamVarTok BoxMaxDepth),
      (cs "lineskiplimit", primTok $ LenParamVarTok LineSkipLimit),
      (cs "delimitershortfall", primTok $ LenParamVarTok DelimiterShortfall),
      (cs "nulldelimiterspace", primTok $ LenParamVarTok NullDelimiterSpace),
      (cs "scriptspace", primTok $ LenParamVarTok ScriptSpace),
      (cs "mathsurround", primTok $ LenParamVarTok MathSurround),
      (cs "predisplaysize", primTok $ LenParamVarTok PreDisplaySize),
      (cs "displaywidth", primTok $ LenParamVarTok DisplayWidth),
      (cs "displayindent", primTok $ LenParamVarTok DisplayIndent),
      (cs "parindent", primTok $ LenParamVarTok ParIndent),
      (cs "hangindent", primTok $ LenParamVarTok HangIndent),
      (cs "hoffset", primTok $ LenParamVarTok HOffset),
      (cs "voffset", primTok $ LenParamVarTok VOffset),
      -- Glue parameters.
      (cs "baselineskip", primTok $ GlueParamVarTok BaselineSkip),
      (cs "lineskip", primTok $ GlueParamVarTok LineSkip),
      (cs "parskip", primTok $ GlueParamVarTok ParSkip),
      (cs "abovedisplayskip", primTok $ GlueParamVarTok AboveDisplaySkip),
      (cs "abovedisplayshortskip", primTok $ GlueParamVarTok AboveDisplayShortSkip),
      (cs "belowdisplayskip", primTok $ GlueParamVarTok BelowDisplaySkip),
      (cs "belowdisplayshortskip", primTok $ GlueParamVarTok BelowDisplayShortSkip),
      (cs "leftskip", primTok $ GlueParamVarTok LeftSkip),
      (cs "rightskip", primTok $ GlueParamVarTok RightSkip),
      (cs "topskip", primTok $ GlueParamVarTok TopSkip),
      (cs "splittopskip", primTok $ GlueParamVarTok SplitTopSkip),
      (cs "tabskip", primTok $ GlueParamVarTok TabSkip),
      (cs "spaceskip", primTok $ GlueParamVarTok SpaceSkip),
      (cs "xspaceskip", primTok $ GlueParamVarTok XSpaceSkip),
      (cs "parfillskip", primTok $ GlueParamVarTok ParFillSkip),
      -- Math-glue parameters.
      (cs "thinmuskip", primTok $ MathGlueParamVarTok ThinMuSkip),
      (cs "medmuskip", primTok $ MathGlueParamVarTok MedMuSkip),
      (cs "thickmuskip", primTok $ MathGlueParamVarTok ThickMuSkip),
      -- Token list parameters.
      (cs "output", primTok $ TokenListParamVarTok Output),
      (cs "everypar", primTok $ TokenListParamVarTok EveryPar),
      (cs "everymath", primTok $ TokenListParamVarTok EveryMath),
      (cs "everydisplay", primTok $ TokenListParamVarTok EveryDisplay),
      (cs "everyhbox", primTok $ TokenListParamVarTok EveryHBox),
      (cs "everyvbox", primTok $ TokenListParamVarTok EveryVBox),
      (cs "everyjob", primTok $ TokenListParamVarTok EveryJob),
      (cs "everycr", primTok $ TokenListParamVarTok EveryCR),
      (cs "errhelp", primTok $ TokenListParamVarTok ErrHelp),
      -- Special integers.
      (cs "spacefactor", primTok $ SpecialTeXIntTok SpaceFactorTeXInt),
      (cs "prevgraf", primTok $ SpecialTeXIntTok PrevGrafTeXInt),
      (cs "deadcycles", primTok $ SpecialTeXIntTok DeadCyclesTeXInt),
      (cs "insertpenalties", primTok $ SpecialTeXIntTok InsertPenaltiesTeXInt),
      -- Special lengths.
      (cs "prevdepth", primTok $ SpecialLengthTok PrevDepth),
      (cs "pagegoal", primTok $ SpecialLengthTok PageGoal),
      (cs "pagetotal", primTok $ SpecialLengthTok PageTotal),
      (cs "pagestretch", primTok $ SpecialLengthTok PageStretch),
      (cs "pagefilstretch", primTok $ SpecialLengthTok PageFilStretch),
      (cs "pagefillstretch", primTok $ SpecialLengthTok PageFillStretch),
      (cs "pagefilllstretch", primTok $ SpecialLengthTok PageFilllStretch),
      (cs "pageshrink", primTok $ SpecialLengthTok PageShrink),
      (cs "pagedepth", primTok $ SpecialLengthTok PageDepth),
      -- Register reference type prefixes.
      (cs "count", primTok $ RegisterVariableTok RegInt),
      (cs "dimen", primTok $ RegisterVariableTok RegLen),
      (cs "skip", primTok $ RegisterVariableTok RegGlue),
      (cs "muskip", primTok $ RegisterVariableTok RegMathGlue),
      (cs "toks", primTok $ RegisterVariableTok RegTokenList),
      -- Short-hand definition heads.
      (cs "chardef", primTok $ ShortDefHeadTok CharQuantity),
      (cs "mathchardef", primTok $ ShortDefHeadTok MathCharQuantity),
      (cs "countdef", primTok $ ShortDefHeadTok $ RegQuantity RegInt),
      (cs "dimendef", primTok $ ShortDefHeadTok $ RegQuantity RegLen),
      (cs "skipdef", primTok $ ShortDefHeadTok $ RegQuantity RegGlue),
      (cs "muskipdef", primTok $ ShortDefHeadTok $ RegQuantity RegMathGlue),
      (cs "toksdef", primTok $ ShortDefHeadTok $ RegQuantity RegTokenList),
      -- Modify variables.
      (cs "advance", primTok AdvanceVarTok),
      (cs "multiply", primTok $ ScaleVarTok Upward),
      (cs "divide", primTok $ ScaleVarTok Downward),
      -- Code types.
      (cs "catcode", primTok $ CodeTypeTok CategoryCodeType),
      (cs "mathcode", primTok $ CodeTypeTok MathCodeType),
      (cs "lccode", primTok $ CodeTypeTok $ ChangeCaseCodeType Downward),
      (cs "uccode", primTok $ CodeTypeTok $ ChangeCaseCodeType Upward),
      (cs "sfcode", primTok $ CodeTypeTok SpaceFactorCodeType),
      (cs "delcode", primTok $ CodeTypeTok DelimiterCodeType),
      -- Alias tokens.
      (cs "let", primTok LetTok),
      (cs "futurelet", primTok FutureLetTok),
      -- Font range.
      (cs "textfont", primTok $ FontRangeTok TextSizeFontRange),
      (cs "scriptfont", primTok $ FontRangeTok ScriptSizeFontRange),
      (cs "scriptscriptfont", primTok $ FontRangeTok ScriptScriptSizeFontRange),
      -- Internal integer.
      (cs "lastpenalty", primTok LastPenaltyTok),
      (cs "parshape", primTok ParagraphShapeTok),
      (cs "badness", primTok BadnessTok),
      (cs "inputlineno", primTok InputLineNrTok),
      -- Internal length.
      (cs "lastkern", primTok LastKernTok),
      (cs "fontdimen", primTok FontDimensionTok),
      (cs "ht", primTok $ BoxDimensionTok BoxHeight),
      (cs "wd", primTok $ BoxDimensionTok BoxWidth),
      (cs "dp", primTok $ BoxDimensionTok BoxDepth),
      -- Internal glue.
      (cs "lastskip", primTok LastGlueTok),
      -- Specifying a box.
      (cs "box", primTok $ FetchedBoxTok Pop),
      (cs "copy", primTok $ FetchedBoxTok Lookup),
      (cs "lastbox", primTok LastBoxTok),
      (cs "vsplit", primTok SplitVBoxTok),
      (cs "hbox", primTok $ ExplicitBoxTok ExplicitHBox),
      (cs "vbox", primTok $ ExplicitBoxTok $ ExplicitVBox BL.E.DefaultAlign),
      (cs "vtop", primTok $ ExplicitBoxTok $ ExplicitVBox BL.E.TopAlign),
      (cs "setbox", primTok SetBoxRegisterTok),
      -- Stream.
      (cs "read", primTok ReadTok),
      -- Font.
      (cs "font", primTok FontTok),
      (cs "hyphenchar", primTok $ FontCharTok HyphenChar),
      (cs "skewchar", primTok $ FontCharTok SkewChar),
      -- Hyphenation.
      (cs "hyphenation", primTok HyphenationTok),
      (cs "patterns", primTok HyphenationPatternsTok),
      -- Interaction mode.
      (cs "errorstopmode", primTok $ InteractionModeTok ErrorStopMode),
      (cs "scrollmode", primTok $ InteractionModeTok ScrollMode),
      (cs "nonstopmode", primTok $ InteractionModeTok NonStopMode),
      (cs "batchmode", primTok $ InteractionModeTok BatchMode)
    ]
