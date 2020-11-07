{-# LANGUAGE DeriveAnyClass #-}
module Hex.Resolve.Token where

import qualified Data.Aeson as Ae
import qualified Data.Ascii as Ascii
import qualified Data.Map.Strict as Map
import qualified Hex.BreakList.Elem as BL.E
import Hex.Config.Codes
import qualified Hex.Lex as Lex
import Hex.Quantity
import Hexlude

-- mconcat on this newtype wrapper should get the final sign of a list of
-- signs. Bit pretentious, sorry.
data Sign
  = Positive
  | Negative
  deriving stock (Show, Eq, Generic)

instance Semigroup Sign where

  a <> b = if a == b then Positive else Negative

instance Monoid Sign where

  mempty = Positive

instance ToJSON Sign

instance Describe Sign where

  describe Positive = singleLine "Sign/+"
  describe Negative = singleLine "Sign/-"

data Signed a = Signed Sign a
  deriving stock (Functor, Generic)

deriving stock instance Show a => Show (Signed a)

instance ToJSON a => ToJSON (Signed a)

instance Describe a => Describe (Signed a) where

  describe (Signed sign v) = case sign of
    Positive -> describePrepended 0 "+" v
    Negative -> describePrepended 0 "-" v

evalSigned :: Num a => Signed a -> a
evalSigned (Signed sign a) = case sign of
  Positive -> a
  Negative -> -a

data TeXIntParameter
  = PreTolerance -- Badness tolerance before hyphenation
  | Tolerance -- Badness tolerance after hyphenation
  | HBadness -- Badness above which bad hboxes will be shown
  | VBadness -- Badness above which bad vboxes will be shown
  | LinePenalty -- Amount added to badness of every line in a paragraph
  | HyphenPenalty -- Penalty for line break after discretionary hyphen
  | ExHyphenPenalty -- Penalty for line break after explicit hyphen
  | BinOpPenalty -- Penalty for line break after binary operation
  | RelPenalty -- Penalty for line break after math relation
  | ClubPenalty -- Penalty for creating a club line at bottom of page
  | WidowPenalty -- Penalty for creating a widow line at top of page
  | DisplayWidowPenalty -- Ditto, before a display
  | BrokenPenalty -- Penalty for page break after a hyphenated line
  | PreDisplayPenalty -- Penalty for page break just before a display
  | PostDisplayPenalty -- Penalty for page break just after a display
  | InterlinePenalty -- Additional penalty for page break between lines
  | FloatingPenalty -- Penalty for insertions that are split
  | OutputPenalty -- Penalty at the current page break
  | DoubleHyphenDemerits -- Demerits for consecutive broken lines
  | FinalHyphenDemerits -- Demerits for a penultimate broken line
  | AdjDemerits -- Demerits for adjacent incompatible lines
  | Looseness -- Change to the number of lines in a paragraph
  | Pausing -- Positive if pausing after each line is read from a file
  | HoldingInserts -- Positive if insertions remain dormant in output box
  | TracingOnline -- Positive if showing diagnostic info on the terminal
  | TracingMacros -- Positive if showing macros as they are expanded
  | TracingStats -- Positive if showing statistics about memory usage
  | TracingParagraphs -- Positive if showing line-break calculations
  | TracingPages -- Positive if showing page-break calculations
  | TracingOutput -- Positive if showing boxes that are shipped out
  | TracingLostChars -- Positive if showing characters not in the font
  | TracingCommands -- Positive if showing commands before they are executed
  | TracingRestores -- Positive if showing deassignments when groups end
  | Language -- The current set of hyphenation rules
  | UCHyph -- Positive if hyphenating words beginning with capital letters
  | LeftHyphenMin -- Smallest fragment at beginning of hyphenated word
  | RightHyphenMin -- Smallest fragment at end of hyphenated word
  | GlobalDefs -- Nonzero if overriding \global specifications
  | DefaultHyphenChar -- \hyphenchar value when a font is loaded
  | DefaultSkewChar -- \skewchar value when a font is loaded
  | EscapeChar -- Escape character in the output of control-sequence tokens
  | EndLineChar -- Character placed at the right end of an input line
  | NewLineChar -- Character that starts a new output line
  | MaxDeadCycles -- Upper bound on \deadcycles
  | HangAfter -- Hanging indentation changes after this many lines
  | Fam -- The current family number
  | Mag -- Magnification ratio, times 1000
  | DelimiterFactor -- Ratio for variable delimiters, times 1000
  | Time -- Current time of day in minutes since midnight
  | Day -- Current day of the month
  | Month -- Current month of the year
  | Year -- Current year of our Lord
  | ShowBoxBreadth -- Maximum items per level when boxes are shown
  | ShowBoxDepth -- Maximum level when boxes are shown
  | ErrorContextLines -- Maximum extra context shown when errors occur
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, Hashable)

data LengthParameter
  = HFuzz -- Maximum overrun before overfull hbox messages occur
  | VFuzz -- Maximum overrun before overfull vbox messages occur
  | OverfullRule -- Width of rules appended to overfull boxes
  | EmergencyStretch -- Reduces badnesses on final pass of line-breaking
  | HSize -- Line width in horizontal mode
  | VSize -- Page height in vertical mode
  | MaxDepth -- Maximum depth of boxes on main pages
  | SplitMaxDepth -- Maximum depth of boxes on split pages
  | BoxMaxDepth -- Maximum depth of boxes on explicit pages
  | LineSkipLimit -- Threshold where \baselineskip changes to \lineskip
  | DelimiterShortfall -- Maximum space not covered by a delimiter
  | NullDelimiterSpace -- Width of a null delimiter
  | ScriptSpace -- Extra space after subscript or superscript
  | MathSurround -- Kerning before and after math in text
  | PreDisplaySize -- Length of text preceding a display
  | DisplayWidth -- Length of line for displayed equation
  | DisplayIndent -- Indentation of line for displayed equation
  | ParIndent -- Width of \indent
  | HangIndent -- Amount of hanging indentation
  | HOffset -- Horizontal offset in \shipout
  | VOffset -- Vertical offset in \shipout
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, Hashable)

data GlueParameter
  = BaselineSkip -- Desired glue between baselines
  | LineSkip -- Interline glue if \baselineskip isn't feasible
  | ParSkip -- Extra glue just above paragraphs
  | AboveDisplaySkip -- Extra glue just above displays
  | AboveDisplayShortSkip -- Ditto, following short lines
  | BelowDisplaySkip -- Extra glue just below Displays
  | BelowDisplayShortSkip -- Ditto, following short lines
  | LeftSkip -- Glue at left of justified lines
  | RightSkip -- Glue at right of justified lines
  | TopSkip -- Glue at top of main pages
  | SplitTopSkip -- Glue at top of split pages
  | TabSkip -- Glue between aligned entries
  | SpaceSkip -- Glue between words, if nonzero
  | XSpaceSkip -- Glue between sentences, if nonzero
  | ParFillSkip -- Additional \rightskip at end of paragraphs
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, Hashable)

data MathGlueParameter
  = ThinMuSkip -- Thin space in math formulas
  | MedMuSkip -- Medium space in math formulas
  | ThickMuSkip -- Thick space in math formulas
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, Hashable)

data TokenListParameter
  = Output -- The user's output routine
  | EveryPar -- Tokens to insert when a paragraph begins
  | EveryMath -- Tokens to insert when math in text begins
  | EveryDisplay -- Tokens to insert when display math begins
  | EveryHBox -- Tokens to insert when an hbox begins
  | EveryVBox -- Tokens to insert when a vbox begins
  | EveryJob -- Tokens to insert when the job begins
  | EveryCR -- Tokens to insert after every \cr or nonredundant \crcr
  | ErrHelp -- Tokens that supplement an \errmessage
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, Hashable)

data SpecialTeXInt
  = SpaceFactorTeXInt
  | PrevGrafTeXInt
  | DeadCyclesTeXInt
  | InsertPenaltiesTeXInt
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, Hashable)

data SpecialLength
  = PrevDepth
  | PageGoal
  | PageTotal
  | PageStretch
  | PageFilStretch
  | PageFillStretch
  | PageFilllStretch
  | PageShrink
  | PageDepth
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, Hashable)

data AssignPrefixTok
  = LongTok
  | OuterTok
  | GlobalTok
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data IndentFlag
  = Indent
  | DoNotIndent
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data ExpandDefFlag
  = ExpandDef
  | InhibitDef
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data ScopeFlag
  = Global
  | Local
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data StandardOutputStream
  = StdOut
  | StdErr
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data BoxFetchMode
  = Pop
  | Lookup
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data PresetGlueType
  = Fil -- \{h,v}fil
  | Fill -- \{h,v}fill
  | StretchOrShrink -- \{h,v}ss
  | FilNeg -- \{h,v}filneg
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data LeadersType
  = Aligned -- \leaders
  | Centered -- \cleaders
  | Expanded -- \xleaders
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data ModeAttribute
  = VerticalMode
  | HorizontalMode
  | MathMode
  | InnerMode
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data BoxRegisterAttribute
  = HasVerticalBox
  | HasHorizontalBox
  | IsVoid
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data FontChar
  = HyphenChar -- \hyphenchar
  | SkewChar -- \skewchar
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data FontRange
  = TextSizeFontRange -- \textfont
  | ScriptSizeFontRange -- \scriptfont
  | ScriptScriptSizeFontRange -- \scriptscriptfont
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance Hashable FontRange

data ModedCommandPrimitiveToken
  = SpecifiedGlueTok -- \vskip, \hskip
  | PresetGlueTok PresetGlueType -- \{v,h}{fil,fill,filneg,ss}
  | AlignedMaterialTok -- \halign, \valign
  | ShiftedBoxTok Direction -- \moveleft, \moveright, \raise, \lower
  | UnwrappedFetchedBoxTok BoxFetchMode -- \un{v,h}{box,copy}
  | RuleTok -- \hrule, \vrule
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data SyntaxCommandArg
  = EndCSNameTok -- \endcsname
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data CodeType
  = CategoryCodeType
  | MathCodeType
  | ChangeCaseCodeType VDirection
  | SpaceFactorCodeType
  | DelimiterCodeType
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data QuantityType
  = CharQuantity -- \chardef
  | MathCharQuantity -- \mathchardef
  | RegQuantity RegisterType
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data RegisterType
  = RegInt -- \count, \countdef
  | RegLen -- \dimen, \dimendef
  | RegGlue -- \skip, \skipdef
  | RegMathGlue -- \muskip, \muskipdef
  | RegTokenList -- \toks, \toksdef
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data InteractionMode
  = ErrorStopMode -- \errorstopmode
  | ScrollMode -- \scrollmode
  | NonStopMode -- \nonstopmode
  | BatchMode -- \batchmode
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data Digit
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving stock (Eq, Ord, Bounded, Enum, Show, Generic)
  deriving anyclass (ToJSON)

instance Ae.ToJSONKey Digit where
  toJSONKey = Ae.genericToJSONKey Ae.defaultJSONKeyOptions

digitToChar :: Digit -> CharCode
digitToChar d =
  CharCode $ case d of
    One -> Ascii._1
    Two -> Ascii._2
    Three -> Ascii._3
    Four -> Ascii._4
    Five -> Ascii._5
    Six -> Ascii._6
    Seven -> Ascii._7
    Eight -> Ascii._8
    Nine -> Ascii._9

charCodeToDigit :: CharCode -> Maybe Digit
charCodeToDigit cc = case unsafeCodeAsChar cc of
  '1' -> Just One
  '2' -> Just Two
  '3' -> Just Three
  '4' -> Just Four
  '5' -> Just Five
  '6' -> Just Six
  '7' -> Just Seven
  '8' -> Just Eight
  '9' -> Just Nine
  _ -> Nothing

newtype BalancedText = BalancedText (Seq Lex.Token)
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)
  deriving newtype (Eq, Semigroup, Monoid)

newtype ExpandedBalancedText = ExpandedBalancedText (Seq PrimitiveToken)
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)
  deriving newtype (Eq, Semigroup, Monoid)

-- We use a map to restrict our parameter keys' domain to [1..9].
type MacroParameters = Map.Map Digit BalancedText

-- A token in a macro template.
-- TODO: Technically, we could narrow the domain of a MacroTextLexToken,
-- because we should know that we won't have a 'Parameter'-category token.
data MacroTextToken
  = -- A 'normal' token.
    MacroTextLexToken Lex.Token
  | -- A token to be substituted by a macro argument.
    MacroTextParamToken Digit
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- A macro template.
newtype MacroText = MacroText (Seq MacroTextToken)
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)
  deriving newtype Eq

data MacroContents
  = MacroContents
      { -- Tokens to expect before the first argument.
        preParamTokens :: BalancedText
      , parameters :: MacroParameters
      , replacementTokens :: MacroText
      , long, outer :: Bool
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data RemovableItem
  = PenaltyItem -- \unpenalty
  | KernItem -- \unkern
  | GlueItem -- \unskip
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data ExplicitBox
  = ExplicitHBox -- \hbox
  | ExplicitVBox BL.E.VBoxAlignType
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data PrimitiveToken
  = SyntaxCommandArg SyntaxCommandArg
  | -- Starters of commands.
    RelaxTok -- \relax
  | ChangeScopeCSTok Sign
  | ShowTokenTok -- \show
  | ShowBoxTok -- \showbox
  | ShowListsTok -- \showlists
  | ShowTheInternalQuantityTok -- \showthe
  | ShipOutTok -- \shipout
  | IgnoreSpacesTok -- \ignorespaces
  | SetAfterAssignmentTokenTok -- \afterassignment
  | AddToAfterGroupTokensTok -- \aftergroup
  | MessageTok StandardOutputStream -- \message, \errmessage
  | ImmediateTok -- \immediate
  | OpenInputTok -- \openin
  | CloseInputTok -- \closein
  | OpenOutputTok -- \openout
  | CloseOutputTok -- \closeout
  | WriteTok -- \write
  | DoSpecialTok -- \special
  | PenaltyTok -- \penalty
  | KernTok -- \kern
  | MathKernTok -- \mkern
  | RemoveItemTok RemovableItem
  | MarkTok -- \mark
  | InsertionTok -- \insert
  | LeadersTok LeadersType
  | StartParagraphTok IndentFlag -- \indent, \noindent
  | EndParagraphTok -- \par
    -- Starters of mode-specific commands with almost mode-independent grammar.
  | ModedCommand Axis ModedCommandPrimitiveToken
  | -- Starters of Vertical-Mode-specific commands.
    EndTok -- \end
  | DumpTok -- \dump
    -- Starters of Horizontal-Mode-specific commands.
  | ControlSpaceTok -- \␣ (a control symbol named ' ')
  | ControlCharTok -- \char
  | AccentTok -- \accent
  | ItalicCorrectionTok -- \/
  | DiscretionaryTextTok -- \discretionary
  | DiscretionaryHyphenTok -- \-
  | ToggleMathModeTok -- '$'
    -- > > Modifying how to apply assignments.
  | AssignPrefixTok AssignPrefixTok
  | -- > > Modifying how to parse the macro.
    --     \def, \gdef, \edef (expanded-def), \xdef (global-expanded-def).
    DefineMacroTok ScopeFlag ExpandDefFlag
  | -- > Setting variable values.
    IntParamVarTok TeXIntParameter
  | LenParamVarTok LengthParameter
  | GlueParamVarTok GlueParameter
  | MathGlueParamVarTok MathGlueParameter
  | TokenListParamVarTok TokenListParameter
  | SpecialTeXIntTok SpecialTeXInt -- \example: \spacefactor
  | SpecialLengthTok SpecialLength -- \example: \pagestretch
      -- Tokens storing integers defined by short-hand definitions.
  | IntRefTok QuantityType TeXInt
  | -- A char-cat pair defined by a 'let' assignment. This differs from a
    -- \chardef target, because \chardef maps to a character number, which is
    -- categorised at the time of use, while a \let maps to a static char-cat
    -- pair.
    LetCharCat Lex.CharCat
  | -- A control sequence representing a particular font, such as defined through
    -- \font.
    FontRefToken TeXInt
  | -- Heads of register references.
    RegisterVariableTok RegisterType
  | -- Heads of int-ref definitions.
    ShortDefHeadTok QuantityType
  | -- > Modifying variable values with arithmetic.
    AdvanceVarTok -- \advance
  | ScaleVarTok VDirection -- \multiply, \divide.
  | CodeTypeTok CodeType
  | -- > Aliasing tokens.
    LetTok -- \let
  | FutureLetTok -- \futurelet
    -- > Setting font math-family-member things.
  | FontRangeTok FontRange
  | -- > Internal integers.
    LastPenaltyTok -- \lastpenalty
  | ParagraphShapeTok -- \parshape
  | BadnessTok -- \badness
  | InputLineNrTok -- \inputlineno
    -- Internal lengths.
  | LastKernTok -- \lastkern
  | FontDimensionTok -- \fontdimen
  | BoxDimensionTok BoxDim -- \ht, \wd, \dp
      -- Internal glues.
  | LastGlueTok -- \lastskip
    -- Specifying boxes.
  | FetchedBoxTok BoxFetchMode -- \box, \copy
  | LastBoxTok -- \lastbox
  | SplitVBoxTok -- \vsplit
  | ExplicitBoxTok ExplicitBox
  | -- > Setting the contents of a box register.
    SetBoxRegisterTok -- \setbox
    -- > Reading contents into control sequences (not sure what this is about).
  | ReadTok -- \read
    -- > Defining macros resolving to a font.
  | FontTok -- \font
    -- Involved in global assignments.
    -- > Setting properties of a font.
  | FontCharTok FontChar
  | -- > Configuring hyphenation.
    HyphenationTok -- \hyphenation
  | HyphenationPatternsTok -- \patterns
    -- > Setting interaction mode.
  | InteractionModeTok InteractionMode
  | UnresolvedTok Lex.Token
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance Describe PrimitiveToken where

  describe = \case
    UnresolvedTok lt ->
      describePrepended 0 "PrimitiveToken/UnresolvedTok" lt
    x -> singleLine $ show x

data TokenAttribute
  = CharCodeAttribute -- \if
  | CatCodeAttribute -- \ifcat
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data MarkRegister
  = TopMark -- \topmark
  | FirstMark -- \firstmark
  | BottomMark -- \botmark
  | SplitFirstMark -- \splitfirstmark
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data ConditionTok
  = ConditionHeadTok ConditionHeadTok
  | ConditionBodyTok ConditionBodyTok
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data ConditionHeadTok
  = IfTeXIntPairTestTok -- \ifnum
  | IfLengthPairTestTok -- \ifdim
  | IfTeXIntOddTok -- \ifodd
  | IfInModeTok ModeAttribute -- \ifvmode, \ifhmode, \ifmmode, \ifinner
  | IfTokenAttributesEqualTok TokenAttribute
  | IfTokensEqualTok -- \ifx
  | IfBoxRegisterIsTok BoxRegisterAttribute -- \ifvoid, \ifhbox, \ifvbox
  | IfInputEndedTok -- \ifeof
  | IfConstTok Bool -- \iftrue, \iffalse
  | CaseTok -- \ifcase
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data ConditionBodyTok
  = Else -- \else
  | Or -- \or
  | EndIf -- \fi
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data SyntaxCommandHeadToken
  = MacroTok MacroContents
  | ConditionTok ConditionTok
  | NumberTok -- \number
  | RomanNumeralTok -- \romannumeral
  | StringTok -- \string
  | JobNameTok -- \jobname
  | FontNameTok -- \fontname
  | MeaningTok -- \meaning
  | CSNameTok -- \csname
  | ExpandAfterTok -- \expandafter
  | NoExpandTok -- \noexpand
  | MarkRegisterTok MarkRegister
  | InputTok -- \input
  | EndInputTok -- \endinput
  | TheTok -- \the
  | ChangeCaseTok VDirection -- \uppercase, \lowercase
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance Describe SyntaxCommandHeadToken where

  describe = singleLine . show

-- TODO: I think we can make this independent of parsing, and move it out.
data ResolvedToken
  = SyntaxCommandHeadToken SyntaxCommandHeadToken
  | PrimitiveToken PrimitiveToken
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance Describe ResolvedToken where

  describe = singleLine . show
