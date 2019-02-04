module HeX.Parse.Token where

import qualified Data.Map.Strict               as Map

import           HeX.Concept
import           HeX.Type
import qualified HeX.Lex                       as Lex

-- mconcat on this newtype wrapper should get the final sign of a list of
-- signs. Bit pretentious, sorry.
newtype Sign = Sign { getSign :: Bool }
    deriving (Show, Eq)

instance Semigroup Sign where
    Sign x <> Sign y = Sign $ x == y

instance Monoid Sign where
    mempty = Sign True

data IntegerParameter
    = PreTolerance           -- Badness tolerance before hyphenation
    | Tolerance              -- Badness tolerance after hyphenation
    | HBadness               -- Badness above which bad hboxes will be shown
    | VBadness               -- Badness above which bad vboxes will be shown
    | LinePenalty            -- Amount added to badness of every line in a paragraph
    | HyphenPenalty          -- Penalty for line break after discretionary hyphen
    | ExHyphenPenalty        -- Penalty for line break after explicit hyphen
    | BinOpPenalty           -- Penalty for line break after binary operation
    | RelPenalty             -- Penalty for line break after math relation
    | ClubPenalty            -- Penalty for creating a club line at bottom of page
    | WidowPenalty           -- Penalty for creating a widow line at top of page
    | DisplayWidowPenalty    -- Ditto, before a display
    | BrokenPenalty          -- Penalty for page break after a hyphenated line
    | PreDisplayPenalty      -- Penalty for page break just before a display
    | PostDisplayPenalty     -- Penalty for page break just after a display
    | InterlinePenalty       -- Additional penalty for page break between lines
    | FloatingPenalty        -- Penalty for insertions that are split
    | OutputPenalty          -- Penalty at the current page break
    | DoubleHyphenDemerits   -- Demerits for consecutive broken lines
    | FinalHyphenDemerits    -- Demerits for a penultimate broken line
    | AdjDemerits            -- Demerits for adjacent incompatible lines
    | Looseness              -- Change to the number of lines in a paragraph
    | Pausing                -- Positive if pausing after each line is read from a file
    | HoldingInserts         -- Positive if insertions remain dormant in output box
    | TracingOnline          -- Positive if showing diagnostic info on the terminal
    | TracingMacros          -- Positive if showing macros as they are expanded
    | TracingStats           -- Positive if showing statistics about memory usage
    | TracingParagraphs      -- Positive if showing line-break calculations
    | TracingPages           -- Positive if showing page-break calculations
    | TracingOutput          -- Positive if showing boxes that are shipped out
    | TracingLostChars       -- Positive if showing characters not in the font
    | TracingCommands        -- Positive if showing commands before they are executed
    | TracingRestores        -- Positive if showing deassignments when groups end
    | Language               -- The current set of hyphenation rules
    | UCHyph                 -- Positive if hyphenating words beginning with capital letters
    | LeftHyphenMin          -- Smallest fragment at beginning of hyphenated word
    | RightHyphenMin         -- Smallest fragment at end of hyphenated word
    | GlobalDefs             -- Nonzero if overriding \global specifications
    | DefaultHyphenChar      -- \hyphenchar value when a font is loaded
    | DefaultSkewChar        -- \skewchar value when a font is loaded
    | EscapeChar             -- Escape character in the output of control-sequence tokens
    | EndLineChar            -- Character placed at the right end of an input line
    | NewLineChar            -- Character that starts a new output line
    | MaxDeadCycles          -- Upper bound on \deadcycles
    | HangAfter              -- Hanging indentation changes after this many lines
    | Fam                    -- The current family number
    | Mag                    -- Magnification ratio, times 1000
    | DelimiterFactor        -- Ratio for variable delimiters, times 1000
    | Time                   -- Current time of day in minutes since midnight
    | Day                    -- Current day of the month
    | Month                  -- Current month of the year
    | Year                   -- Current year of our Lord
    | ShowBoxBreadth         -- Maximum items per level when boxes are shown
    | ShowBoxDepth           -- Maximum level when boxes are shown
    | ErrorContextLines      -- Maximum extra context shown when errors occur
    deriving (Show, Eq)

data LengthParameter
    = HFuzz                  -- Maximum overrun before overfull hbox messages occur
    | VFuzz                  -- Maximum overrun before overfull vbox messages occur
    | OverfullRule           -- Width of rules appended to overfull boxes
    | EmergencyStretch       -- Reduces badnesses on final pass of line-breaking
    | HSize                  -- Line width in horizontal mode
    | VSize                  -- Page height in vertical mode
    | MaxDepth               -- Maximum depth of boxes on main pages
    | SplitMaxDepth          -- Maximum depth of boxes on split pages
    | BoxMaxDepth            -- Maximum depth of boxes on explicit pages
    | LineSkipLimit          -- Threshold where \baselineskip changes to \lineskip
    | DelimiterShortfall     -- Maximum space not covered by a delimiter
    | NullDelimiterSpace     -- Width of a null delimiter
    | ScriptSpace            -- Extra space after subscript or superscript
    | MathSurround           -- Kerning before and after math in text
    | PreDisplaySize         -- Length of text preceding a display
    | DisplayWidth           -- Length of line for displayed equation
    | DisplayIndent          -- Indentation of line for displayed equation
    | ParIndent              -- Width of \indent
    | HangIndent             -- Amount of hanging indentation
    | HOffset                -- Horizontal offset in \shipout
    | VOffset                -- Vertical offset in \shipout
    deriving (Show, Eq)

data GlueParameter
    = BaselineSkip           -- Desired glue between baselines
    | LineSkip               -- Interline glue if \baselineskip isn't feasible
    | ParSkip                -- Extra glue just above paragraphs
    | AboveDisplaySkip       -- Extra glue just above displays
    | AboveDisplayShortSkip  -- Ditto, following short lines
    | BelowDisplaySkip       -- Extra glue just below Displays
    | BelowDisplayShortSkip  -- Ditto, following short lines
    | LeftSkip               -- Glue at left of justified lines
    | RightSkip              -- Glue at right of justified lines
    | TopSkip                -- Glue at top of main pages
    | SplitTopSkip           -- Glue at top of split pages
    | TabSkip                -- Glue between aligned entries
    | SpaceSkip              -- Glue between words, if nonzero
    | XSpaceSkip             -- Glue between sentences, if nonzero
    | ParFillSkip            -- Additional \rightskip at end of paragraphs
    deriving (Show, Eq)

data MathGlueParameter
    = ThinMuSkip             -- Thin space in math formulas
    | MedMuSkip              -- Medium space in math formulas
    | ThickMuSkip            -- Thick space in math formulas
    deriving (Show, Eq)

data TokenListParameter
    = Output                 -- The user's output routine
    | EveryPar               -- Tokens to insert when a paragraph begins
    | EveryMath              -- Tokens to insert when math in text begins
    | EveryDisplay           -- Tokens to insert when display math begins
    | EveryHBox              -- Tokens to insert when an hbox begins
    | EveryVBox              -- Tokens to insert when a vbox begins
    | EveryJob               -- Tokens to insert when the job begins
    | EveryCR                -- Tokens to insert after every \cr or nonredundant \crcr
    | ErrHelp                -- Tokens that supplement an \errmessage
    deriving (Show, Eq)

data SpecialInteger
    = SpaceFactorInteger
    | PrevGrafInteger
    | DeadCyclesInteger
    | InsertPenaltiesInteger
    deriving (Show, Eq)

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
    deriving (Show, Eq)

data AssignPrefixTok
    = LongTok
    | OuterTok
    | GlobalTok
    deriving (Show, Eq)

data IndentFlag
    = Indent
    | DoNotIndent
    deriving (Show, Eq)

data ExpandDefFlag
    = ExpandDef
    | InhibitDef
    deriving (Show, Eq)

data GlobalFlag
    = Global
    | Local
    deriving (Show, Eq)

data MessageStream
    = Out
    | Err
    deriving (Show, Eq)

data BoxFetchMode
    = Pop
    | Lookup
    deriving (Show, Eq)

data PresetGlueType
    = Fil              -- \{h,v}fil
    | Fill             -- \{h,v}fill
    | StretchOrShrink  -- \{h,v}ss
    | FilNeg           -- \{h,v}filneg
    deriving (Show, Eq)

data LeadersType
    = Aligned   -- \leaders
    | Centered  -- \cleaders
    | Expanded  -- \xleaders
    deriving (Show, Eq)

data ModeAttribute
    = IsVertical
    | IsHorizontal
    | IsMath
    | IsInner
    deriving (Show, Eq)

data BoxRegisterAttribute
    = HasVerticalBox
    | HasHorizontalBox
    | IsVoid
    deriving (Show, Eq)

data FontChar
    = HyphenChar  -- \hyphenchar
    | SkewChar  -- \skewchar
    deriving (Show, Eq)

data FontRange
    = TextSizeFontRange -- \textfont
    | ScriptSizeFontRange -- \scriptfont
    | ScriptScriptSizeFontRange -- \scriptscriptfont
    deriving (Show, Eq)

data ModedCommandPrimitiveToken
    = SpecifiedGlueTok -- \vskip, \hskip
    | PresetGlueTok PresetGlueType -- \{v,h}{fil,fill,filneg,ss}
    | AlignedMaterialTok -- \halign, \valign
    | ShiftedBoxTok Direction -- \moveleft, \moveright, \raise, \lower
    | UnwrappedFetchedBoxTok BoxFetchMode -- \un{v,h}{box,copy}
    | RuleTok -- \hrule, \vrule
    deriving (Show, Eq)

data SyntaxCommandArg
    = EndCSNameTok -- \endcsname
    deriving (Show, Eq)

data CodeType
    = CategoryCodeType
    | MathCodeType
    | ChangeCaseCodeType VDirection
    | SpaceFactorCodeType
    | DelimiterCodeType
    deriving (Show, Eq)

data QuantityType
    = CharQuantity  -- \chardef
    | MathCharQuantity  -- \mathchardef
    | RegQuantity RegisterType
    deriving (Show, Eq)

data RegisterType
    = RegInt        -- \count, \countdef
    | RegLen        -- \dimen, \dimendef
    | RegGlue       -- \skip, \skipdef
    | RegMathGlue   -- \muskip, \muskipdef
    | RegTokenList  -- \toks, \toksdef
    deriving (Show, Eq)

data InteractionMode
    = ErrorStopMode  -- \errorstopmode
    | ScrollMode  -- \scrollmode
    | NonStopMode  -- \nonstopmode
    | BatchMode  -- \batchmode
    deriving (Show, Eq)

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
    deriving (Eq, Ord, Bounded, Enum, Show)

digitToChar :: Digit -> Char
digitToChar One   = '1'
digitToChar Two   = '2'
digitToChar Three = '3'
digitToChar Four  = '4'
digitToChar Five  = '5'
digitToChar Six   = '6'
digitToChar Seven = '7'
digitToChar Eight = '8'
digitToChar Nine  = '9'

charToDigit :: Char -> Maybe Digit
charToDigit '1' = Just One
charToDigit '2' = Just Two
charToDigit '3' = Just Three
charToDigit '4' = Just Four
charToDigit '5' = Just Five
charToDigit '6' = Just Six
charToDigit '7' = Just Seven
charToDigit '8' = Just Eight
charToDigit '9' = Just Nine
charToDigit _   = Nothing

-- We use a map to restrict our parameter keys' domain to [1..9].
type MacroParameters = Map.Map Digit [Lex.Token]

-- A token in a macro template.
-- TODO: Technically, we could narrow the domain of a MacroTextLexToken,
-- because we should know that we won't have a 'Parameter'-category token.
data MacroTextToken
    -- A 'normal' token.
    = MacroTextLexToken Lex.Token
    -- A token to be substituted by a macro argument.
    | MacroTextParamToken Digit
    deriving (Eq, Show)

-- A macro template.
newtype MacroText = MacroText [MacroTextToken]
    deriving (Show, Eq)

data MacroContents = MacroContents
    { -- Tokens to expect before the first argument.
      preParamTokens :: [Lex.Token]
    , parameters :: MacroParameters
    , replacementTokens :: MacroText
    , long, outer :: Bool
    } deriving (Show, Eq)

data RemovableItem
    = PenaltyItem  -- \unpenalty
    | KernItem  -- \unkern
    | GlueItem  -- \unskip
    deriving (Show, Eq)

data PrimitiveToken
    = SyntaxCommandArg SyntaxCommandArg
    -- Starters of commands.
    | RelaxTok -- \relax
    | ChangeScopeCSTok Sign
    | ShowTokenTok -- \show
    | ShowBoxTok -- \showbox
    | ShowListsTok -- \showlists
    | ShowInternalQuantityTok -- \showthe
    | ShipOutTok -- \shipout
    | IgnoreSpacesTok -- \ignorespaces
    | SetAfterAssignmentTokenTok -- \afterassignment
    | ToAfterGroupTokensTok -- \aftergroup
    | MessageTok MessageStream -- \message, \errmessage
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
    -- Starters of Vertical-Mode-specific commands.
    | EndTok -- \end
    | DumpTok -- \dump
    -- Starters of Horizontal-Mode-specific commands.
    | ControlSpaceTok -- \␣ (a control symbol named ' ')
    | ControlCharTok -- \char
    | AccentTok -- \accent
    | ItalicCorrectionTok -- \/
    | DiscretionaryTextTok -- \discretionary
    | DiscretionaryHyphenTok -- \-
    | ToggleMathModeTok -- $
    -- > > Modifying how to apply assignments.
    | AssignPrefixTok AssignPrefixTok
    -- > > Modifying how to parse the macro.
    --     \def, \gdef, \edef (expanded-def), \xdef (global-expanded-def).
    | DefineMacroTok GlobalFlag ExpandDefFlag
    -- > Setting variable values.
    | IntParamVarTok IntegerParameter
    | LenParamVarTok LengthParameter
    | GlueParamVarTok GlueParameter
    | MathGlueParamVarTok MathGlueParameter
    | TokenListParamVarTok TokenListParameter
    | SpecialIntegerTok SpecialInteger -- \example: \spacefactor
    | SpecialLengthTok SpecialLength -- \example: \pagestretch
    -- Tokens storing integers defined by short-hand definitions.
    | IntRefTok QuantityType IntVal
    -- A control sequence representing a particular font, such as defined through
    -- \font.
    | FontRefToken IntVal
    -- Heads of register references.
    | RegisterVariableTok RegisterType
    -- Heads of int-ref definitions.
    | ShortDefHeadTok QuantityType
    -- > Modifying variable values with arithmetic.
    | AdvanceVarTok -- \advance
    | ScaleVarTok VDirection -- \multiply, \divide.
    | CodeTypeTok CodeType
    -- > Aliasing tokens.
    | LetTok -- \let
    | FutureLetTok -- \futurelet
    -- > Setting font math-family-member things.
    | FontRangeTok FontRange
    -- > Internal integers.
    | LastPenaltyTok -- \lastpenalty
    | ParagraphShapeTok -- \parshape
    | BadnessTok -- \badness
    | InputLineNrTok -- \inputlineno
    -- Internal lengths.
    | LastKernTok -- \lastkern
    | FontDimensionTok -- \fontdimen
    | BoxDimensionTok TypoDim -- \ht, \wd, \dp
    -- Internal glues.
    | LastGlueTok -- \lastskip
    -- Specifying boxes.
    | FetchedBoxTok BoxFetchMode -- \box, \copy
    | LastBoxTok -- \lastbox
    | SplitVBoxTok -- \vsplit
    | HBoxTok -- \hbox
    | VBoxTok {top :: Bool} -- \vbox, \vtop
    -- > Setting the contents of a box register.
    | SetBoxRegisterTok -- \setbox
    -- > Reading contents into control sequences (not sure what this is about).
    | ReadTok -- \read
    -- > Defining macros resolving to a font.
    | FontTok -- \font
    -- Involved in global assignments.
    -- > Setting properties of a font.
    | FontCharTok FontChar
    -- > Configuring hyphenation.
    | HyphenationTok -- \hyphenation
    | HyphenationPatternsTok -- \patterns
    -- > Setting interaction mode.
    | InteractionModeTok InteractionMode
    -- Conditions.
    -- \| CompareIntegers -- \ifnum
    -- \| CompareDistances -- \ifdim
    -- \| IfIntegerOdd -- \ifodd
    -- \| IfInMode ModeAttribute -- \ifvmode, \ifhmode, \ifmmode, \ifinner
    -- \| IfCharacterCodesEqual -- \if
    -- \| IfCategoryCodesEqual -- \ifcat
    -- \| IfTokensEqual -- \ifx
    -- \| IfBoxRegisterIs BoxRegisterAttribute -- \ifvoid, \ifhbox, \ifvbox
    -- \| IfInputEnded -- \ifeof
    -- \| IfConst Bool -- \iftrue, \iffalse
    -- \| IfCase Bool -- \ifcase
    -- -- Parts of conditions.
    -- \| Else -- \else
    -- \| EndIf -- \fi
    -- \| Or -- \or

    | ResolutionError Lex.ControlSequenceLike
    | SubParserError String

    | UnexpandedTok Lex.Token
    deriving (Show, Eq)

instance Ord PrimitiveToken where
    compare _ _ = EQ

data SyntaxCommandHeadToken
    = ChangeCaseTok VDirection -- \uppercase, \lowercase
    | CSNameTok
    | MacroTok MacroContents
    deriving (Show, Eq)

data ResolvedToken
    = SyntaxCommandHeadToken SyntaxCommandHeadToken
    | PrimitiveToken PrimitiveToken
    deriving (Show, Eq)

instance Ord ResolvedToken where
    compare _ _ = EQ
