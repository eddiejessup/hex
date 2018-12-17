module HeX.Parse.Resolved.Parameter where

data IntegerParameter
  = PreTolerance  -- Badness tolerance before hyphenation
  | Tolerance  -- Badness tolerance after hyphenation
  | HBadness  -- Badness above which bad hboxes will be shown
  | VBadness  -- Badness above which bad vboxes will be shown
  | LinePenalty  -- Amount added to badness of every line in a paragraph
  | HyphenPenalty  -- Penalty for line break after discretionary hyphen
  | ExHyphenPenalty  -- Penalty for line break after explicit hyphen
  | BinOpPenalty  -- Penalty for line break after binary operation
  | RelPenalty  -- Penalty for line break after math relation
  | ClubPenalty  -- Penalty for creating a club line at bottom of page
  | WidowPenalty  -- Penalty for creating a widow line at top of page
  | DisplayWidowPenalty  -- Ditto, before a display
  | BrokenPenalty  -- Penalty for page break after a hyphenated line
  | PreDisplayPenalty  -- Penalty for page break just before a display
  | PostDisplayPenalty  -- Penalty for page break just after a display
  | InterlinePenalty  -- Additional penalty for page break between lines
  | FloatingPenalty  -- Penalty for insertions that are split
  | OutputPenalty  -- Penalty at the current page break
  | DoubleHyphenDemerits  -- Demerits for consecutive broken lines
  | FinalHyphenDemerits  -- Demerits for a penultimate broken line
  | AdjDemerits  -- Demerits for adjacent incompatible lines
  | Looseness  -- Change to the number of lines in a paragraph
  | Pausing  -- Positive if pausing after each line is read from a file
  | HoldingInserts  -- Positive if insertions remain dormant in output box
  | TracingOnline  -- Positive if showing diagnostic info on the terminal
  | TracingMacros  -- Positive if showing macros as they are expanded
  | TracingStats  -- Positive if showing statistics about memory usage
  | TracingParagraphs  -- Positive if showing line-break calculations
  | TracingPages  -- Positive if showing page-break calculations
  | TracingOutput  -- Positive if showing boxes that are shipped out
  | TracingLostChars  -- Positive if showing characters not in the font
  | TracingCommands  -- Positive if showing commands before they are executed
  | TracingRestores  -- Positive if showing deassignments when groups end
  | Language  -- The current set of hyphenation rules
  | UCHyph  -- Positive if hyphenating words beginning with capital letters
  | LeftHyphenMin  -- Smallest fragment at beginning of hyphenated word
  | RightHyphenMin  -- Smallest fragment at end of hyphenated word
  | GlobalDefs  -- Nonzero if overriding \global specifications
  | DefaultHyphenChar  -- \hyphenchar value when a font is loaded
  | DefaultSkewChar  -- \skewchar value when a font is loaded
  | EscapeChar  -- Escape character in the output of control-sequence tokens
  | EndLineChar  -- Character placed at the right end of an input line
  | NewLineChar  -- Character that starts a new output line
  | MaxDeadCycles  -- Upper bound on \deadcycles
  | HangAfter  -- Hanging indentation changes after this many lines
  | Fam  -- The current family number
  | Mag  -- Magnification ratio, times 1000
  | DelimiterFactor  -- Ratio for variable delimiters, times 1000
  | Time  -- Current time of day in minutes since midnight
  | Day  -- Current day of the month
  | Month  -- Current month of the year
  | Year  -- Current year of our Lord
  | ShowBoxBreadth  -- Maximum items per level when boxes are shown
  | ShowBoxDepth  -- Maximum level when boxes are shown
  | ErrorContextLines  -- Maximum extra context shown when errors occur
  deriving (Show, Eq)

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
  deriving (Show, Eq)

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
  deriving (Show, Eq)

data MathGlueParameter
  = ThinMuSkip -- Thin space in math formulas
  | MedMuSkip -- Medium space in math formulas
  | ThickMuSkip -- Thick space in math formulas
  deriving (Show, Eq)

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
  deriving (Show, Eq)

data SpecialInteger
  = SpaceFactor
  | PrevGraf
  | DeadCycles
  | InsertPenalties
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
