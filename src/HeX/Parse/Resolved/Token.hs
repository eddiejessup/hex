module HeX.Parse.Resolved.Token where

import qualified HeX.Lex as Lex
import HeX.Parse.Lexed

data HDirection
  = Leftward
  | Rightward
  deriving (Show, Eq)

data VDirection
  = Upward
  | Downward
  deriving (Show, Eq)

data Direction
  = Forward
  | Backward
  deriving (Show, Eq)

data MessageStream
  = Out
  | Err
  deriving (Show, Eq)

data Axis
  = Horizontal
  | Vertical
  deriving (Show, Eq)

data PresetGlueType
  = Fil -- \{h,v}fil
  | Fill -- \{h,v}fill
  | StretchOrShrink -- \{h,v}ss
  | FilNeg -- \{h,v}filneg
  deriving (Show, Eq)

data LeadersType
  = Aligned -- \leaders
  | Centered -- \cleaders
  | Expanded -- \xleaders
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

data MacroParameter = MacroParameter
  { nr :: Int
  , delimiter :: [Lex.Token]
  } deriving (Show, Eq)

data Macro =
  Macro [MacroParameter]
        BalancedText
  deriving (Show, Eq)

data ModedCommandPrimitiveToken
  = AddSpecifiedGlue -- \vskip, \hskip
  | AddPresetGlue PresetGlueType -- \{v,h}{fil,fill,filneg,ss}
  -- | AddAlignedMaterial -- \halign, \valign
  -- | AddShiftedBox Direction -- \moveleft, \moveright, \raise, \lower
  -- | AddUnwrappedFetchedBox { pop :: Bool } -- \un{v,h}{box,copy}
  | AddRule -- \hrule, \vrule
  deriving (Show, Eq)

data PrimitiveToken
  -- Starters of commands.
  = Relax -- \relax
  -- | RightBrace -- }
  -- | BeginGroup -- \begingroup
  -- | EndGroup -- \endgroup
  -- | ShowToken -- \show
  -- | ShowBox -- \showbox
  -- | ShowLists -- \showlists
  -- | ShowInternalQuantity -- \showthe
  -- | ShipOut -- \shipout
  | IgnoreSpaces -- \ignorespaces
  -- | SetAfterAssignmentToken -- \afterassignment
  -- | AddToAfterGroupTokens -- \aftergroup
  -- | Message MessageStream -- \message, \errmessage
  -- | Immediate -- \immediate
  -- | OpenInput -- \openin
  -- | CloseInput -- \closein
  -- | OpenOutput -- \openout
  -- | CloseOutput -- \closeout
  -- | Write -- \write
  -- | AddWhatsit -- \special
  | AddPenalty -- \penalty
  | AddKern -- \kern
  -- | RemoveLastPenalty -- \unpenalty
  -- | RemoveLastKern -- \unkern
  -- | RemoveLastGlue -- \unskip
  -- | AddMark -- \mark
  -- | AddInsertion -- \insert
  -- | AddLeaders LeadersType
  | StartParagraph { indent :: Bool } -- \indent, \noindent
  | EndParagraph -- \par
  -- | LeftBrace -- {
  -- Starters of mode-specific commands with almost mode-independent grammar.
  | ModedCommand Axis
                 ModedCommandPrimitiveToken
  -- Starters of Vertical-Mode-specific commands.
  | End -- \end
  -- | Dump -- \dump
  -- Starters of Horizontal-Mode-specific commands.
  -- | ControlSpace -- \␣ (a control symbol named ' ')
  -- | AddCharacterCode -- \char
  -- | TokenForCharacter -- a control sequence representing a character, such as defined through \chardef.
  -- | AddAccentedCharacter -- \accent
  -- | AddItalicCorrection -- \/
  -- | AddDiscretionaryText -- \discretionary
  -- | AddDiscretionaryHyphen -- \-
  -- | ToggleMathMode -- $
  -- -- Starters of box invocations.
  -- | PopRegisterBox -- \box
  -- | LookupRegisterBox -- \copy
  -- | LastBox -- \lastbox
  -- -- VBox from splitting off some material from a box register.
  -- | SplitVBox -- \vsplit
  -- | HBox -- \hbox
  -- | VBox {top :: Bool} -- \vbox, \vtop
  -- Direction represents reading direction: forward means right, or down.
  -- | AddFetchedBox { pop :: Bool } -- \box, \copy
  -- -- Involved in assignments.
  -- -- * * Modifying how to apply assignments.
  | Global -- \global
  -- -- * Defining macros.
  -- -- * * Modifying how to parse the macro.
  | Long -- \long
  | Outer -- \outer
  --     \def, \gdef, \edef (expanded-def), \xdef (global-expanded-def).
  | DefineMacro { global, expand :: Bool }
  -- -- * * Modifying how to parse the macro.
  -- -- * Setting variable values.
  -- -- * * Setting an integer value.
  -- -- |      IntegerParameter IntegerParameter -- example: \tolerance
  --        -- a control sequence representing the contents of an integer
  --        -- register, such as defined through \countdef.
  -- |      TokenForInteger
  -- |      LookupInteger -- \count
  -- -- * * Setting a distance (also known as a 'dimen') value.
  -- -- |      DistanceParameter DistanceParameter -- example: \hsize
  --        -- a control sequence representing the contents of a distance
  --        -- register, such as defined through \dimendef.
  -- |      TokenForDistance
  -- |      LookupDistance -- \dimen
  -- -- * * Setting a glue (also known as a 'skip') value.
  -- -- |      GlueParameter GlueParameter -- example: \lineskip
  --        -- a control sequence representing the contents of a glue
  --        -- register, such as defined through \skipdef.
  -- |      TokenForGlue
  -- |      LookupGlue -- \skip
  -- -- * * Setting a math-glue (also known as a 'muskip' or 'muglue') value.
  -- -- |      MathGlueParameter MathGlueParameter -- example: \thinmuskip
  --        -- a control sequence representing the contents of a math-glue
  --        -- register, such as defined through \muskipdef.
  -- |      TokenForMathGlue
  -- |      LookupMathGlue -- \muskip
  -- -- * * Setting a token-list value.
  -- -- |      TokenListParameter TokenListParameter -- example: \everypar
  --        -- a control sequence representing the contents of a token-list
  --        -- register, such as defined through \toksdef.
  -- |      TokenForTokenList
  -- |      LookupTokenList -- \toks
  -- * Modifying variable values with arithmetic.
  -- | Advance -- \advance
  -- | Multiply -- \multiply
  -- | Divide -- \divide
  -- * Setting code table values.
  -- | SetCodeCategory -- \catcode
  -- | SetCodeMathCategory -- \mathcode
  -- | SetCodeChangeCase VDirection -- \uccode, \lccode
  -- | SetCodeSpaceFactor -- \sfcode
  -- | SetCodeDelimiter -- \delcode
  -- * Aliasing tokens.
  -- | Let -- \let
  -- | FutureLet -- \futurelet
  -- * Defining simple token macros (also known as 'short-hand definitions').
  -- * * Macros resolving to character and math-character codes.
  -- | MacroToCharacter -- \chardef
  -- | MacroToMathCharacter -- \mathchardef
  -- * * Macros resolving to the contents of locations in registers.
  -- | MacroToIntegerLookup -- \countdef
  -- | MacroToDistanceLookup -- \dimendef
  -- | MacroToGlueLookup -- \skipdef
  -- | MacroToMathGlueLookup -- \muskipdef
  -- | MacroToTokenListLookup -- \toksdef
  -- * Setting the current font.
  -- A control sequence representing a particular font, such as defined through
  -- \font.
  | TokenForFont Int
  -- * Setting font math-family-member things.
  -- | SetTextSizeFont -- \textfont
  -- | SetScriptSizeFont -- \scriptfont
  -- | SetScriptScriptSizeFont -- \scriptscriptfont
  -- * Setting paragraph shape things.
  -- | SetParagraphShape -- \parshape
  -- * Reading contents into control sequences (not sure what this is about).
  -- | Read -- \read
  -- * Setting the contents of a box register.
  -- | SetBoxRegister -- \setbox
  -- * Defining macros resolving to a font.
  | MacroToFont -- \font
  -- Involved in global assignments.
  -- * Setting properties of a font.
  -- | SetFontDimension -- \fontdimen
  -- | SetFontHyphenCharacter -- \hyphenchar
  -- | SetFontSkewCharacter -- \skewchar
  -- * Configuring hyphenation.
  -- | AddHyphenationExceptions -- \hyphenation
  -- | SetHyphenationPatterns -- \patterns
  -- * Dealing with box dimensions.
  -- | BoxHeight -- \ht
  -- | BoxWidth -- \wd
  -- | BoxDepth -- \dp
  -- * Setting interaction mode.
  -- | SwitchToErrorStopMode -- \errorstopmode
  -- | SwitchToScrollMode -- \scrollmode
  -- | SwitchToNonStopMode -- \nonstopmode
  -- | SwitchToBatchMode -- \batchmode
  -- * Setting special values.
  -- | SpecialInteger SpecialInteger -- \example: \spacefactor
  -- | SpecialDistance SpecialDistance -- \example: \pagestretch
  -- Conditions.
  -- | CompareIntegers -- \ifnum
  -- | CompareDistances -- \ifdim
  -- | IfIntegerOdd -- \ifodd
  -- | IfInMode ModeAttribute -- \ifvmode, \ifhmode, \ifmmode, \ifinner
  -- | IfCharacterCodesEqual -- \if
  -- | IfCategoryCodesEqual -- \ifcat
  -- | IfTokensEqual -- \ifx
  -- | IfBoxRegisterIs BoxRegisterAttribute -- \ifvoid, \ifhbox, \ifvbox
  -- | IfInputEnded -- \ifeof
  -- | IfConst Bool -- \iftrue, \iffalse
  -- | IfCase Bool -- \ifcase
  -- -- Parts of conditions.
  -- | Else -- \else
  -- | EndIf -- \fi
  -- | Or -- \or
  | CharCat Lex.CharCat
  deriving (Show, Eq)

instance Ord PrimitiveToken where
  compare _ _ = EQ


data ExpandableToken
  = ChangeCaseToken VDirection -- \uppercase, \lowercase
  | MacroToken Macro
  deriving (Show, Eq)

instance Ord ExpandableToken where
  compare _ _ = EQ

instance Ord ResolvedToken where
  compare _ _ = EQ

data ResolvedToken
  = ExpandableToken ExpandableToken
  | PrimitiveToken PrimitiveToken
  deriving (Show, Eq)
