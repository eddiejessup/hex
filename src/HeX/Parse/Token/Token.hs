module HeX.Parse.Token.Token where

import qualified Data.Map.Strict               as Map

import           HeX.Concept
import qualified HeX.Lex                       as Lex
import           HeX.Parse.Token.Parameter


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

data ModedCommandPrimitiveToken
    = AddSpecifiedGlueTok -- \vskip, \hskip
    | AddPresetGlueTok PresetGlueType -- \{v,h}{fil,fill,filneg,ss}
  -- \| AddAlignedMaterial -- \halign, \valign
  -- \| AddShiftedBox Direction -- \moveleft, \moveright, \raise, \lower
  -- \| AddUnwrappedFetchedBox { pop :: Bool } -- \un{v,h}{box,copy}
    | AddRuleTok -- \hrule, \vrule
    deriving (Show, Eq)

data SyntaxCommandArg
    = EndCSNameTok
    deriving (Show, Eq)

data CodeType
    = CategoryCode
    | MathCode
    | ChangeCaseCode VDirection
    | SpaceFactorCode
    | DelimiterCode
    deriving (Show, Eq)

data DefTokenType
    = DefInt
    | DefLen
    | DefGlue
    | DefMathGlue
    | DefTokenList
    -- A control sequence representing a character, such as defined through
    -- \chardef.
    | DefChar
    | DefMathChar
    | DefFont
    deriving (Show, Eq)

data RegisterType
    = RegInt        -- \count
    | RegLen        -- \dimen
    | RegGlue       -- \skip
    | RegMathGlue   -- \mskip
    | RegTokenList  -- \toks
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
    } deriving (Show, Eq)

data PrimitiveToken
    = SyntaxCommandArg SyntaxCommandArg
    -- Starters of commands.
    | RelaxTok -- \relax
    -- \| RightBrace -- }
    -- \| BeginGroup -- \begingroup
    -- \| EndGroup -- \endgroup
    -- \| ShowToken -- \show
    -- \| ShowBox -- \showbox
    -- \| ShowLists -- \showlists
    -- \| ShowInternalQuantity -- \showthe
    -- \| ShipOut -- \shipout
    | IgnoreSpacesTok -- \ignorespaces
    -- \| SetAfterAssignmentToken -- \afterassignment
    -- \| AddToAfterGroupTokens -- \aftergroup
    -- \| Message MessageStream -- \message, \errmessage
    -- \| Immediate -- \immediate
    -- \| OpenInput -- \openin
    -- \| CloseInput -- \closein
    -- \| OpenOutput -- \openout
    -- \| CloseOutput -- \closeout
    -- \| Write -- \write
    -- \| AddWhatsit -- \special
    | AddPenaltyTok -- \penalty
    | AddKernTok -- \kern
    -- \| RemoveLastPenalty -- \unpenalty
    -- \| RemoveLastKern -- \unkern
    -- \| RemoveLastGlue -- \unskip
    -- \| AddMark -- \mark
    -- \| AddInsertion -- \insert
    -- \| AddLeaders LeadersType
    | StartParagraphTok IndentFlag -- \indent, \noindent
    | EndParagraphTok -- \par
    -- \| LeftBrace -- {
    -- Starters of mode-specific commands with almost mode-independent grammar.
    | ModedCommand Axis ModedCommandPrimitiveToken
    -- Starters of Vertical-Mode-specific commands.
    | EndTok -- \end
    -- \| Dump -- \dump
    -- Starters of Horizontal-Mode-specific commands.
    -- \| ControlSpace -- \␣ (a control symbol named ' ')
    -- \| AddCharacterCode -- \char
    -- \| AddAccentedCharacter -- \accent
    -- \| AddItalicCorrection -- \/
    -- \| AddDiscretionaryText -- \discretionary
    -- \| AddDiscretionaryHyphen -- \-
    -- \| ToggleMathMode -- $
    -- -- Starters of box invocations.
    -- \| PopRegisterBox -- \box
    -- \| LookupRegisterBox -- \copy
    -- \| LastBox -- \lastbox
    -- -- VBox from splitting off some material from a box register.
    -- \| SplitVBox -- \vsplit
    -- \| HBox -- \hbox
    -- \| VBox {top :: Bool} -- \vbox, \vtop
    -- Direction represents reading direction: forward means right, or down.
    -- \| AddFetchedBox { pop :: Bool } -- \box, \copy
    -- -- Involved in assignments.
    -- -- > > Modifying how to apply assignments.
    | AssignPrefixTok AssignPrefixTok
    -- -- > Defining macros.
    -- -- > > Modifying how to parse the macro.
    --     \def, \gdef, \edef (expanded-def), \xdef (global-expanded-def).
    -- -- > > Modifying how to parse the macro.
    | DefineMacroTok GlobalFlag ExpandDefFlag
    -- -- > Setting variable values.
    | IntParamVarTok IntegerParameter
    | LenParamVarTok LengthParameter
    | GlueParamVarTok GlueParameter
    | MathGlueParamVarTok MathGlueParameter
    | TokenListParamVarTok TokenListParameter
    | SpecialIntegerTok SpecialInteger -- \example: \spacefactor
    | SpecialLengthTok SpecialLength -- \example: \pagestretch
    | TokenVariableTok DefTokenType String
    | RegisterVariableTok RegisterType
    -- > Modifying variable values with arithmetic.
    -- \| Advance -- \advance
    -- \| Multiply -- \multiply
    -- \| Divide -- \divide
    | CodeTypeTok CodeType
    -- > Aliasing tokens.
    -- \| Let -- \let
    -- \| FutureLet -- \futurelet
    -- > Defining simple token macros (also known as 'short-hand definitions').
    -- > > Macros resolving to character and math-character codes.
    -- \| MacroToCharacter -- \chardef
    -- \| MacroToMathCharacter -- \mathchardef
    -- > > Macros resolving to the contents of locations in registers.
    -- \| MacroToIntegerLookup -- \countdef
    -- \| MacroToDistanceLookup -- \dimendef
    -- \| MacroToGlueLookup -- \skipdef
    -- \| MacroToMathGlueLookup -- \muskipdef
    -- \| MacroToTokenListLookup -- \toksdef
    -- > Setting the current font.
    -- A control sequence representing a particular font, such as defined through
    -- \font.
    | TokenForFont Int
    -- > Setting font math-family-member things.
    -- \| SetTextSizeFont -- \textfont
    -- \| SetScriptSizeFont -- \scriptfont
    -- \| SetScriptScriptSizeFont -- \scriptscriptfont
    -- > Setting paragraph shape things.
    -- \| SetParagraphShape -- \parshape
    -- > Reading contents into control sequences (not sure what this is about).
    -- \| Read -- \read
    -- > Setting the contents of a box register.
    -- \| SetBoxRegister -- \setbox
    -- > Defining macros resolving to a font.
    | FontTok -- \font
    -- Involved in global assignments.
    -- > Setting properties of a font.
    -- \| SetFontDimension -- \fontdimen
    -- \| SetFontHyphenCharacter -- \hyphenchar
    -- \| SetFontSkewCharacter -- \skewchar
    -- > Configuring hyphenation.
    -- \| AddHyphenationExceptions -- \hyphenation
    -- \| SetHyphenationPatterns -- \patterns
    -- > Dealing with box dimensions.
    -- \| BoxHeight -- \ht
    -- \| BoxWidth -- \wd
    -- \| BoxDepth -- \dp
    -- > Setting interaction mode.
    -- \| SwitchToErrorStopMode -- \errorstopmode
    -- \| SwitchToScrollMode -- \scrollmode
    -- \| SwitchToNonStopMode -- \nonstopmode
    -- \| SwitchToBatchMode -- \batchmode
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

    | ResolutionError
    | SubParserError String

    | UnexpandedTok Lex.Token
    deriving (Show, Eq)

instance Ord PrimitiveToken where
    compare _ _ = EQ

data SyntaxCommandHead
    = ChangeCaseTok VDirection -- \uppercase, \lowercase
    | CSNameTok
    | MacroTok MacroContents
    deriving (Show, Eq)

data ResolvedToken
    = SyntaxCommandHead SyntaxCommandHead
    | PrimitiveToken PrimitiveToken
    deriving (Show, Eq)

instance Ord ResolvedToken where
    compare _ _ = EQ
