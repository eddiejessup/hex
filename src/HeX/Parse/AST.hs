module HeX.Parse.AST where

import           HeXlude

import           Path            ( File, Path, Rel )

import           HeX.Categorise  ( CharCode )
import qualified HeX.Lex         as Lex
import qualified HeX.Parse.Token as T
import           HeX.Unit        ( PhysicalUnit(..) )

-- TeXInt.

data TeXInt = TeXInt T.Sign UnsignedTeXInt
    deriving ( Show )

newtype EightBitTeXInt = EightBitTeXInt TeXInt
    deriving ( Show )

constTeXInt :: TeXIntVal -> TeXInt
constTeXInt n = TeXInt (T.Sign True) $ constUTeXInt n

data UnsignedTeXInt =
    NormalTeXIntAsUTeXInt NormalTeXInt | CoercedTeXInt CoercedTeXInt
    deriving ( Show )

constUTeXInt :: TeXIntVal -> UnsignedTeXInt
constUTeXInt n = NormalTeXIntAsUTeXInt $ TeXIntConstant n

-- Think: 'un-coerced integer'.
data NormalTeXInt = TeXIntConstant Int | InternalTeXInt InternalTeXInt
    deriving ( Show )

zeroTeXInt :: NormalTeXInt
zeroTeXInt = TeXIntConstant 0

oneTeXInt :: NormalTeXInt
oneTeXInt = TeXIntConstant 1

data CoercedTeXInt =
    InternalLengthAsInt InternalLength | InternalGlueAsInt InternalGlue
    deriving ( Show )

-- Length.
data Length = Length T.Sign UnsignedLength
    deriving ( Show )

zeroLength :: Length
zeroLength = Length (T.Sign True) $
    NormalLengthAsULength $
    LengthSemiConstant zeroFactor scaledPointUnit

data UnsignedLength =
    NormalLengthAsULength NormalLength | CoercedLength CoercedLength
    deriving ( Show )

-- Think: 'un-coerced length'.
data NormalLength =
      -- 'semi-constant' because Factor and Unit can be quite un-constant-like.
      LengthSemiConstant Factor Unit
    | InternalLength InternalLength
    deriving ( Show )

data Factor =
      NormalTeXIntFactor NormalTeXInt
      -- Badly named 'decimal constant' in the TeXbook. Granted, it is specified
      -- with decimal digits, but its main feature is that it can represent
      -- non-integers.
    | RationalConstant Rational
    deriving ( Show )

zeroFactor, oneFactor :: Factor
zeroFactor = NormalTeXIntFactor zeroTeXInt

oneFactor = NormalTeXIntFactor oneTeXInt

data Unit =
    PhysicalUnit PhysicalUnitFrame PhysicalUnit | InternalUnit InternalUnit
    deriving ( Show )

scaledPointUnit :: Unit
scaledPointUnit = PhysicalUnit MagnifiedFrame ScaledPoint

data InternalUnit =
      Em
    | Ex
    | InternalTeXIntUnit InternalTeXInt
    | InternalLengthUnit InternalLength
    | InternalGlueUnit InternalGlue
    deriving ( Show )

data PhysicalUnitFrame = MagnifiedFrame | TrueFrame
    deriving ( Show )

data CoercedLength = InternalGlueAsLength InternalGlue
    deriving ( Show )

-- Math-length.
data MathLength = MathLength T.Sign UnsignedMathLength
    deriving ( Show )

data UnsignedMathLength = NormalMathLengthAsUMathLength NormalMathLength
                        | CoercedMathLength CoercedMathLength
    deriving ( Show )

-- Think: 'un-coerced length'.
data NormalMathLength =
    -- 'semi-constant' because Factor and Unit can be quite un-constant-like.
    MathLengthSemiConstant Factor MathUnit
    deriving ( Show )

data MathUnit = Mu | InternalMathGlueAsUnit InternalMathGlue
    deriving ( Show )

data CoercedMathLength = InternalMathGlueAsMathLength InternalMathGlue
    deriving ( Show )

-- Glue.
data Glue = ExplicitGlue Length (Maybe Flex) (Maybe Flex)
          | InternalGlue T.Sign InternalGlue
    deriving ( Show )

data Flex = FiniteFlex Length | FilFlex FilLength
    deriving ( Show )

oneFilFlex, minusOneFilFlex, oneFillFlex :: Flex
oneFilFlex = FilFlex oneFil

minusOneFilFlex = FilFlex minusOneFil

oneFillFlex = FilFlex oneFill

data FilLength = FilLength T.Sign Factor Int
    deriving ( Show )

oneFil, minusOneFil, oneFill :: FilLength
oneFil = FilLength (T.Sign True) oneFactor 1

minusOneFil = FilLength (T.Sign False) oneFactor 1

oneFill = FilLength (T.Sign True) oneFactor 2

-- Math glue.
data MathGlue = ExplicitMathGlue MathLength (Maybe MathFlex) (Maybe MathFlex)
              | InternalMathGlue T.Sign InternalMathGlue
    deriving ( Show )

data MathFlex = FiniteMathFlex MathLength | FilMathFlex FilLength
    deriving ( Show )

-- Internal quantities.
data QuantVariable a = ParamVar a | RegisterVar EightBitTeXInt
    deriving ( Show )

type TeXIntVariable = QuantVariable T.TeXIntParameter

type LengthVariable = QuantVariable T.LengthParameter

type GlueVariable = QuantVariable T.GlueParameter

type MathGlueVariable = QuantVariable T.MathGlueParameter

type TokenListVariable = QuantVariable T.TokenListParameter

data InternalTeXInt =
      InternalTeXIntVariable TeXIntVariable
    | InternalSpecialTeXInt T.SpecialTeXInt
    | InternalCodeTableRef CodeTableRef
    | InternalCharToken TeXIntVal
    | InternalMathCharToken TeXIntVal
    | InternalFontCharRef FontCharRef
    | LastPenalty
    | ParShape
    | InputLineNr
    | Badness
    deriving ( Show )

data CodeTableRef = CodeTableRef T.CodeType TeXInt
    deriving ( Show )

data FontCharRef = FontCharRef T.FontChar FontRef
    deriving ( Show )

data FontRef =
    FontTokenRef Int | CurrentFontRef | FamilyMemberFontRef FamilyMember
    deriving ( Show )

data FamilyMember = FamilyMember T.FontRange TeXInt
    deriving ( Show )

data BoxDimensionRef = BoxDimensionRef TeXInt BoxDim
    deriving ( Show )

data FontDimensionRef = FontDimensionRef TeXInt FontRef
    deriving ( Show )

data InternalLength =
      InternalLengthVariable LengthVariable
    | InternalSpecialLength T.SpecialLength
    | InternalFontDimensionRef FontDimensionRef
    | InternalBoxDimensionRef BoxDimensionRef
    | LastKern
    deriving ( Show )

data InternalGlue = InternalGlueVariable GlueVariable | LastGlue
    deriving ( Show )

data InternalMathGlue =
    InternalMathGlueVariable MathGlueVariable | LastMathGlue
    deriving ( Show )

-- Assignments.
data Assignment = Assignment { body :: AssignmentBody, global :: T.GlobalFlag }
    deriving ( Show )

data ControlSequenceTarget =
      MacroTarget T.MacroContents
    | LetTarget Lex.Token
    | FutureLetTarget Lex.Token Lex.Token
    | ShortDefineTarget T.QuantityType TeXInt
    | ReadTarget TeXInt
    | FontTarget FontSpecification (Path Rel File)
    deriving ( Show )

data AssignmentBody =
      DefineControlSequence Lex.ControlSequenceLike ControlSequenceTarget
    | SetVariable VariableAssignment
    | ModifyVariable VariableModification
    | AssignCode CodeAssignment
    | SelectFont TeXIntVal
    | SetFamilyMember FamilyMember FontRef
    | SetParShape [(Length, Length)]
    | SetBoxRegister EightBitTeXInt Box
      -- -- Global assignments.
    | SetFontDimension FontDimensionRef Length
    | SetFontChar FontCharRef TeXInt
    | SetHyphenation T.BalancedText
    | SetHyphenationPatterns T.BalancedText
    | SetBoxDimension BoxDimensionRef Length
    | SetInteractionMode T.InteractionMode
    deriving ( Show )

data TokenListAssignmentTarget = TokenListAssignmentVar TokenListVariable
                               | TokenListAssignmentText T.BalancedText
    deriving ( Show )

data VariableAssignment =
      TeXIntVariableAssignment TeXIntVariable TeXInt
    | LengthVariableAssignment LengthVariable Length
    | GlueVariableAssignment GlueVariable Glue
    | MathGlueVariableAssignment MathGlueVariable MathGlue
    | TokenListVariableAssignment TokenListVariable TokenListAssignmentTarget
    | SpecialTeXIntVariableAssignment T.SpecialTeXInt TeXInt
    | SpecialLengthVariableAssignment T.SpecialLength Length
    deriving ( Show )

data VariableModification =
      AdvanceTeXIntVariable TeXIntVariable TeXInt
    | AdvanceLengthVariable LengthVariable Length
    | AdvanceGlueVariable GlueVariable Glue
    | AdvanceMathGlueVariable MathGlueVariable MathGlue
    | ScaleVariable VDirection NumericVariable TeXInt
    deriving ( Show )

data NumericVariable = TeXIntNumericVariable TeXIntVariable
                     | LengthNumericVariable LengthVariable
                     | GlueNumericVariable GlueVariable
                     | MathGlueNumericVariable MathGlueVariable
    deriving ( Show )

data CodeAssignment = CodeAssignment CodeTableRef TeXInt
    deriving ( Show )

data FontSpecification = NaturalFont | FontAt Length | FontScaled TeXInt
    deriving ( Show )

-- Box specification.
data Box = FetchedRegisterBox T.BoxFetchMode EightBitTeXInt
         | LastBox
         | VSplitBox TeXInt Length
         | ExplicitBox BoxSpecification T.ExplicitBox
    deriving ( Show )

data BoxSpecification = Natural | To Length | Spread Length
    deriving ( Show )

data BoxOrRule = BoxOrRuleBox Box | BoxOrRuleRule Axis Rule
    deriving ( Show )

-- Commands.

data ModeIndependentCommand
    = Assign Assignment
    | Relax
    | IgnoreSpaces
    | AddPenalty TeXInt
    | AddKern Length
    | AddMathKern MathLength
    | RemoveItem T.RemovableItem
    | SetAfterAssignmentToken Lex.Token
    | AddToAfterGroupTokens Lex.Token
    | Message T.StandardOutputStream T.ExpandedBalancedText
    | ModifyFileStream FileStreamType FileStreamAction TeXInt
    | WriteToStream TeXInt WriteText
    | DoSpecial T.ExpandedBalancedText
    | AddBox BoxPlacement Box
    | ChangeScope T.Sign CommandTrigger
    deriving ( Show )

data Command
    = ShowToken Lex.Token
    | ShowBox TeXInt
    | ShowLists
    | ShowTheInternalQuantity InternalQuantity
    | ShipOut Box
    | AddMark T.BalancedText
      -- -- Note: this *is* an all-modes command. It can happen in non-vertical modes,
      -- -- then can 'migrate' out.
      -- \| AddInsertion TeXInt VModeMaterial
      -- \| AddAdjustment VModeMaterial
    | AddSpace
    | StartParagraph T.IndentFlag
    | EndParagraph
      -- \| AddAlignedMaterial DesiredLength AlignmentMaterial
    | HModeCommand HModeCommand
    | VModeCommand VModeCommand
    | ModeIndependentCommand ModeIndependentCommand
    deriving ( Show )

data VModeCommand
    = End
    | Dump
    | EnterHMode
    | AddVGlue Glue
    | AddVLeaders LeadersSpec
    | AddVRule Rule
    | AddUnwrappedFetchedVBox FetchedBoxRef -- \unv{box,copy}
    deriving ( Show )

data HModeCommand =
      AddControlSpace
    | AddCharacter CharCodeRef
    | AddAccentedCharacter TeXInt [Assignment] (Maybe CharCodeRef)
    | AddItalicCorrection
    | AddDiscretionaryText { preBreak, postBreak, noBreak :: T.BalancedText }
    | AddDiscretionaryHyphen
    | EnterMathMode
    | AddHGlue Glue
    | AddHLeaders LeadersSpec
    | AddHRule Rule
    | AddUnwrappedFetchedHBox FetchedBoxRef -- \unh{box,copy}
    deriving ( Show )

data FetchedBoxRef = FetchedBoxRef TeXInt T.BoxFetchMode
    deriving ( Show )

data LeadersSpec = LeadersSpec T.LeadersType BoxOrRule Glue
    deriving ( Show )

data CommandTrigger = CharCommandTrigger | CSCommandTrigger
    deriving ( Show, Eq )

data InternalQuantity =
      InternalTeXIntQuantity InternalTeXInt
    | InternalLengthQuantity InternalLength
    | InternalGlueQuantity InternalGlue
    | InternalMathGlueQuantity InternalMathGlue
    | FontQuantity FontRef
    | TokenListVariableQuantity TokenListVariable
    deriving ( Show )

data WriteText = ImmediateWriteText T.ExpandedBalancedText
               | DeferredWriteText T.BalancedText
    deriving ( Show )

data WritePolicy = Immediate | Deferred
    deriving ( Show )

data Rule = Rule { width, height, depth :: Maybe Length }
    deriving ( Show )

data FileStreamAction = Open (Path Rel File) | Close
    deriving ( Show )

data FileStreamType = FileInput | FileOutput WritePolicy
    deriving ( Show )

data BoxPlacement = NaturalPlacement | ShiftedPlacement Axis Direction Length
    deriving ( Show )

data CharCodeRef =
    CharRef CharCode | CharTokenRef TeXIntVal | CharCodeNrRef TeXInt
    deriving ( Show )

-- Condition heads.
data IfConditionHead =
      IfTeXIntPairTest TeXInt Ordering TeXInt -- \ifnum
    | IfLengthPairTest Length Ordering Length -- \ifdim
    | IfTeXIntOdd TeXInt -- \ifodd
    | IfInMode T.ModeAttribute -- \ifvmode, \ifhmode, \ifmmode, \ifinner
    | IfTokenAttributesEqual T.TokenAttribute T.PrimitiveToken T.PrimitiveToken -- \if, \ifcat
    | IfTokensEqual Lex.Token Lex.Token -- \ifx
    | IfBoxRegisterIs T.BoxRegisterAttribute TeXInt -- \ifvoid, \ifhbox, \ifvbox
    | IfInputEnded TeXInt -- \ifeof
    | IfConst Bool -- \iftrue, \iffalse
    deriving ( Show )

data ConditionHead = IfConditionHead IfConditionHead | CaseConditionHead TeXInt
    deriving ( Show )
