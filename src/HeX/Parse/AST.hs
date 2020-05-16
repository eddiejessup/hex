module Hex.Parse.AST where

import qualified Hex.Config.Codes as Code
import qualified Hex.Lex as Lex
import qualified Hex.Quantity as Q
import qualified Hex.Resolve.Token as T
import Hexlude
import qualified Path

-- TeXInt.

type TeXInt = T.Signed UnsignedTeXInt

newtype EightBitTeXInt = EightBitTeXInt TeXInt
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

constTeXInt :: Q.TeXInt -> TeXInt
constTeXInt n = T.Signed T.Positive $ constUTeXInt n

data UnsignedTeXInt
  = NormalTeXIntAsUTeXInt NormalTeXInt
  | CoercedTeXInt CoercedTeXInt
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

constUTeXInt :: Q.TeXInt -> UnsignedTeXInt
constUTeXInt n = NormalTeXIntAsUTeXInt $ TeXIntConstant n

-- Think: 'un-coerced integer'.
data NormalTeXInt = TeXIntConstant Q.TeXInt | InternalTeXInt InternalTeXInt
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

zeroTeXInt :: NormalTeXInt
zeroTeXInt = TeXIntConstant 0

oneTeXInt :: NormalTeXInt
oneTeXInt = TeXIntConstant 1

data CoercedTeXInt
  = InternalLengthAsInt InternalLength
  | InternalGlueAsInt InternalGlue
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

-- Length.
type Length = T.Signed UnsignedLength

zeroLength :: Length
zeroLength =
  T.Signed T.Positive
    $ NormalLengthAsULength
    $ LengthSemiConstant zeroFactor scaledPointUnit

data UnsignedLength
  = NormalLengthAsULength NormalLength
  | CoercedLength CoercedLength
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Describe UnsignedLength where
  describe = \case
    NormalLengthAsULength v ->
      describePrepended 0 "UnsignedLength/NormalLengthAsULength" v
    CoercedLength v ->
      describePrepended 0 "UnsignedLength/CoercedLength" v

-- Think: 'un-coerced length'.
data NormalLength
  = -- 'semi-constant' because Factor and Unit can be quite un-constant-like.
    LengthSemiConstant Factor Unit
  | InternalLength InternalLength
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Describe NormalLength where
  describe = \case
    LengthSemiConstant f u ->
      [ (0, "NormalLength/LengthSemiConstant"),
        (1, "Factor " <> quote (show f))
      ]
        <> describeRel 1 u
    InternalLength il ->
      describePrepended 0 "NormalLength/InternalLength" il

data Factor
  = NormalTeXIntFactor NormalTeXInt
  | -- Badly named 'decimal constant' in the TeXbook. Granted, it is specified
    -- with decimal digits, but its main feature is that it can represent
    -- non-integers.
    RationalConstant Rational
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

zeroFactor, oneFactor :: Factor
zeroFactor = NormalTeXIntFactor zeroTeXInt
oneFactor = NormalTeXIntFactor oneTeXInt

data Unit
  = PhysicalUnit PhysicalUnitFrame Q.PhysicalUnit
  | InternalUnit InternalUnit
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Describe Unit where
  describe = \case
    PhysicalUnit MagnifiedFrame pu ->
      describePrepended 0 "Unit/Physical MagnifiedFrame" pu
    PhysicalUnit TrueFrame pu ->
      describePrepended 0 "Unit/Physical TrueFrame" pu
    InternalUnit iu ->
      describePrepended 0 "Unit/Internal" iu

scaledPointUnit :: Unit
scaledPointUnit = PhysicalUnit MagnifiedFrame Q.ScaledPoint

data InternalUnit
  = Em
  | Ex
  | InternalTeXIntUnit InternalTeXInt
  | InternalLengthUnit InternalLength
  | InternalGlueUnit InternalGlue
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Describe InternalUnit where
  describe = \case
    Em -> singleLine "InternalUnit/em"
    Ex -> singleLine "InternalUnit/ex"
    v -> singleLine $ show v

data PhysicalUnitFrame = MagnifiedFrame | TrueFrame
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

newtype CoercedLength = InternalGlueAsLength InternalGlue
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Describe CoercedLength where
  describe (InternalGlueAsLength ig) =
    singleLine $ "CoercedLength/InternalGlue " <> quote (show ig)

-- Math-length.
type MathLength = T.Signed UnsignedMathLength

data UnsignedMathLength
  = NormalMathLengthAsUMathLength NormalMathLength
  | CoercedMathLength CoercedMathLength
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

-- Think: 'un-coerced length'.
data NormalMathLength
  = -- 'semi-constant' because Factor and Unit can be quite un-constant-like.
    MathLengthSemiConstant Factor MathUnit
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data MathUnit = Mu | InternalMathGlueAsUnit InternalMathGlue
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

newtype CoercedMathLength = InternalMathGlueAsMathLength InternalMathGlue
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

-- Glue.
data Glue
  = ExplicitGlue Length (Maybe Flex) (Maybe Flex)
  | InternalGlue (T.Signed InternalGlue)
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Describe Glue where
  describe = \case
    ExplicitGlue len mayStretch mayShrink ->
      [ (0, "Gliue/Explicit")
      ]
        <> describePrepended 1 "Length" len
        <> case mayStretch of
          Nothing ->
            [(1, "No stretch")]
          Just stretch ->
            describePrepended 1 "Stretch" stretch
        <> case mayShrink of
          Nothing -> [(1, "No shrink")]
          Just shrink ->
            describePrepended 1 "Shrink" shrink
    InternalGlue ig ->
      [ (0, "Glue/Internal " <> quote (show ig))
      ]

data Flex = FiniteFlex Length | FilFlex FilLength
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Describe Flex where
  describe = singleLine . show

oneFilFlex, minusOneFilFlex, oneFillFlex :: Flex
oneFilFlex = FilFlex oneFil
minusOneFilFlex = FilFlex minusOneFil
oneFillFlex = FilFlex oneFill

data FilLength = FilLength (T.Signed Factor) Int
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

oneFil, minusOneFil, oneFill :: FilLength
oneFil = FilLength (T.Signed T.Positive oneFactor) 1
minusOneFil = FilLength (T.Signed T.Negative oneFactor) 1
oneFill = FilLength (T.Signed T.Positive oneFactor) 2

-- Math glue.
data MathGlue
  = ExplicitMathGlue MathLength (Maybe MathFlex) (Maybe MathFlex)
  | InternalMathGlue T.Sign InternalMathGlue
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data MathFlex = FiniteMathFlex MathLength | FilMathFlex FilLength
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

-- Internal quantities.
data QuantVariable a = ParamVar a | RegisterVar EightBitTeXInt
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

type TeXIntVariable = QuantVariable T.TeXIntParameter

type LengthVariable = QuantVariable T.LengthParameter

type GlueVariable = QuantVariable T.GlueParameter

type MathGlueVariable = QuantVariable T.MathGlueParameter

type TokenListVariable = QuantVariable T.TokenListParameter

data InternalTeXInt
  = InternalTeXIntVariable TeXIntVariable
  | InternalSpecialTeXInt T.SpecialTeXInt
  | InternalCodeTableRef CodeTableRef
  | InternalCharToken Q.TeXInt
  | InternalMathCharToken Q.TeXInt
  | InternalFontCharRef FontCharRef
  | LastPenalty
  | ParShape
  | InputLineNr
  | Badness
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data CodeTableRef = CodeTableRef T.CodeType TeXInt
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data FontCharRef = FontCharRef T.FontChar FontRef
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data FontRef
  = FontTokenRef Q.TeXInt
  | CurrentFontRef
  | FamilyMemberFontRef FamilyMember
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data FamilyMember = FamilyMember T.FontRange TeXInt
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data BoxDimensionRef = BoxDimensionRef EightBitTeXInt BoxDim
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data FontDimensionRef = FontDimensionRef TeXInt FontRef
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data InternalLength
  = InternalLengthVariable LengthVariable
  | InternalSpecialLength T.SpecialLength
  | InternalFontDimensionRef FontDimensionRef
  | InternalBoxDimensionRef BoxDimensionRef
  | LastKern
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Describe InternalLength where
  describe = singleLine . show

data InternalGlue = InternalGlueVariable GlueVariable | LastGlue
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data InternalMathGlue
  = InternalMathGlueVariable MathGlueVariable
  | LastMathGlue
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

-- Assignments.
data Assignment = Assignment {body :: AssignmentBody, scope :: T.ScopeFlag}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

newtype TeXFilePath = TeXFilePath (Path.Path Path.Rel Path.File)
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data ControlSequenceTarget
  = MacroTarget T.MacroContents
  | LetTarget Lex.Token
  | FutureLetTarget Lex.Token Lex.Token
  | ShortDefineTarget T.QuantityType TeXInt
  | ReadTarget TeXInt
  | FontTarget FontSpecification TeXFilePath
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data AssignmentBody
  = DefineControlSequence Lex.ControlSequenceLike ControlSequenceTarget
  | SetVariable VariableAssignment
  | ModifyVariable VariableModification
  | AssignCode CodeAssignment
  | SelectFont Q.TeXInt
  | SetFamilyMember FamilyMember FontRef
  | SetParShape [(Length, Length)]
  | SetBoxRegister EightBitTeXInt Box
  | -- -- Global assignments.
    SetFontDimension FontDimensionRef Length
  | SetFontChar FontCharRef TeXInt
  | SetHyphenation T.BalancedText
  | SetHyphenationPatterns T.BalancedText
  | SetBoxDimension BoxDimensionRef Length
  | SetInteractionMode T.InteractionMode
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data TokenListAssignmentTarget
  = TokenListAssignmentVar TokenListVariable
  | TokenListAssignmentText T.BalancedText
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data VariableAssignment
  = TeXIntVariableAssignment TeXIntVariable TeXInt
  | LengthVariableAssignment LengthVariable Length
  | GlueVariableAssignment GlueVariable Glue
  | MathGlueVariableAssignment MathGlueVariable MathGlue
  | TokenListVariableAssignment TokenListVariable TokenListAssignmentTarget
  | SpecialTeXIntVariableAssignment T.SpecialTeXInt TeXInt
  | SpecialLengthVariableAssignment T.SpecialLength Length
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data VariableModification
  = AdvanceTeXIntVariable TeXIntVariable TeXInt
  | AdvanceLengthVariable LengthVariable Length
  | AdvanceGlueVariable GlueVariable Glue
  | AdvanceMathGlueVariable MathGlueVariable MathGlue
  | ScaleVariable VDirection NumericVariable TeXInt
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data NumericVariable
  = TeXIntNumericVariable TeXIntVariable
  | LengthNumericVariable LengthVariable
  | GlueNumericVariable GlueVariable
  | MathGlueNumericVariable MathGlueVariable
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data CodeAssignment = CodeAssignment CodeTableRef TeXInt
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data FontSpecification = NaturalFont | FontAt Length | FontScaled TeXInt
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

-- Box specification.
data Box
  = FetchedRegisterBox T.BoxFetchMode EightBitTeXInt
  | LastBox
  | VSplitBox TeXInt Length
  | ExplicitBox BoxSpecification T.ExplicitBox
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data BoxSpecification = Natural | To Length | Spread Length
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data BoxOrRule = BoxOrRuleBox Box | BoxOrRuleRule Axis Rule
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

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
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Describe ModeIndependentCommand where
  describe = singleLine . show

data Command
  = ShowToken Lex.Token
  | ShowBox TeXInt
  | ShowLists
  | ShowTheInternalQuantity InternalQuantity
  | ShipOut Box
  | AddMark T.BalancedText
  | -- -- Note: this *is* an all-modes command. It can happen in non-vertical modes,
    -- -- then can 'migrate' out.
    -- \| AddInsertion TeXInt VModeMaterial
    -- \| AddAdjustment VModeMaterial
    AddSpace
  | StartParagraph T.IndentFlag
  | EndParagraph
  | -- \| AddAlignedMaterial DesiredLength AlignmentMaterial
    HModeCommand HModeCommand
  | VModeCommand VModeCommand
  | ModeIndependentCommand ModeIndependentCommand
  deriving stock (Show, Generic)

instance ToJSON Command

instance Describe Command where
  describe = \case
    HModeCommand c ->
      describePrepended 0 "Command/H" c
    VModeCommand c ->
      describePrepended 0 "Command/V" c
    ModeIndependentCommand c ->
      describePrepended 0 "Command/AllMode" c
    c ->
      singleLine $ show c

data VModeCommand
  = End
  | Dump
  | EnterHMode
  | AddVGlue Glue
  | AddVLeaders LeadersSpec
  | AddVRule Rule
  | AddUnwrappedFetchedVBox FetchedBoxRef -- \unv{box,copy}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Describe VModeCommand where
  describe = singleLine . show

data HModeCommand
  = AddControlSpace
  | AddCharacter CharCodeRef
  | AddAccentedCharacter TeXInt [Assignment] (Maybe CharCodeRef)
  | AddItalicCorrection
  | AddDiscretionaryText {preBreak, postBreak, noBreak :: T.BalancedText}
  | AddDiscretionaryHyphen
  | EnterMathMode
  | AddHGlue Glue
  | AddHLeaders LeadersSpec
  | AddHRule Rule
  | AddUnwrappedFetchedHBox FetchedBoxRef -- \unh{box,copy}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Describe HModeCommand where
  describe = \case
    AddCharacter ref ->
      describePrepended 0 "HModeCommand/AddCharacter" ref
    AddHGlue g ->
      describePrepended 0 "HModeCommand/AddGlue" g
    c ->
      singleLine $ "HModeCommand/" <> show c

data FetchedBoxRef = FetchedBoxRef TeXInt T.BoxFetchMode
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data LeadersSpec = LeadersSpec T.LeadersType BoxOrRule Glue
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data CommandTrigger = CharCommandTrigger | CSCommandTrigger
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data InternalQuantity
  = InternalTeXIntQuantity InternalTeXInt
  | InternalLengthQuantity InternalLength
  | InternalGlueQuantity InternalGlue
  | InternalMathGlueQuantity InternalMathGlue
  | FontQuantity FontRef
  | TokenListVariableQuantity TokenListVariable
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data WriteText
  = ImmediateWriteText T.ExpandedBalancedText
  | DeferredWriteText T.BalancedText
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data WritePolicy = Immediate | Deferred
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data Rule = Rule {width, height, depth :: Maybe Length}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data FileStreamAction = Open TeXFilePath | Close
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data FileStreamType = FileInput | FileOutput WritePolicy
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data BoxPlacement = NaturalPlacement | ShiftedPlacement Axis Direction Length
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data CharCodeRef
  = CharRef Code.CharCode
  | CharTokenRef Q.TeXInt
  | CharCodeNrRef TeXInt
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Describe CharCodeRef where
  describe = \case
    CharRef c ->
      describePrepended 0 "CharCodeRef/CharRef" c
    v ->
      singleLine $ show v

-- Condition heads.
data IfConditionHead
  = IfTeXIntPairTest TeXInt Ordering TeXInt -- \ifnum
  | IfLengthPairTest Length Ordering Length -- \ifdim
  | IfTeXIntOdd TeXInt -- \ifodd
  | IfInMode T.ModeAttribute -- \ifvmode, \ifhmode, \ifmmode, \ifinner
  | IfTokenAttributesEqual T.TokenAttribute T.PrimitiveToken T.PrimitiveToken -- \if, \ifcat
  | IfTokensEqual Lex.Token Lex.Token -- \ifx
  | IfBoxRegisterIs T.BoxRegisterAttribute TeXInt -- \ifvoid, \ifhbox, \ifvbox
  | IfInputEnded TeXInt -- \ifeof
  | IfConst Bool -- \iftrue, \iffalse
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data ConditionHead = IfConditionHead IfConditionHead | CaseConditionHead TeXInt
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)
