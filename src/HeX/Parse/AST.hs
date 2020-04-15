module Hex.Parse.AST where

import           Hexlude

import qualified Path

import qualified Hex.Lex          as Lex
import qualified Hex.Resolve.Token  as T
import qualified Hex.Quantity     as Q
import qualified Hex.Config.Codes as Code

-- TeXInt.

type TeXInt = T.Signed UnsignedTeXInt

newtype EightBitTeXInt = EightBitTeXInt TeXInt
    deriving stock (Show)

constTeXInt :: Q.TeXInt -> TeXInt
constTeXInt n = T.Signed T.Positive $ constUTeXInt n

data UnsignedTeXInt =
    NormalTeXIntAsUTeXInt NormalTeXInt | CoercedTeXInt CoercedTeXInt
    deriving stock (Show)

constUTeXInt :: Q.TeXInt -> UnsignedTeXInt
constUTeXInt n = NormalTeXIntAsUTeXInt $ TeXIntConstant n

-- Think: 'un-coerced integer'.
data NormalTeXInt = TeXIntConstant Q.TeXInt | InternalTeXInt InternalTeXInt
    deriving stock (Show)

zeroTeXInt :: NormalTeXInt
zeroTeXInt = TeXIntConstant 0

oneTeXInt :: NormalTeXInt
oneTeXInt = TeXIntConstant 1

data CoercedTeXInt =
    InternalLengthAsInt InternalLength | InternalGlueAsInt InternalGlue
    deriving stock (Show)

-- Length.
type Length = T.Signed UnsignedLength

zeroLength :: Length
zeroLength = T.Signed T.Positive $
    NormalLengthAsULength $
    LengthSemiConstant zeroFactor scaledPointUnit

data UnsignedLength =
    NormalLengthAsULength NormalLength | CoercedLength CoercedLength
    deriving stock (Show)

instance Readable UnsignedLength where
    describe = \case
        NormalLengthAsULength v -> describe v
        CoercedLength v -> describe v

-- Think: 'un-coerced length'.
data NormalLength =
      -- 'semi-constant' because Factor and Unit can be quite un-constant-like.
      LengthSemiConstant Factor Unit
    | InternalLength InternalLength
    deriving stock (Show)

instance Readable NormalLength where
    describe = \case
        LengthSemiConstant f u -> show f <> " " <> describe u
        InternalLength il -> show il

data Factor =
      NormalTeXIntFactor NormalTeXInt
      -- Badly named 'decimal constant' in the TeXbook. Granted, it is specified
      -- with decimal digits, but its main feature is that it can represent
      -- non-integers.
    | RationalConstant Rational
    deriving stock (Show)

zeroFactor, oneFactor :: Factor
zeroFactor = NormalTeXIntFactor zeroTeXInt

oneFactor = NormalTeXIntFactor oneTeXInt

data Unit =
    PhysicalUnit PhysicalUnitFrame Q.PhysicalUnit | InternalUnit InternalUnit
    deriving stock (Show)

instance Readable Unit where
    describe = \case
        PhysicalUnit MagnifiedFrame pu -> describe pu
        PhysicalUnit TrueFrame pu -> "true " <> describe pu
        InternalUnit iu -> describe iu

scaledPointUnit :: Unit
scaledPointUnit = PhysicalUnit MagnifiedFrame Q.ScaledPoint

data InternalUnit =
      Em
    | Ex
    | InternalTeXIntUnit InternalTeXInt
    | InternalLengthUnit InternalLength
    | InternalGlueUnit InternalGlue
    deriving stock (Show)

instance Readable InternalUnit where
    describe = \case
        Em -> "em"
        Ex -> "ex"
        v -> show v

data PhysicalUnitFrame = MagnifiedFrame | TrueFrame
    deriving stock (Show)

newtype CoercedLength = InternalGlueAsLength InternalGlue
    deriving stock (Show)

instance Readable CoercedLength where
    describe (InternalGlueAsLength ig) = "fromGlue(" <> show ig <> ")"

-- Math-length.
type MathLength = T.Signed UnsignedMathLength

data UnsignedMathLength = NormalMathLengthAsUMathLength NormalMathLength
                        | CoercedMathLength CoercedMathLength
    deriving stock (Show)

-- Think: 'un-coerced length'.
data NormalMathLength =
    -- 'semi-constant' because Factor and Unit can be quite un-constant-like.
    MathLengthSemiConstant Factor MathUnit
    deriving stock (Show)

data MathUnit = Mu | InternalMathGlueAsUnit InternalMathGlue
    deriving stock (Show)

newtype CoercedMathLength = InternalMathGlueAsMathLength InternalMathGlue
    deriving stock (Show)

-- Glue.
data Glue = ExplicitGlue Length (Maybe Flex) (Maybe Flex)
          | InternalGlue (T.Signed InternalGlue)
    deriving stock (Show)

instance Readable Glue where
    describe = \case
        ExplicitGlue len mayStretch mayShrink ->
            describe len
            <> case mayStretch of
                    Nothing -> ""
                    Just stretch -> " plus " <> show stretch
            <> case mayShrink of
                    Nothing -> ""
                    Just shrink -> " minus " <> show shrink
        InternalGlue ig ->
            show ig

data Flex = FiniteFlex Length | FilFlex FilLength
    deriving stock (Show)

oneFilFlex, minusOneFilFlex, oneFillFlex :: Flex
oneFilFlex = FilFlex oneFil

minusOneFilFlex = FilFlex minusOneFil

oneFillFlex = FilFlex oneFill

data FilLength = FilLength (T.Signed Factor) Int
    deriving stock (Show)

oneFil, minusOneFil, oneFill :: FilLength
oneFil = FilLength (T.Signed T.Positive oneFactor) 1

minusOneFil = FilLength (T.Signed T.Negative oneFactor) 1

oneFill = FilLength (T.Signed T.Positive oneFactor) 2

-- Math glue.
data MathGlue = ExplicitMathGlue MathLength (Maybe MathFlex) (Maybe MathFlex)
              | InternalMathGlue T.Sign InternalMathGlue
    deriving stock (Show)

data MathFlex = FiniteMathFlex MathLength | FilMathFlex FilLength
    deriving stock (Show)

-- Internal quantities.
data QuantVariable a = ParamVar a | RegisterVar EightBitTeXInt
    deriving stock (Show)

type TeXIntVariable = QuantVariable T.TeXIntParameter

type LengthVariable = QuantVariable T.LengthParameter

type GlueVariable = QuantVariable T.GlueParameter

type MathGlueVariable = QuantVariable T.MathGlueParameter

type TokenListVariable = QuantVariable T.TokenListParameter

data InternalTeXInt =
      InternalTeXIntVariable TeXIntVariable
    | InternalSpecialTeXInt T.SpecialTeXInt
    | InternalCodeTableRef CodeTableRef
    | InternalCharToken Q.TeXInt
    | InternalMathCharToken Q.TeXInt
    | InternalFontCharRef FontCharRef
    | LastPenalty
    | ParShape
    | InputLineNr
    | Badness
    deriving stock (Show)

data CodeTableRef = CodeTableRef T.CodeType TeXInt
    deriving stock (Show)

data FontCharRef = FontCharRef T.FontChar FontRef
    deriving stock (Show)

data FontRef =
    FontTokenRef Q.TeXInt | CurrentFontRef | FamilyMemberFontRef FamilyMember
    deriving stock (Show)

data FamilyMember = FamilyMember T.FontRange TeXInt
    deriving stock (Show)

data BoxDimensionRef = BoxDimensionRef EightBitTeXInt BoxDim
    deriving stock (Show)

data FontDimensionRef = FontDimensionRef TeXInt FontRef
    deriving stock (Show)

data InternalLength =
      InternalLengthVariable LengthVariable
    | InternalSpecialLength T.SpecialLength
    | InternalFontDimensionRef FontDimensionRef
    | InternalBoxDimensionRef BoxDimensionRef
    | LastKern
    deriving stock (Show)

data InternalGlue = InternalGlueVariable GlueVariable | LastGlue
    deriving stock (Show)

data InternalMathGlue =
    InternalMathGlueVariable MathGlueVariable | LastMathGlue
    deriving stock (Show)

-- Assignments.
data Assignment = Assignment { body :: AssignmentBody, global :: T.GlobalFlag }
    deriving stock (Show)

newtype TeXFilePath = TeXFilePath (Path.Path Path.Rel Path.File)
    deriving stock (Show)

data ControlSequenceTarget =
      MacroTarget T.MacroContents
    | LetTarget Lex.Token
    | FutureLetTarget Lex.Token Lex.Token
    | ShortDefineTarget T.QuantityType TeXInt
    | ReadTarget TeXInt
    | FontTarget FontSpecification TeXFilePath
    deriving stock (Show)

data AssignmentBody =
      DefineControlSequence Lex.ControlSequenceLike ControlSequenceTarget
    | SetVariable VariableAssignment
    | ModifyVariable VariableModification
    | AssignCode CodeAssignment
    | SelectFont Q.TeXInt
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
    deriving stock (Show)

data TokenListAssignmentTarget = TokenListAssignmentVar TokenListVariable
                               | TokenListAssignmentText T.BalancedText
    deriving stock (Show)

data VariableAssignment =
      TeXIntVariableAssignment TeXIntVariable TeXInt
    | LengthVariableAssignment LengthVariable Length
    | GlueVariableAssignment GlueVariable Glue
    | MathGlueVariableAssignment MathGlueVariable MathGlue
    | TokenListVariableAssignment TokenListVariable TokenListAssignmentTarget
    | SpecialTeXIntVariableAssignment T.SpecialTeXInt TeXInt
    | SpecialLengthVariableAssignment T.SpecialLength Length
    deriving stock (Show)

data VariableModification =
      AdvanceTeXIntVariable TeXIntVariable TeXInt
    | AdvanceLengthVariable LengthVariable Length
    | AdvanceGlueVariable GlueVariable Glue
    | AdvanceMathGlueVariable MathGlueVariable MathGlue
    | ScaleVariable VDirection NumericVariable TeXInt
    deriving stock (Show)

data NumericVariable = TeXIntNumericVariable TeXIntVariable
                     | LengthNumericVariable LengthVariable
                     | GlueNumericVariable GlueVariable
                     | MathGlueNumericVariable MathGlueVariable
    deriving stock (Show)

data CodeAssignment = CodeAssignment CodeTableRef TeXInt
    deriving stock (Show)

data FontSpecification = NaturalFont | FontAt Length | FontScaled TeXInt
    deriving stock (Show)

-- Box specification.
data Box = FetchedRegisterBox T.BoxFetchMode EightBitTeXInt
         | LastBox
         | VSplitBox TeXInt Length
         | ExplicitBox BoxSpecification T.ExplicitBox
    deriving stock (Show)

data BoxSpecification = Natural | To Length | Spread Length
    deriving stock (Show)

data BoxOrRule = BoxOrRuleBox Box | BoxOrRuleRule Axis Rule
    deriving stock (Show)

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
    deriving stock (Show)

instance Readable ModeIndependentCommand where
    describe = show

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
    deriving stock (Show)

instance Readable Command where
    describe = \case
        HModeCommand c -> describe c
        VModeCommand c -> describe c
        ModeIndependentCommand c -> describe c
        c -> show c


data VModeCommand
    = End
    | Dump
    | EnterHMode
    | AddVGlue Glue
    | AddVLeaders LeadersSpec
    | AddVRule Rule
    | AddUnwrappedFetchedVBox FetchedBoxRef -- \unv{box,copy}
    deriving stock (Show)

instance Readable VModeCommand where
    describe = show

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
    deriving stock (Show)

instance Readable HModeCommand where
    describe = \case
        AddCharacter ref -> "Add char " <> describe ref
        AddHGlue g -> "Add glue " <> describe g
        c -> show c

data FetchedBoxRef = FetchedBoxRef TeXInt T.BoxFetchMode
    deriving stock (Show)

data LeadersSpec = LeadersSpec T.LeadersType BoxOrRule Glue
    deriving stock (Show)

data CommandTrigger = CharCommandTrigger | CSCommandTrigger
    deriving stock ( Show, Eq )

data InternalQuantity =
      InternalTeXIntQuantity InternalTeXInt
    | InternalLengthQuantity InternalLength
    | InternalGlueQuantity InternalGlue
    | InternalMathGlueQuantity InternalMathGlue
    | FontQuantity FontRef
    | TokenListVariableQuantity TokenListVariable
    deriving stock (Show)

data WriteText = ImmediateWriteText T.ExpandedBalancedText
               | DeferredWriteText T.BalancedText
    deriving stock (Show)

data WritePolicy = Immediate | Deferred
    deriving stock (Show)

data Rule = Rule { width, height, depth :: Maybe Length }
    deriving stock (Show)

data FileStreamAction = Open TeXFilePath | Close
    deriving stock (Show)

data FileStreamType = FileInput | FileOutput WritePolicy
    deriving stock (Show)

data BoxPlacement = NaturalPlacement | ShiftedPlacement Axis Direction Length
    deriving stock (Show)

data CharCodeRef =
    CharRef Code.CharCode | CharTokenRef Q.TeXInt | CharCodeNrRef TeXInt
    deriving stock (Show)

instance Readable CharCodeRef where
    describe = \case
        CharRef c -> describe c
        v -> show v

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
    deriving stock (Show)

data ConditionHead = IfConditionHead IfConditionHead | CaseConditionHead TeXInt
    deriving stock (Show)
