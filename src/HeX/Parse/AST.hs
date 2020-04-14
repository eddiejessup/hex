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
    deriving (Show)

constTeXInt :: Q.TeXInt -> TeXInt
constTeXInt n = T.Signed T.Positive $ constUTeXInt n

data UnsignedTeXInt =
    NormalTeXIntAsUTeXInt NormalTeXInt | CoercedTeXInt CoercedTeXInt
    deriving (Show)

constUTeXInt :: Q.TeXInt -> UnsignedTeXInt
constUTeXInt n = NormalTeXIntAsUTeXInt $ TeXIntConstant n

-- Think: 'un-coerced integer'.
data NormalTeXInt = TeXIntConstant Q.TeXInt | InternalTeXInt InternalTeXInt
    deriving (Show)

zeroTeXInt :: NormalTeXInt
zeroTeXInt = TeXIntConstant 0

oneTeXInt :: NormalTeXInt
oneTeXInt = TeXIntConstant 1

data CoercedTeXInt =
    InternalLengthAsInt InternalLength | InternalGlueAsInt InternalGlue
    deriving (Show)

-- Length.
type Length = T.Signed UnsignedLength

zeroLength :: Length
zeroLength = T.Signed T.Positive $
    NormalLengthAsULength $
    LengthSemiConstant zeroFactor scaledPointUnit

data UnsignedLength =
    NormalLengthAsULength NormalLength | CoercedLength CoercedLength
    deriving (Show)

instance Readable UnsignedLength where
    describe = \case
        NormalLengthAsULength v -> describe v
        CoercedLength v -> describe v

-- Think: 'un-coerced length'.
data NormalLength =
      -- 'semi-constant' because Factor and Unit can be quite un-constant-like.
      LengthSemiConstant Factor Unit
    | InternalLength InternalLength
    deriving (Show)

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
    deriving (Show)

zeroFactor, oneFactor :: Factor
zeroFactor = NormalTeXIntFactor zeroTeXInt

oneFactor = NormalTeXIntFactor oneTeXInt

data Unit =
    PhysicalUnit PhysicalUnitFrame Q.PhysicalUnit | InternalUnit InternalUnit
    deriving (Show)

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
    deriving (Show)

instance Readable InternalUnit where
    describe = \case
        Em -> "em"
        Ex -> "ex"
        v -> show v

data PhysicalUnitFrame = MagnifiedFrame | TrueFrame
    deriving (Show)

newtype CoercedLength = InternalGlueAsLength InternalGlue
    deriving (Show)

instance Readable CoercedLength where
    describe (InternalGlueAsLength ig) = "fromGlue(" <> show ig <> ")"

-- Math-length.
type MathLength = T.Signed UnsignedMathLength

data UnsignedMathLength = NormalMathLengthAsUMathLength NormalMathLength
                        | CoercedMathLength CoercedMathLength
    deriving (Show)

-- Think: 'un-coerced length'.
data NormalMathLength =
    -- 'semi-constant' because Factor and Unit can be quite un-constant-like.
    MathLengthSemiConstant Factor MathUnit
    deriving (Show)

data MathUnit = Mu | InternalMathGlueAsUnit InternalMathGlue
    deriving (Show)

newtype CoercedMathLength = InternalMathGlueAsMathLength InternalMathGlue
    deriving (Show)

-- Glue.
data Glue = ExplicitGlue Length (Maybe Flex) (Maybe Flex)
          | InternalGlue (T.Signed InternalGlue)
    deriving (Show)

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
    deriving (Show)

oneFilFlex, minusOneFilFlex, oneFillFlex :: Flex
oneFilFlex = FilFlex oneFil

minusOneFilFlex = FilFlex minusOneFil

oneFillFlex = FilFlex oneFill

data FilLength = FilLength (T.Signed Factor) Int
    deriving (Show)

oneFil, minusOneFil, oneFill :: FilLength
oneFil = FilLength (T.Signed T.Positive oneFactor) 1

minusOneFil = FilLength (T.Signed T.Negative oneFactor) 1

oneFill = FilLength (T.Signed T.Positive oneFactor) 2

-- Math glue.
data MathGlue = ExplicitMathGlue MathLength (Maybe MathFlex) (Maybe MathFlex)
              | InternalMathGlue T.Sign InternalMathGlue
    deriving (Show)

data MathFlex = FiniteMathFlex MathLength | FilMathFlex FilLength
    deriving (Show)

-- Internal quantities.
data QuantVariable a = ParamVar a | RegisterVar EightBitTeXInt
    deriving (Show)

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
    deriving (Show)

data CodeTableRef = CodeTableRef T.CodeType TeXInt
    deriving (Show)

data FontCharRef = FontCharRef T.FontChar FontRef
    deriving (Show)

data FontRef =
    FontTokenRef Q.TeXInt | CurrentFontRef | FamilyMemberFontRef FamilyMember
    deriving (Show)

data FamilyMember = FamilyMember T.FontRange TeXInt
    deriving (Show)

data BoxDimensionRef = BoxDimensionRef EightBitTeXInt BoxDim
    deriving (Show)

data FontDimensionRef = FontDimensionRef TeXInt FontRef
    deriving (Show)

data InternalLength =
      InternalLengthVariable LengthVariable
    | InternalSpecialLength T.SpecialLength
    | InternalFontDimensionRef FontDimensionRef
    | InternalBoxDimensionRef BoxDimensionRef
    | LastKern
    deriving (Show)

data InternalGlue = InternalGlueVariable GlueVariable | LastGlue
    deriving (Show)

data InternalMathGlue =
    InternalMathGlueVariable MathGlueVariable | LastMathGlue
    deriving (Show)

-- Assignments.
data Assignment = Assignment { body :: AssignmentBody, global :: T.GlobalFlag }
    deriving (Show)

newtype TeXFilePath = TeXFilePath (Path.Path Path.Rel Path.File)
    deriving (Show)

data ControlSequenceTarget =
      MacroTarget T.MacroContents
    | LetTarget Lex.Token
    | FutureLetTarget Lex.Token Lex.Token
    | ShortDefineTarget T.QuantityType TeXInt
    | ReadTarget TeXInt
    | FontTarget FontSpecification TeXFilePath
    deriving (Show)

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
    deriving (Show)

data TokenListAssignmentTarget = TokenListAssignmentVar TokenListVariable
                               | TokenListAssignmentText T.BalancedText
    deriving (Show)

data VariableAssignment =
      TeXIntVariableAssignment TeXIntVariable TeXInt
    | LengthVariableAssignment LengthVariable Length
    | GlueVariableAssignment GlueVariable Glue
    | MathGlueVariableAssignment MathGlueVariable MathGlue
    | TokenListVariableAssignment TokenListVariable TokenListAssignmentTarget
    | SpecialTeXIntVariableAssignment T.SpecialTeXInt TeXInt
    | SpecialLengthVariableAssignment T.SpecialLength Length
    deriving (Show)

data VariableModification =
      AdvanceTeXIntVariable TeXIntVariable TeXInt
    | AdvanceLengthVariable LengthVariable Length
    | AdvanceGlueVariable GlueVariable Glue
    | AdvanceMathGlueVariable MathGlueVariable MathGlue
    | ScaleVariable VDirection NumericVariable TeXInt
    deriving (Show)

data NumericVariable = TeXIntNumericVariable TeXIntVariable
                     | LengthNumericVariable LengthVariable
                     | GlueNumericVariable GlueVariable
                     | MathGlueNumericVariable MathGlueVariable
    deriving (Show)

data CodeAssignment = CodeAssignment CodeTableRef TeXInt
    deriving (Show)

data FontSpecification = NaturalFont | FontAt Length | FontScaled TeXInt
    deriving (Show)

-- Box specification.
data Box = FetchedRegisterBox T.BoxFetchMode EightBitTeXInt
         | LastBox
         | VSplitBox TeXInt Length
         | ExplicitBox BoxSpecification T.ExplicitBox
    deriving (Show)

data BoxSpecification = Natural | To Length | Spread Length
    deriving (Show)

data BoxOrRule = BoxOrRuleBox Box | BoxOrRuleRule Axis Rule
    deriving (Show)

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
    deriving (Show)

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
    deriving (Show)

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
    deriving (Show)

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
    deriving (Show)

instance Readable HModeCommand where
    describe = \case
        AddCharacter ref -> "Add char " <> describe ref
        AddHGlue g -> "Add glue " <> describe g
        c -> show c

data FetchedBoxRef = FetchedBoxRef TeXInt T.BoxFetchMode
    deriving (Show)

data LeadersSpec = LeadersSpec T.LeadersType BoxOrRule Glue
    deriving (Show)

data CommandTrigger = CharCommandTrigger | CSCommandTrigger
    deriving ( Show, Eq )

data InternalQuantity =
      InternalTeXIntQuantity InternalTeXInt
    | InternalLengthQuantity InternalLength
    | InternalGlueQuantity InternalGlue
    | InternalMathGlueQuantity InternalMathGlue
    | FontQuantity FontRef
    | TokenListVariableQuantity TokenListVariable
    deriving (Show)

data WriteText = ImmediateWriteText T.ExpandedBalancedText
               | DeferredWriteText T.BalancedText
    deriving (Show)

data WritePolicy = Immediate | Deferred
    deriving (Show)

data Rule = Rule { width, height, depth :: Maybe Length }
    deriving (Show)

data FileStreamAction = Open TeXFilePath | Close
    deriving (Show)

data FileStreamType = FileInput | FileOutput WritePolicy
    deriving (Show)

data BoxPlacement = NaturalPlacement | ShiftedPlacement Axis Direction Length
    deriving (Show)

data CharCodeRef =
    CharRef Code.CharCode | CharTokenRef Q.TeXInt | CharCodeNrRef TeXInt
    deriving (Show)

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
    deriving (Show)

data ConditionHead = IfConditionHead IfConditionHead | CaseConditionHead TeXInt
    deriving (Show)
