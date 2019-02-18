{-# LANGUAGE DuplicateRecordFields #-}

module HeX.Parse.AST where

import           Path                           ( File
                                                , Path
                                                , Rel
                                                )

import           HeX.Type
import           HeX.Categorise                 ( CharCode )
import qualified HeX.Lex                       as Lex
import           HeX.Unit                       ( PhysicalUnit(..) )
import qualified HeX.Parse.Token               as T

-- Number.

data Number = Number T.Sign UnsignedNumber
    deriving (Show)

constNumber :: IntVal -> Number
constNumber n = Number (T.Sign True) $ constUNumber n

data UnsignedNumber
    = NormalIntegerAsUNumber NormalInteger
    | CoercedInteger CoercedInteger
    deriving (Show)

constUNumber :: IntVal -> UnsignedNumber
constUNumber n = NormalIntegerAsUNumber $ IntegerConstant n

-- Think: 'un-coerced integer'.
data NormalInteger
    = IntegerConstant Int
    | InternalInteger InternalInteger
    deriving (Show)

zeroInteger, oneInteger :: NormalInteger
zeroInteger = IntegerConstant 0
oneInteger = IntegerConstant 1

data CoercedInteger
    = InternalLengthAsInt InternalLength
    | InternalGlueAsInt InternalGlue
    deriving (Show)

-- Length.

data Length = Length T.Sign UnsignedLength
    deriving (Show)

zeroLength :: Length
zeroLength = Length (T.Sign True) $ NormalLengthAsULength $ LengthSemiConstant zeroFactor scaledPointUnit

data UnsignedLength
    = NormalLengthAsULength NormalLength
    | CoercedLength CoercedLength
    deriving (Show)

-- Think: 'un-coerced length'.
data NormalLength
    -- 'semi-constant' because Factor and Unit can be quite un-constant-like.
    = LengthSemiConstant Factor Unit
    | InternalLength InternalLength
    deriving (Show)

data Factor
    = NormalIntegerFactor NormalInteger
    -- Badly named 'decimal constant' in the TeXbook. Granted, it is specified
    -- with decimal digits, but its main feature is that it can represent
    -- non-integers.
    | RationalConstant Rational
    deriving (Show)

zeroFactor, oneFactor :: Factor
zeroFactor = NormalIntegerFactor zeroInteger
oneFactor = NormalIntegerFactor oneInteger

data Unit
    = PhysicalUnit PhysicalUnitFrame PhysicalUnit
    | InternalUnit InternalUnit
    deriving (Show)

scaledPointUnit :: Unit
scaledPointUnit = PhysicalUnit MagnifiedFrame ScaledPoint

data InternalUnit
    = Em
    | Ex
    | InternalIntegerUnit InternalInteger
    | InternalLengthUnit InternalLength
    | InternalGlueUnit InternalGlue
    deriving (Show)

data PhysicalUnitFrame
    = MagnifiedFrame
    | TrueFrame
    deriving (Show)

data CoercedLength
    = InternalGlueAsLength InternalGlue
    deriving (Show)

-- Math-length.

data MathLength = MathLength T.Sign UnsignedMathLength
    deriving (Show)

data UnsignedMathLength
    = NormalMathLengthAsUMathLength NormalMathLength
    | CoercedMathLength CoercedMathLength
    deriving (Show)

-- Think: 'un-coerced length'.
data NormalMathLength
    -- 'semi-constant' because Factor and Unit can be quite un-constant-like.
    = MathLengthSemiConstant Factor MathUnit
    deriving (Show)

data MathUnit
    = Mu
    | InternalMathGlueAsUnit InternalMathGlue
    deriving (Show)

data CoercedMathLength
    = InternalMathGlueAsMathLength InternalMathGlue
    deriving (Show)

-- Glue.

data Glue
    = ExplicitGlue Length (Maybe Flex) (Maybe Flex)
    | InternalGlue T.Sign InternalGlue
    deriving (Show)

data Flex
    = FiniteFlex Length
    | FilFlex FilLength
    deriving (Show)

oneFilFlex, minusOneFilFlex, oneFillFlex :: Flex
oneFilFlex = FilFlex oneFil
minusOneFilFlex = FilFlex minusOneFil
oneFillFlex = FilFlex oneFill

data FilLength = FilLength T.Sign Factor Int
    deriving (Show)

oneFil, minusOneFil, oneFill :: FilLength
oneFil = FilLength (T.Sign True) oneFactor 1
minusOneFil = FilLength (T.Sign False) oneFactor 1
oneFill = FilLength (T.Sign True) oneFactor 2

-- Math glue.

data MathGlue
    = ExplicitMathGlue MathLength (Maybe MathFlex) (Maybe MathFlex)
    | InternalMathGlue T.Sign InternalMathGlue
    deriving (Show)

data MathFlex
    = FiniteMathFlex MathLength
    | FilMathFlex FilLength
    deriving (Show)

-- Internal quantities.

data QuantVariable a
    = ParamVar a
    | RegisterVar Number
    deriving (Show)

type IntegerVariable = QuantVariable T.IntegerParameter
type LengthVariable = QuantVariable T.LengthParameter
type GlueVariable = QuantVariable T.GlueParameter
type MathGlueVariable = QuantVariable T.MathGlueParameter
type TokenListVariable = QuantVariable T.TokenListParameter

data InternalInteger
    = InternalIntegerVariable IntegerVariable
    | InternalSpecialInteger T.SpecialInteger
    | InternalCodeTableRef CodeTableRef
    | InternalCharToken IntVal
    | InternalMathCharToken IntVal
    | InternalFontCharRef FontCharRef
    | LastPenalty
    | ParShape
    | InputLineNr
    | Badness
    deriving (Show)

data CodeTableRef = CodeTableRef T.CodeType Number
    deriving (Show)

data FontCharRef = FontCharRef T.FontChar FontRef
    deriving (Show)

data FontRef
    = FontTokenRef Int
    | CurrentFontRef
    | FamilyMemberFontRef FamilyMember
    deriving (Show)

data FamilyMember = FamilyMember T.FontRange Number
    deriving (Show)

data BoxDimensionRef = BoxDimensionRef Number TypoDim
    deriving (Show)

data FontDimensionRef = FontDimensionRef Number FontRef
    deriving (Show)

data InternalLength
    = InternalLengthVariable LengthVariable
    | InternalSpecialLength T.SpecialLength
    | InternalFontDimensionRef FontDimensionRef
    | InternalBoxDimensionRef BoxDimensionRef
    | LastKern
    deriving (Show)

data InternalGlue
    = InternalGlueVariable GlueVariable
    | LastGlue
    deriving (Show)

data InternalMathGlue
    = InternalMathGlueVariable MathGlueVariable
    | LastMathGlue
    deriving (Show)

-- Assignments.

data Assignment = Assignment
  { body   :: AssignmentBody
  , global :: T.GlobalFlag
  } deriving (Show)

data ControlSequenceTarget
    = MacroTarget T.MacroContents
    | LetTarget Lex.Token
    | FutureLetTarget Lex.Token Lex.Token
    | ShortDefineTarget T.QuantityType Number
    | ReadTarget Number
    | FontTarget FontSpecification (Path Rel File)
    deriving (Show)

data AssignmentBody
    = DefineControlSequence Lex.ControlSequenceLike ControlSequenceTarget
    | SetVariable VariableAssignment
    | ModifyVariable VariableModification
    | AssignCode CodeAssignment
    | SelectFont IntVal
    | SetFamilyMember FamilyMember FontRef
    | SetParShape [(Length, Length)]
    | SetBoxRegister Number Box
    -- -- Global assignments.
    | SetFontDimension FontDimensionRef Length
    | SetFontChar FontCharRef Number
    | SetHyphenation T.BalancedText
    | SetHyphenationPatterns T.BalancedText
    | SetBoxDimension BoxDimensionRef Length
    | SetInteractionMode T.InteractionMode
    | SetSpecialInteger T.SpecialInteger Number
    | SetSpecialLength T.SpecialLength Length
    deriving (Show)

data TokenListAssignmentTarget
    = TokenListAssignmentVar TokenListVariable
    | TokenListAssignmentText T.BalancedText
    deriving (Show)

data VariableAssignment
    = IntegerVariableAssignment IntegerVariable Number
    | LengthVariableAssignment LengthVariable Length
    | GlueVariableAssignment GlueVariable Glue
    | MathGlueVariableAssignment MathGlueVariable MathGlue
    | TokenListVariableAssignment TokenListVariable TokenListAssignmentTarget
    deriving (Show)

data VariableModification
    = AdvanceIntegerVariable IntegerVariable Number
    | AdvanceLengthVariable LengthVariable Length
    | AdvanceGlueVariable GlueVariable Glue
    | AdvanceMathGlueVariable MathGlueVariable MathGlue
    | ScaleVariable VDirection NumericVariable Number
    deriving (Show)

data NumericVariable
    = IntegerNumericVariable IntegerVariable
    | LengthNumericVariable LengthVariable
    | GlueNumericVariable GlueVariable
    | MathGlueNumericVariable MathGlueVariable
    deriving (Show)

data CodeAssignment = CodeAssignment CodeTableRef Number
    deriving (Show)

data FontSpecification
    = NaturalFont
    | FontAt Length
    | FontScaled Number
    deriving (Show)

-- Box specification.

data Box
    = FetchedRegisterBox T.BoxFetchMode Number
    | LastBox
    | VSplitBox Number Length
    -- \| ExplicitBox BoxSpecification ExplicitBox
    deriving (Show)

data BoxSpecification
    = Natural
    | To Length
    | Spread Length
    deriving (Show)

-- data ExplicitBox
--     = ExplicitHBox HModeMaterial
--     | ExplicitVBox VModeMaterial VBoxAlignType
--     deriving (Show)

data VBoxAlignType
    = DefaultAlign
    | TopAlign
    deriving (Show)

data BoxOrRule
    = BoxOrRuleBox Box
    | BoxOrRuleRule Rule
    deriving (Show)

-- Commands.

data AllModesCommand
    = ShowToken Lex.Token
    | ShowBox Number
    | ShowLists
    | ShowTheInternalQuantity InternalQuantity
    | ShipOut Box
    | SetAfterAssignmentToken Lex.Token
    | AddToAfterGroupTokens Lex.Token
    | AddMark T.BalancedText
    -- -- Note: this *is* an all-modes command. It can happen in non-vertical modes,
    -- -- then can 'migrate' out.
    -- \| AddInsertion Number VModeMaterial
    -- \| AddAdjustment VModeMaterial
    | AddSpace
    | AddBox BoxPlacement Box
    | StartParagraph T.IndentFlag
    | EndParagraph
    | AddLeaders T.LeadersType BoxOrRule Glue
    | AddUnwrappedFetchedBox Number T.BoxFetchMode -- \un{v,h}{box,copy}
    | AddRule Rule
    -- \| AddAlignedMaterial DesiredLength AlignmentMaterial
    | ModeIndependentCommand ModeIndependentCommand
    deriving (Show)

data ModeIndependentCommand
    = Assign Assignment
    | ChangeScope T.Sign CommandTrigger
    | Relax
    | IgnoreSpaces
    | AddPenalty Number
    | AddKern Length
    | AddMathKern MathLength
    | RemoveItem T.RemovableItem
    | AddGlue Glue
    | Message T.MessageStream T.ExpandedBalancedText
    | ModifyFileStream FileStreamType FileStreamAction Number
    | WriteToStream Number WriteText
    | DoSpecial T.ExpandedBalancedText
    deriving (Show)

data VModeCommand
    = VAllModesCommand AllModesCommand
    | End
    | Dump
    | EnterHMode
    deriving (Show)

data HModeCommand
    = HAllModesCommand AllModesCommand
    | AddControlSpace
    | AddCharacter CharCodeRef
    | AddAccentedCharacter Number [Assignment] (Maybe CharCodeRef)
    | AddItalicCorrection
    | AddDiscretionaryText { preBreak, postBreak, noBreak :: T.BalancedText }
    | AddDiscretionaryHyphen
    | EnterMathMode
    | LeaveHMode
    deriving (Show)

data CommandTrigger
    = CharCommandTrigger
    | CSCommandTrigger
    deriving (Show, Eq)

data InternalQuantity
    = InternalIntegerQuantity InternalInteger
    | InternalLengthQuantity InternalLength
    | InternalGlueQuantity InternalGlue
    | InternalMathGlueQuantity InternalMathGlue
    | FontQuantity FontRef
    | TokenListVariableQuantity TokenListVariable
    deriving (Show)

data WriteText
    = ImmediateWriteText T.ExpandedBalancedText
    | DeferredWriteText T.BalancedText
    deriving (Show)

data WritePolicy
    = Immediate
    | Deferred
    deriving (Show)

data Rule = Rule
    { width, height, depth :: Maybe Length }
    deriving (Show)

data FileStreamAction
    = Open (Path Rel File)
    | Close
    deriving (Show)

data FileStreamType
    = FileInput
    | FileOutput WritePolicy
    deriving (Show)

data BoxPlacement
    = NaturalPlacement
    | ShiftedPlacement Direction Length
    deriving (Show)

data CharCodeRef
    = CharRef CharCode
    | CharTokenRef IntVal
    | CharCodeNrRef Number
    deriving (Show)

-- Condition heads.

data IfConditionHead
    = IfIntegerPairTest Number Ordering Number -- \ifnum
    | IfLengthPairTest Length Ordering Length -- \ifdim
    | IfIntegerOdd Number -- \ifodd
    | IfInMode T.ModeAttribute -- \ifvmode, \ifhmode, \ifmmode, \ifinner
    | IfTokenAttributesEqual T.TokenAttribute T.PrimitiveToken T.PrimitiveToken -- \if, \ifcat
    | IfTokensEqual Lex.Token Lex.Token -- \ifx
    | IfBoxRegisterIs T.BoxRegisterAttribute Number -- \ifvoid, \ifhbox, \ifvbox
    | IfInputEnded Number -- \ifeof
    | IfConst Bool -- \iftrue, \iffalse
    deriving (Show)

data ConditionHead
    = IfConditionHead IfConditionHead
    | CaseConditionHead Number
    deriving (Show)
