{-# LANGUAGE DuplicateRecordFields #-}

module HeX.Parse.AST where

import           Path                           ( File
                                                , Path
                                                , Rel
                                                )

import           HeX.Concept
import           HeX.Type
import qualified HeX.BreakList                 as BL
import           HeX.Categorise                 ( CharCode )
import qualified HeX.Lex                       as Lex
import           HeX.Unit                       ( PhysicalUnit(..) )
import qualified HeX.Parse.Token               as T

-- Number.

data Number = Number Sign UnsignedNumber
    deriving (Show)

-- mconcat on this newtype wrapper should get the final sign of a list of
-- signs. Bit pretentious, sorry.
newtype Sign = Sign { getSign :: Bool }
    deriving (Show, Eq)

instance Semigroup Sign where
    Sign x <> Sign y = Sign $ x == y

instance Monoid Sign where
    mempty = Sign True

data UnsignedNumber
    = NormalIntegerAsUNumber NormalInteger
    | CoercedInteger CoercedInteger
    deriving (Show)

-- Think: 'un-coerced integer'.
data NormalInteger
    = IntegerConstant Int
    | InternalInteger InternalInteger
    deriving (Show)

data CoercedInteger
    = InternalLengthAsInt InternalLength
    | InternalGlueAsInt InternalGlue
    deriving (Show)

-- Length.

data Length = Length Sign UnsignedLength
    deriving (Show)

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

data Unit
    = PhysicalUnit PhysicalUnitFrame PhysicalUnit
    | InternalUnit InternalUnit
    deriving (Show)

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

data MathLength = MathLength Sign UnsignedMathLength
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
    | InternalGlue Sign InternalGlue
    deriving (Show)

data Flex
    = FiniteFlex Length
    | FilFlex FilLength
    deriving (Show)

data FilLength = FilLength Sign Factor Int
    deriving (Show)

-- Math glue.

data MathGlue
    = ExplicitMathGlue MathLength (Maybe MathFlex) (Maybe MathFlex)
    | InternalMathGlue Sign InternalMathGlue
    deriving (Show)

data MathFlex
    = FiniteMathFlex MathLength
    | FilMathFlex FilLength
    deriving (Show)

-- Internal quantities.

data QuantVariable a v
    = ParamVar a
    | TokenVar v
    | RegisterVar Number
    deriving (Show)

type IntegerVariable = QuantVariable T.IntegerParameter IntVal
type LengthVariable = QuantVariable T.LengthParameter LenVal
type GlueVariable = QuantVariable T.GlueParameter BL.Glue
-- TODO: What does a MathGlue evaluate to? Maybe not a BreakList Glue.
type MathGlueVariable = QuantVariable T.MathGlueParameter BL.Glue
type TokenListVariable = QuantVariable T.TokenListParameter [Lex.Token]

data InternalInteger
    = InternalIntegerVariable IntegerVariable
    | InternalSpecialInteger T.SpecialInteger
    | InternalCodeTableRef CodeTableRef
    | InternalCharToken CharCode
    | InternalMathCharToken CharCode
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

data AssignmentBody
    = DefineMacro MacroAssignment
    | SetVariable VariableAssignment
    | ModifyVariable VariableModification
    | AssignCode CodeAssignment
    | Let Lex.ControlSequenceLike Lex.Token
    | FutureLet Lex.ControlSequenceLike Lex.Token Lex.Token
    | ShortDefine T.QuantityType Lex.ControlSequenceLike Number
    | SelectFont IntVal
    | SetFamilyMember FamilyMember FontRef
    | SetParShape [(Length, Length)]
    | ReadToControlSequence Number Lex.ControlSequenceLike
    | SetBoxRegister Number Box
    | DefineFont Lex.ControlSequenceLike FontSpecification (Path Rel File)
    -- -- Global assignments.
    | SetFontDimension FontDimensionRef Length
    | SetFontChar FontCharRef Number
    | SetHyphenation BalancedText
    | SetHyphenationPatterns BalancedText
    | SetBoxDimension BoxDimensionRef Length
    | SetInteractionMode T.InteractionMode
    | SetSpecialInteger T.SpecialInteger Number
    | SetSpecialLength T.SpecialLength Length
    deriving (Show)

data MacroAssignment = MacroAssignment
    { name        :: Lex.ControlSequenceLike
    , contents    :: T.MacroContents
    , long, outer :: Bool
    } deriving (Show)

data VariableAssignment
    = IntegerVariableAssignment IntegerVariable Number
    | LengthVariableAssignment LengthVariable Length
    | GlueVariableAssignment GlueVariable Glue
    | MathGlueVariableAssignment MathGlueVariable MathGlue
    | TokenListVariableAssignmentVar TokenListVariable TokenListVariable
    | TokenListVariableAssignmentText TokenListVariable BalancedText
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
    = LeftBrace
    | RightBrace
    | BeginGroup
    | EndGroup
    | ShowToken Lex.Token
    | ShowBox Number
    | ShowLists
    -- \| ShowInternalQuantity InternalQuantity
    | ShipOut Box
    | SetAfterAssignmentToken Lex.Token
    | AddToAfterGroupTokens Lex.Token
    | Message T.MessageStream BalancedText
    | ModifyFileStream Number FileStreamAction FileStreamType
    | WriteToStream Number BalancedText WritePolicy
    | AddWhatsit BalancedText
    | RemoveLastPenalty
    | RemoveLastKern
    | RemoveLastGlue
    | AddMark BalancedText
    -- -- Note: this *is* an all-modes command. It can happen in non-vertical modes,
    -- -- then can 'migrate' out.
    -- \| AddInsertion Number VModeMaterial
    | AddLeaders T.LeadersType BoxOrRule Glue
    | AddSpace
    | AddBox BoxPlacement Box
    | AddUnwrappedFetchedBox Number T.BoxFetchMode -- \un{v,h}{box,copy}
    | AddRule Rule
    -- \| AddAlignedMaterial DesiredLength AlignmentMaterial
    | StartParagraph T.IndentFlag
    | EndParagraph
    | ModeIndependentCommand ModeIndependentCommand
    deriving (Show)

data ModeIndependentCommand
    = Assign Assignment
    | Relax
    | IgnoreSpaces
    | AddPenalty Number
    | AddKern Length
    | AddMathKern MathLength
    | AddGlue Glue
    deriving (Show)

data VModeCommand
    = VAllModesCommand AllModesCommand
    | EnterHMode
    | End
    | Dump
    deriving (Show)

data HModeCommand
    = HAllModesCommand AllModesCommand
    | LeaveHMode
    | EnterMathMode
    -- \| AddAdjustment VModeMaterial
    | AddControlSpace
    | AddCharacter CharCodeRef
    | AddAccentedCharacter { accentCode :: Number, targetCode :: Maybe Number, assignments :: [Assignment]}
    | AddItalicCorrection
    | AddDiscretionaryText { preBreak, postBreak, noBreak :: BalancedText }
    deriving (Show)

data WritePolicy
    = Immediate
    | Deferred
    deriving (Show)

newtype BalancedText = BalancedText [Lex.Token]
    deriving (Show, Eq)

data Rule = Rule
    { width, height, depth :: Maybe Length }
    deriving (Show)

data FileStreamAction
    = Open String
    | Close
    deriving (Show)

data FileStreamType
    = FileInput
    | FileOutput WritePolicy
    deriving (Show)

data BoxPlacement
    = NaturalPlacement
    | ShiftedPlacement Length
    deriving (Show)

data CharCodeRef
    = CharRef CharCode
    | CharTokenRef CharCode
    | CharCodeNrRef Number
    deriving (Show)
