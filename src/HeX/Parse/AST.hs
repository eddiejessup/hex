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
import           HeX.Parse.Token.Parameter

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
    = MathLengthSemiConstant Factor Unit
    deriving (Show)

data MathUnit
    = Mu
    | InternalMathGlueAsUnit InternalMathGlue
    deriving (Show)

data CoercedMathLength
    = InternalGlueAsMathLength InternalMathGlue
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
    | InternalSpecialInteger SpecialInteger
    | InternalCodeTableRef CodeTableRef
    | InternalCharToken String
    | InternalMathCharToken String
    | InternalFontCharAttr FontCharAttr FontRef
    | LastPenalty
    | ParShape
    | InputLineNr
    | Badness
    deriving (Show)

data CodeTableRef = CodeTableRef T.CodeType Number
    deriving (Show)

data FontCharAttr
    = HyphenChar
    | SkewChar
    deriving (Show)

data FontRef
    = FontTokenRef String
    | CurrentFontRef
    | FamilyMemberFontRef FamilyMember
    deriving (Show)

data FamilyMember = FamilyMember FontRange Number
    deriving (Show)

data FontRange
    = TextFontRange
    | ScriptFontRange
    | ScriptScriptFontRange
    deriving (Show)

data InternalLength
    = InternalLengthVariable LengthVariable
    | InternalSpecialLength SpecialLength
    | InternalFontLengthAttr Number FontRef
    | InternalBoxDimension TypoDim Number
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

data QuantityType
    = CharQuantity
    | MathCharQuantity
    | IntegerQuantity
    | LengthQuantity
    | GlueQuantity
    | MathGlueQuantity
    | TokenListQuantity
    deriving (Show)

data AssignmentBody
    = DefineMacro MacroAssignment
    | SetVariable VariableAssignment
    | ModifyVariable VariableModification
    | AssignCode CodeAssignment
    | Let Lex.ControlSequenceLike Lex.Token
    | FutureLet Lex.ControlSequenceLike Lex.Token Lex.Token
    | ShortDefine QuantityType Lex.ControlSequenceLike Number
    | SelectFont IntVal
    | SetFamilyMember FamilyMember FontRef
    -- \| SetParShape ParShapeAssignment
    | ReadToControlSequence Number Lex.ControlSequenceLike
    | SetBoxRegister Number Box
    | DefineFont Lex.ControlSequenceLike FontSpecification (Path Rel File)
    -- -- Global assignments.
    | SetFontDimension Number FontRef Length
    | SetFontChar FontCharAttr FontRef Number
    | SetHyphenation BalancedText
    | SetHyphenationPatterns BalancedText
    | SetBoxSize TypoDim Number Length
    | SetInteractionMode InteractionMode
    | SetSpecialInteger SpecialInteger Number
    | SetSpecialLength SpecialLength Length
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
    deriving (Show)

data VariableModification
    = AdvanceInteger IntegerVariable Number
    | AdvanceLength LengthVariable Length
    | AdvanceGlue GlueVariable Glue
    | AdvanceMathGlue MathGlueVariable MathGlue
    | ScaleVariable ScaleOp NumberVariable Number
    deriving (Show)

data ScaleOp
    = Multiply
    | Divide
    deriving (Show)

data NumberVariable
    = IntegerNumberVariable IntegerVariable
    | LengthNumberVariable LengthVariable
    | GlueNumberVariable GlueVariable
    | MathGlueNumberVariable MathGlueVariable
    deriving (Show)

data CodeAssignment = CodeAssignment CodeTableRef Number
    deriving (Show)

data FontSpecification
    = NaturalFont
    | FontAt Length
    | FontScaled Number
    deriving (Show)

data InteractionMode
    = ErrorStopMode
    | ScrollMode
    | NonStopMode
    | BatchMode
    deriving (Show)

-- Box specification.

data Box
    = FetchedRegisterBox BoxFetchMode Number
    | LastBox
    -- \| VSplit
    -- \| ExplicitBox BoxSpecification ExplicitBox
    deriving (Show)

data BoxFetchMode
    = Pop
    | Lookup
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
    | AddUnwrappedFetchedBox Number BoxFetchMode -- \un{v,h}{box,copy}
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
