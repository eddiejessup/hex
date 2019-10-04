module HeX.Evaluate where

import           HeXlude

import           Control.Monad.Reader (MonadReader, ask, asks)
import           Data.Char            (chr)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.Sequence        as Seq

import qualified TFM

import qualified HeX.Box              as B
import qualified HeX.BreakList        as BL
import qualified HeX.Categorise       as Cat
import           HeX.Categorise       (CharCode)
import           HeX.Config
import qualified HeX.Lex              as Lex
import qualified HeX.Parse.AST        as AST
import qualified HeX.Parse.Token      as T
import           HeX.Quantity


class TeXEvaluable a where
    type EvalTarget a

    texEvaluate
        :: ( MonadReader Config m
           , MonadErrorAnyOf e m '[EvaluationError, ConfigError]
           )
        => a
        -> m (EvalTarget a)

newtype EvaluationError
    = EvaluationError Text
    deriving (Show)


instance TeXEvaluable AST.TokenListAssignmentTarget where
    type EvalTarget AST.TokenListAssignmentTarget = T.BalancedText

    texEvaluate = \case
        AST.TokenListAssignmentVar tgtVar ->
            texEvaluate tgtVar
        AST.TokenListAssignmentText tgtText ->
            pure tgtText

-- TeXInt.

signedTeXEval
    :: (TeXEvaluable a
       , MonadReader Config m
       , MonadErrorAnyOf e m '[EvaluationError, ConfigError]
       , Num (EvalTarget a)
       )
    => T.Signed a
    -> m (EvalTarget a)
signedTeXEval (T.Signed sign u) =
    do
    eu <- texEvaluate u
    pure $ T.evalSigned (T.Signed sign eu)

instance TeXEvaluable AST.TeXInt where
    type EvalTarget AST.TeXInt = TeXIntVal

    texEvaluate = signedTeXEval

instance TeXEvaluable AST.UnsignedTeXInt where
    type EvalTarget AST.UnsignedTeXInt = Int

    texEvaluate = \case
        AST.NormalTeXIntAsUTeXInt v -> texEvaluate v
        AST.CoercedTeXInt v -> texEvaluate v

instance TeXEvaluable AST.NormalTeXInt where
    type EvalTarget AST.NormalTeXInt = Int

    texEvaluate = \case
        AST.TeXIntConstant n -> pure n
        AST.InternalTeXInt v -> texEvaluate v

instance TeXEvaluable AST.InternalTeXInt where
    type EvalTarget AST.InternalTeXInt = Int

    texEvaluate = \case
        AST.InternalTeXIntVariable v -> texEvaluate v
        AST.InternalSpecialTeXInt v  -> texEvaluate v
        AST.InternalCodeTableRef v    -> texEvaluate v
        AST.InternalCharToken n       -> pure n
        AST.InternalMathCharToken n   -> pure n
        AST.InternalFontCharRef v     -> texEvaluate v
        AST.LastPenalty               -> panic "Not implemented: evaluate LastPenalty"
        AST.ParShape                  -> panic "Not implemented: evaluate ParShape"
        AST.InputLineNr               -> panic "Not implemented: evaluate InputLineNr"
        AST.Badness                   -> panic "Not implemented: evaluate Badness"

instance TeXEvaluable AST.EightBitTeXInt where
    type EvalTarget AST.EightBitTeXInt = EightBitInt

    texEvaluate (AST.EightBitTeXInt n) =
        texEvaluate n >>= toEighBit
      where
        toEighBit i = liftMaybe (throw (EvaluationError ("TeXInt not in range: " <> show i))) (newEightBitInt i)

getRegisterIdx
    :: ( MonadReader Config m
       , MonadErrorAnyOf e m '[EvaluationError, ConfigError]
       )
    => AST.EightBitTeXInt
    -> (EightBitInt -> Config -> a)
    -> m a
getRegisterIdx n f =
    do
    en <- texEvaluate n
    asks $ f en

instance TeXEvaluable AST.TeXIntVariable where
    type EvalTarget AST.TeXIntVariable = Int

    texEvaluate = \case
        AST.ParamVar p -> asks $ lookupTeXIntParameter p
        AST.RegisterVar n -> getRegisterIdx n lookupTeXIntRegister

instance TeXEvaluable T.SpecialTeXInt where
    type EvalTarget T.SpecialTeXInt = Int

    texEvaluate p = asks $ lookupSpecialTeXInt p

instance TeXEvaluable AST.CodeTableRef where
    type EvalTarget AST.CodeTableRef = Int

    texEvaluate (AST.CodeTableRef q n) =
        do
        idx <- chr <$> texEvaluate n
        conf <- ask
        let
            lookupFrom :: Enum v => (Scope -> HashMap Char v) -> Maybe Int
            lookupFrom getMap = fromEnum <$> scopedMapLookup getMap idx conf
        liftMaybe (throw (EvaluationError "err")) $ case q of
            T.CategoryCodeType            -> lookupFrom catCodes
            T.MathCodeType                -> lookupFrom mathCodes
            T.ChangeCaseCodeType Upward   -> lookupFrom uppercaseCodes
            T.ChangeCaseCodeType Downward -> lookupFrom lowercaseCodes
            T.SpaceFactorCodeType         -> lookupFrom spaceFactors
            T.DelimiterCodeType           -> lookupFrom delimiterCodes

instance TeXEvaluable AST.FontCharRef where
    type EvalTarget AST.FontCharRef = Int

    texEvaluate (AST.FontCharRef fChar fontRef) =
        do
        fontInfo <- texEvaluate fontRef >>= lookupFontInfo
        pure $ case fChar of
            T.HyphenChar -> hyphenChar fontInfo
            T.SkewChar   -> skewChar fontInfo

instance TeXEvaluable AST.FontRef where
    type EvalTarget AST.FontRef = Int

    texEvaluate = \case
        AST.FontTokenRef fNr -> pure fNr
        AST.CurrentFontRef -> mLookupCurrentFontNr
        AST.FamilyMemberFontRef v -> texEvaluate v >>= lookupFontFamilyMember

instance TeXEvaluable AST.FamilyMember where
    type EvalTarget AST.FamilyMember = (T.FontRange, Int)
    texEvaluate (AST.FamilyMember rng n) = (rng,) <$> texEvaluate n

instance TeXEvaluable AST.CoercedTeXInt where
    type EvalTarget AST.CoercedTeXInt = Int

    texEvaluate = \case
        AST.InternalLengthAsInt ln -> unLength <$> texEvaluate ln
        AST.InternalGlueAsInt g -> (unLength . BL.dimen) <$> texEvaluate g

-- Length.

instance TeXEvaluable AST.Length where
    type EvalTarget AST.Length = TeXLength

    texEvaluate = signedTeXEval

instance TeXEvaluable AST.UnsignedLength where
    type EvalTarget AST.UnsignedLength = TeXLength

    texEvaluate = \case
        AST.NormalLengthAsULength v -> texEvaluate v
        AST.CoercedLength v -> texEvaluate v

instance TeXEvaluable AST.NormalLength where
    type EvalTarget AST.NormalLength = TeXLength

    texEvaluate = \case
        AST.LengthSemiConstant f u ->
            do
            ef <- texEvaluate f
            eu <- texEvaluate u
            pure $ round $ ef * eu
        AST.InternalLength v ->
            texEvaluate v

instance TeXEvaluable AST.Factor where
    type EvalTarget AST.Factor = Rational

    texEvaluate = \case
        AST.NormalTeXIntFactor n -> fromIntegral <$> texEvaluate n
        AST.RationalConstant r -> pure r

instance TeXEvaluable AST.Unit where
    type EvalTarget AST.Unit = Rational

    texEvaluate = \case
        AST.InternalUnit u -> texEvaluate u
        AST.PhysicalUnit AST.TrueFrame u -> pure $ inScaledPoint u
        AST.PhysicalUnit AST.MagnifiedFrame u ->
            do
            _mag <- asks $ lookupTeXIntParameter T.Mag
            eU <- texEvaluate $ AST.PhysicalUnit AST.TrueFrame u
            pure $ eU * 1000 / fromIntegral _mag

instance TeXEvaluable AST.InternalUnit where
    type EvalTarget AST.InternalUnit = Rational

    texEvaluate = \case
        AST.Em -> TFM.quad . fontMetrics <$> currentFontInfo
        AST.Ex -> TFM.xHeight . fontMetrics <$> currentFontInfo
        AST.InternalTeXIntUnit v -> fromIntegral <$> texEvaluate v
        AST.InternalLengthUnit v -> fromIntegral <$> texEvaluate v
        AST.InternalGlueUnit v -> fromIntegral . BL.dimen <$> texEvaluate v

instance TeXEvaluable AST.InternalLength where
    type EvalTarget AST.InternalLength = TeXLength

    texEvaluate = \case
        AST.InternalLengthVariable v -> texEvaluate v
        AST.InternalSpecialLength v -> texEvaluate v
        AST.InternalFontDimensionRef v -> texEvaluate v
        AST.InternalBoxDimensionRef v -> texEvaluate v
        AST.LastKern -> panic "Not implemented: evaluate LastKern"

instance TeXEvaluable AST.LengthVariable where
    type EvalTarget AST.LengthVariable = TeXLength

    texEvaluate = \case
        AST.ParamVar p -> asks $ lookupLengthParameter p
        AST.RegisterVar n -> getRegisterIdx n lookupLengthRegister

instance TeXEvaluable T.SpecialLength where
    type EvalTarget T.SpecialLength = TeXLength

    texEvaluate p = asks $ lookupSpecialLength p

instance TeXEvaluable AST.FontDimensionRef where
    type EvalTarget AST.FontDimensionRef = TeXLength

    texEvaluate (AST.FontDimensionRef n _) =
        do
        _ <- texEvaluate n
        panic "Not implemented: evaluate FontDimensionRef"

instance TeXEvaluable AST.BoxDimensionRef where
    type EvalTarget AST.BoxDimensionRef = TeXLength

    texEvaluate (AST.BoxDimensionRef idx boxDim) =
        do
        eIdx <- texEvaluate idx
        asks (lookupBoxRegister eIdx)
        <&> maybe 0 (naturalLength boxDim)

instance TeXEvaluable AST.CoercedLength where
    type EvalTarget AST.CoercedLength = TeXLength
    texEvaluate (AST.InternalGlueAsLength g) = BL.dimen <$> texEvaluate g

-- Math length.

instance TeXEvaluable AST.MathLength where
    type EvalTarget AST.MathLength = MathLength

    texEvaluate = signedTeXEval

instance TeXEvaluable AST.UnsignedMathLength where
    type EvalTarget AST.UnsignedMathLength = MathLength

    texEvaluate = \case
        AST.NormalMathLengthAsUMathLength v -> texEvaluate v
        AST.CoercedMathLength v -> texEvaluate v

instance TeXEvaluable AST.NormalMathLength where
    type EvalTarget AST.NormalMathLength = MathLength

    texEvaluate (AST.MathLengthSemiConstant f mathU) =
        do
        ef <- texEvaluate f
        eu <- texEvaluate mathU
        pure $ round $ ef * fromIntegral eu

instance TeXEvaluable AST.MathUnit where
    type EvalTarget AST.MathUnit = MathLength

    texEvaluate = \case
        AST.Mu ->
            pure 1
        AST.InternalMathGlueAsUnit mg ->
            BL.dimen <$> texEvaluate mg

instance TeXEvaluable AST.CoercedMathLength where
    type EvalTarget AST.CoercedMathLength = MathLength
    texEvaluate (AST.InternalMathGlueAsMathLength mg) = BL.dimen <$> texEvaluate mg

-- Glue.

instance TeXEvaluable AST.Glue where
    type EvalTarget AST.Glue = BL.Glue TeXLength

    texEvaluate = \case
        AST.ExplicitGlue dim str shr ->
            BL.Glue <$> texEvaluate dim <*> evalFlex str <*> evalFlex shr
        AST.InternalGlue (T.Signed sign v) ->
            do
            ev <- texEvaluate v
            pure $ case sign of
                T.Positive -> ev
                T.Negative -> BL.negateGlue ev
      where
        evalFlex = \case
            Just f -> texEvaluate f
            Nothing -> pure BL.noFlex

instance TeXEvaluable AST.Flex where
    type EvalTarget AST.Flex = BL.GlueFlex

    texEvaluate = \case
        AST.FiniteFlex ln ->
            do
            eLn <- texEvaluate ln
            pure BL.GlueFlex { BL.factor = fromIntegral eLn, BL.order = 0 }
        AST.FilFlex ln -> texEvaluate ln

instance TeXEvaluable AST.FilLength where
    type EvalTarget AST.FilLength = BL.GlueFlex

    texEvaluate (AST.FilLength fl filOrder) =
        do
        efl <- signedTeXEval fl
        pure BL.GlueFlex { BL.factor = efl, BL.order = filOrder }

instance TeXEvaluable AST.InternalGlue where
    type EvalTarget AST.InternalGlue = BL.Glue TeXLength

    texEvaluate = \case
        AST.InternalGlueVariable v -> texEvaluate v
        AST.LastGlue -> panic "Not implemented: evaluate LastGlue"

instance TeXEvaluable AST.GlueVariable where
    type EvalTarget AST.GlueVariable = BL.Glue TeXLength

    texEvaluate = \case
        AST.ParamVar p    -> asks $ lookupGlueParameter p
        AST.RegisterVar n -> getRegisterIdx n lookupGlueRegister

-- Math glue.

instance TeXEvaluable AST.MathGlue where
    type EvalTarget AST.MathGlue = BL.Glue MathLength

    texEvaluate = \case
        AST.ExplicitMathGlue mDim mStr mShr ->
            BL.Glue <$> texEvaluate mDim <*> evalMathFlex mStr <*> evalMathFlex mShr
        AST.InternalMathGlue sign v ->
            do
            ev <- texEvaluate v
            pure $ case sign of
                T.Positive -> ev
                T.Negative -> BL.negateGlue ev
      where
        evalMathFlex = \case
            Just f -> texEvaluate f
            Nothing -> pure BL.noFlex

instance TeXEvaluable AST.MathFlex where
    type EvalTarget AST.MathFlex = BL.GlueFlex

    texEvaluate = \case
        AST.FiniteMathFlex ln ->
            do
            eLn <- texEvaluate ln
            pure BL.GlueFlex { BL.factor = fromIntegral eLn, BL.order = 0 }
        AST.FilMathFlex ln -> texEvaluate ln

instance TeXEvaluable AST.InternalMathGlue where
    type EvalTarget AST.InternalMathGlue = BL.Glue MathLength

    texEvaluate = \case
        AST.InternalMathGlueVariable v -> texEvaluate v
        AST.LastMathGlue -> panic "Not implemented: evaluate LastMathGlue"

instance TeXEvaluable AST.MathGlueVariable where
    type EvalTarget AST.MathGlueVariable = BL.Glue MathLength

    texEvaluate = \case
        AST.ParamVar p    -> asks $ lookupMathGlueParameter p
        AST.RegisterVar n -> getRegisterIdx n lookupMathGlueRegister

-- Token list.

instance TeXEvaluable AST.TokenListVariable where
    type EvalTarget AST.TokenListVariable = T.BalancedText

    texEvaluate = \case
        AST.ParamVar p    -> asks $ lookupTokenListParameter p
        AST.RegisterVar n -> getRegisterIdx n lookupTokenListRegister

-- Showing internal quantities.

-- For \number, \romannumeral, \string. \meaning, \jobname, and \fontname: Each
-- character code gets category "other" , except that 32 gets "space".
charCodeAsMadeToken :: CharCode -> Lex.Token
charCodeAsMadeToken c =
    Lex.CharCatToken $ Lex.CharCat c $ case c of
        ' ' -> Cat.Space
        _   -> Cat.Other

instance TeXEvaluable AST.InternalQuantity where
    type EvalTarget AST.InternalQuantity = Seq CharCode

    texEvaluate = \case
        AST.InternalTeXIntQuantity n ->
            do
            en <- texEvaluate n
            pure $ Seq.fromList $ show en
        AST.InternalLengthQuantity d ->
            do
            _ <- texEvaluate d
            panic "Not implemented: evaluate InternalLengthQuantity"
        AST.InternalGlueQuantity g ->
            do
            _ <- texEvaluate g
            panic "Not implemented: evaluate InternalGlueQuantity"
        AST.InternalMathGlueQuantity mg ->
            do
            _ <- texEvaluate mg
            panic "Not implemented: evaluate InternalMathGlueQuantity"
        AST.FontQuantity f ->
            do
            _ <- texEvaluate f
            panic "Not implemented: evaluate FontQuantity"
        AST.TokenListVariableQuantity tl ->
            do
            _ <- texEvaluate tl
            panic "Not implemented: evaluate TokenListVariableQuantity"

-- Condition

data IfBodyState
    = IfPreElse
    | IfPostElse
    deriving (Show, Eq)

data ConditionBlockTarget
    = IfBlockTarget IfBodyState
    | CaseBlockTarget Int
    deriving (Show)

data CaseBodyState
    = CasePostOr
    | CasePostElse
    deriving (Show)

data ConditionBodyState
    = IfBodyState IfBodyState
    | CaseBodyState CaseBodyState
    deriving (Show)

instance TeXEvaluable AST.ConditionHead where
    type EvalTarget AST.ConditionHead = ConditionBlockTarget

    texEvaluate = \case
        AST.CaseConditionHead n ->
            do
            en <- texEvaluate n
            pure $ CaseBlockTarget en
        AST.IfConditionHead ifH ->
            do
            ifResult <- texEvaluate ifH
            pure $ IfBlockTarget $ if ifResult
                then IfPreElse
                else IfPostElse

instance TeXEvaluable AST.IfConditionHead where
    type EvalTarget AST.IfConditionHead = Bool

    texEvaluate = \case
        AST.IfTeXIntPairTest n1 ordering n2 ->
            do
            en1 <- texEvaluate n1
            en2 <- texEvaluate n2
            pure $ ordToComp ordering en1 en2
        AST.IfLengthPairTest d1 ordering d2 ->
            do
            ed1 <- texEvaluate d1
            ed2 <- texEvaluate d2
            pure $ ordToComp ordering ed1 ed2
        AST.IfTeXIntOdd n ->
            do
            en <- texEvaluate n
            pure $ en `mod` 2 == 1
        AST.IfInMode _ ->
            panic "Not implemented: IfInMode"
        -- A control sequence token is considered to have character code 256 and
        -- category code 16.
        -- This logic is hard to follow literally, because my category codee type
        -- is an explicit enumeration, not an integer. So I'll just interpret it
        -- as: control sequences are considered equal to each other, and unequal to
        -- all char-cat pairs.
        -- TODO: Unless the control sequence has been \let equal to a non-active
        -- character token.
        AST.IfTokenAttributesEqual T.CharCodeAttribute t1 t2 ->
            pure $ eqChars t1 t2
        AST.IfTokenAttributesEqual T.CatCodeAttribute t1 t2 ->
            pure $ eqCats t1 t2
        --  The condition is true if (a) the two tokens are not macros, and they
        --  both represent the same (character code, category code) pair, the same
        --  TeX primitive, the same \font or \chardef or \countdef, etc.; or if (b)
        --  the two tokens are macros, and they both have the same status with
        --  respect to \long and \outer, and they both have the same parameters and
        --  “top level” expansion.
        AST.IfTokensEqual (Lex.CharCatToken cc1) (Lex.CharCatToken cc2) ->
            pure $ cc1 == cc2
        AST.IfTokensEqual (Lex.ControlSequenceToken cs1) (Lex.ControlSequenceToken cs2) ->
            do
            conf <- ask
            let lkp cs = lookupCSProper cs conf
            -- Surprisingly, two undefined control sequences are considered equal,
            -- so we may compare the Maybe types.
            -- The 'Just' values are arranged so that I think their naïve
            -- comparison gives the desired behaviour.
            pure $ lkp cs1 == lkp cs2
        AST.IfTokensEqual _ _ ->
            pure False
        AST.IfBoxRegisterIs attr n ->
            do
            _ <- texEvaluate n
            case attr of
                T.HasVerticalBox ->
                    panic "Not implemented: evaluate IfBoxRegister HasVerticalBox"
                T.HasHorizontalBox ->
                    panic "Not implemented: evaluate IfBoxRegister HasHorizontalBox"
                T.IsVoid ->
                    panic "Not implemented: evaluate IfBoxRegister IsVoid"
        AST.IfInputEnded n ->
            do
            _ <- texEvaluate n
            panic "Not implemented: evaluate IfInputEnded"
        AST.IfConst b -> pure b
      where
        ordToComp GT = (>)
        ordToComp LT = (<)
        ordToComp EQ = (==)

        eqChars
            (T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c1 _)))
            (T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat c2 _))) = c1 == c2
        eqChars _ _ = True

        eqCats
            (T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat _ c1)))
            (T.UnexpandedTok (Lex.CharCatToken (Lex.CharCat _ c2))) = c1 == c2
        eqCats _ _ = True

-- Other.

instance TeXEvaluable AST.CharCodeRef where
    type EvalTarget AST.CharCodeRef = CharCode

    texEvaluate ref = case ref of
        AST.CharRef c       -> pure c
        AST.CharTokenRef c  -> pure $ chr c
        AST.CharCodeNrRef n -> chr <$> texEvaluate n

instance TeXEvaluable AST.BoxSpecification where
    type EvalTarget AST.BoxSpecification = B.DesiredLength

    texEvaluate = \case
        AST.Natural -> pure B.Natural
        AST.To ln -> B.To <$> texEvaluate ln
        AST.Spread ln -> B.Spread <$> texEvaluate ln

evaluateFontSpecification
    :: ( MonadReader Config m
       , MonadErrorAnyOf e m '[EvaluationError, ConfigError]
       )
    => TeXLength
    -> AST.FontSpecification
    -> m Rational
evaluateFontSpecification designSizeSP = \case
    AST.NaturalFont ->
        pure 1
    AST.FontAt ln ->
        do
        eLn <- texEvaluate ln
        pure $ fromIntegral eLn % fromIntegral designSizeSP
    AST.FontScaled n ->
        do
        en <- texEvaluate n
        pure $ fromIntegral en % 1000
