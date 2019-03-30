module HeX.Evaluate where

import HeXlude

import           Control.Monad.Except           ( MonadError
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , asks
                                                , ask
                                                )
import           Data.Char                      ( chr )
import           Data.HashMap.Strict            ( HashMap )

import qualified TFM


import qualified HeX.Lex                       as Lex
import qualified HeX.Box                       as B
import qualified HeX.BreakList                 as BL
import           HeX.Categorise                 ( CharCode )
import           HeX.Config
import qualified HeX.Parse.AST                 as AST
import qualified HeX.Parse.Token               as T
import qualified HeX.Unit                      as Unit

class TeXEvaluable a where
    type EvalTarget a

    texEvaluate
        :: (MonadReader Config m, MonadError Text m)
        => a
        -> m (EvalTarget a)

instance TeXEvaluable AST.Number where
    type EvalTarget AST.Number = IntVal

    texEvaluate = evaluateNumber

instance TeXEvaluable AST.Length where
    type EvalTarget AST.Length = LenVal

    texEvaluate = evaluateLength

instance TeXEvaluable AST.Glue where
    type EvalTarget AST.Glue = BL.Glue

    texEvaluate = evaluateGlue

instance TeXEvaluable AST.MathGlue where
    type EvalTarget AST.MathGlue = BL.MathGlue

    texEvaluate = evaluateMathGlue

instance TeXEvaluable AST.TokenListAssignmentTarget where
    type EvalTarget AST.TokenListAssignmentTarget = T.BalancedText

    texEvaluate = \case
        AST.TokenListAssignmentVar tgtVar ->
            evaluateTokenListVariable tgtVar
        AST.TokenListAssignmentText tgtText ->
            pure tgtText

instance TeXEvaluable AST.IntegerVariable where
    type EvalTarget AST.IntegerVariable = IntVal

    texEvaluate = evaluateIntegerVariable

instance TeXEvaluable AST.LengthVariable where
    type EvalTarget AST.LengthVariable = LenVal

    texEvaluate = evaluateLengthVariable

instance TeXEvaluable AST.GlueVariable where
    type EvalTarget AST.GlueVariable = BL.Glue

    texEvaluate = evaluateGlueVariable

instance TeXEvaluable AST.MathGlueVariable where
    type EvalTarget AST.MathGlueVariable = BL.MathGlue

    texEvaluate = evaluateMathGlueVariable

instance TeXEvaluable AST.TokenListVariable where
    type EvalTarget AST.TokenListVariable = T.BalancedText

    texEvaluate = evaluateTokenListVariable

-- Integer.

evaluateNumber :: (MonadReader Config m, MonadError Text m) => AST.Number -> m Int
evaluateNumber (AST.Number (T.Sign isPos) u) =
    do
    size <- evaluateUnsignedNumber u
    pure $ if isPos then size else (-size)

evaluateUnsignedNumber :: (MonadReader Config m, MonadError Text m) => AST.UnsignedNumber -> m Int
evaluateUnsignedNumber = \case
    AST.NormalIntegerAsUNumber v -> evaluateNormalInteger v
    AST.CoercedInteger v -> evaluateCoercedInteger v

evaluateNormalInteger :: (MonadReader Config m, MonadError Text m) => AST.NormalInteger -> m Int
evaluateNormalInteger = \case
    AST.IntegerConstant n -> pure n
    AST.InternalInteger v -> evaluateInternalInteger v

evaluateInternalInteger :: (MonadReader Config m, MonadError Text m) => AST.InternalInteger -> m Int
evaluateInternalInteger = \case
    AST.InternalIntegerVariable v -> evaluateIntegerVariable v
    AST.InternalSpecialInteger v  -> evaluateSpecialInteger v
    AST.InternalCodeTableRef v    -> evaluateCodeTableRef v
    AST.InternalCharToken n       -> pure n
    AST.InternalMathCharToken n   -> pure n
    AST.InternalFontCharRef v     -> evaluateFontCharRef v
    AST.LastPenalty               -> notImplemented
    AST.ParShape                  -> notImplemented
    AST.InputLineNr               -> notImplemented
    AST.Badness                   -> notImplemented

evaluateEightBitInt :: (MonadReader Config m,  MonadError Text m) => AST.Number -> m EightBitInt
evaluateEightBitInt n = evaluateNumber n >>= (\i -> liftMaybe ("Number not in range: " <> show i) $ newEightBitInt i)

getRegisterIdx
    :: (MonadReader Config m, MonadError Text m)
    => AST.Number
    -> (EightBitInt -> Config -> a)
    -> m a
getRegisterIdx n f =
    do
    en <- evaluateEightBitInt n
    asks $ f en

evaluateIntegerVariable :: (MonadReader Config m, MonadError Text m) => AST.IntegerVariable -> m Int
evaluateIntegerVariable = \case
    AST.ParamVar p -> asks $ lookupIntegerParameter p
    AST.RegisterVar n -> getRegisterIdx n lookupIntegerRegister

evaluateSpecialInteger :: (MonadReader Config m) => T.SpecialInteger -> m Int
evaluateSpecialInteger p = asks $ lookupSpecialInteger p

evaluateCodeTableRef :: (MonadReader Config m, MonadError Text m) => AST.CodeTableRef -> m Int
evaluateCodeTableRef (AST.CodeTableRef q n) =
    do
    idx <- chr <$> evaluateNumber n
    let lookupFrom
            :: (MonadReader Config m, MonadError Text m)
            => (Scope -> HashMap CharCode b) -> m b
        lookupFrom getMap = asks (scopedMapLookup getMap idx) >>= liftMaybe "err"
    case q of
        T.CategoryCodeType       -> fromEnum <$> lookupFrom catCodes
        T.MathCodeType           -> fromEnum <$> lookupFrom mathCodes
        T.ChangeCaseCodeType dir -> fromEnum <$> lookupFrom (case dir of
            Upward   -> uppercaseCodes
            Downward -> lowercaseCodes)
        T.SpaceFactorCodeType    -> fromEnum <$> lookupFrom spaceFactors
        T.DelimiterCodeType      -> fromEnum <$> lookupFrom delimiterCodes

evaluateFontCharRef :: (MonadReader Config m, MonadError Text m) => AST.FontCharRef -> m Int
evaluateFontCharRef (AST.FontCharRef fChar fontRef) =
    do
    fontInfo <- evaluateFontRef fontRef >>= lookupFontInfo
    pure $ case fChar of
        T.HyphenChar -> hyphenChar fontInfo
        T.SkewChar   -> skewChar fontInfo

evaluateFontRef :: (MonadReader Config m, MonadError Text m) => AST.FontRef -> m Int
evaluateFontRef = \case
    AST.FontTokenRef fNr -> pure fNr
    AST.CurrentFontRef -> mLookupCurrentFontNr
    AST.FamilyMemberFontRef v -> evaluateFamilyMember v >>= lookupFontFamilyMember

evaluateFamilyMember :: (MonadReader Config m, MonadError Text m) => AST.FamilyMember -> m (T.FontRange, Int)
evaluateFamilyMember (AST.FamilyMember rng n) = (rng,) <$> evaluateNumber n

evaluateCoercedInteger :: (MonadReader Config m, MonadError Text m) => AST.CoercedInteger -> m Int
evaluateCoercedInteger = \case
    AST.InternalLengthAsInt ln -> evaluateInternalLength ln
    AST.InternalGlueAsInt g -> BL.dimen <$> evaluateInternalGlue g

-- Length.

evaluateLength :: (MonadReader Config m, MonadError Text m) => AST.Length -> m Int
evaluateLength (AST.Length (T.Sign isPos) uLn) =
    do
    eULn <- evaluateUnsignedLength uLn
    pure $ if isPos then eULn else -eULn

evaluateUnsignedLength :: (MonadReader Config m, MonadError Text m) => AST.UnsignedLength -> m Int
evaluateUnsignedLength = \case
    AST.NormalLengthAsULength v -> evaluateNormalLength v
    AST.CoercedLength v -> evaluateCoercedLength v

evaluateNormalLength :: (MonadReader Config m, MonadError Text m) => AST.NormalLength -> m Int
evaluateNormalLength (AST.LengthSemiConstant f u) =
    do
    ef <- evaluateFactor f
    eu <- evaluateUnit u
    pure $ round $ ef * eu
evaluateNormalLength (AST.InternalLength v) = evaluateInternalLength v

evaluateFactor :: (MonadReader Config m, MonadError Text m) => AST.Factor -> m Rational
evaluateFactor = \case
    AST.NormalIntegerFactor n -> fromIntegral <$> evaluateNormalInteger n
    AST.RationalConstant r -> pure r

evaluateUnit :: (MonadReader Config m, MonadError Text m) => AST.Unit -> m Rational
evaluateUnit = \case
    AST.InternalUnit u -> evaluateInternalUnit u
    AST.PhysicalUnit AST.TrueFrame u -> pure $ Unit.inScaledPoint u
    AST.PhysicalUnit AST.MagnifiedFrame u ->
        do
        _mag <- asks $ lookupIntegerParameter T.Mag
        eU <- evaluateUnit $ AST.PhysicalUnit AST.TrueFrame u
        pure $ eU * 1000 / fromIntegral _mag

evaluateInternalUnit :: (MonadReader Config m, MonadError Text m) => AST.InternalUnit -> m Rational
evaluateInternalUnit = \case
    AST.Em -> (TFM.quad . fontMetrics) <$> currentFontInfo
    AST.Ex -> (TFM.xHeight . fontMetrics) <$> currentFontInfo
    AST.InternalIntegerUnit v -> fromIntegral <$> evaluateInternalInteger v
    AST.InternalLengthUnit v -> fromIntegral <$> evaluateInternalLength v
    AST.InternalGlueUnit v -> (fromIntegral . BL.dimen) <$> evaluateInternalGlue v

evaluateInternalLength :: (MonadReader Config m, MonadError Text m) => AST.InternalLength -> m Int
evaluateInternalLength = \case
    AST.InternalLengthVariable v -> evaluateLengthVariable v
    AST.InternalSpecialLength v -> evaluateSpecialLength v
    AST.InternalFontDimensionRef v -> evaluateFontDimensionRef v
    AST.InternalBoxDimensionRef v -> evaluateBoxDimensionRef v
    AST.LastKern -> notImplemented

evaluateLengthVariable :: (MonadReader Config m, MonadError Text m) => AST.LengthVariable -> m Int
evaluateLengthVariable = \case
    AST.ParamVar p -> asks $ lookupLengthParameter p
    AST.RegisterVar n -> getRegisterIdx n lookupLengthRegister

evaluateSpecialLength :: (MonadReader Config m) => T.SpecialLength -> m Int
evaluateSpecialLength p = asks $ lookupSpecialLength p

evaluateFontDimensionRef :: (MonadReader Config m, MonadError Text m) => AST.FontDimensionRef -> m Int
evaluateFontDimensionRef (AST.FontDimensionRef n _) =
    do
    _ <- evaluateNumber n
    notImplemented

evaluateBoxDimensionRef :: (MonadReader Config m, MonadError Text m) => AST.BoxDimensionRef -> m Int
evaluateBoxDimensionRef (AST.BoxDimensionRef n _) =
    do
    _ <- evaluateNumber n
    notImplemented

evaluateCoercedLength :: (MonadReader Config m, MonadError Text m) => AST.CoercedLength -> m Int
evaluateCoercedLength (AST.InternalGlueAsLength g) = BL.dimen <$> evaluateInternalGlue g

-- Math length.

evaluateMathLength :: (MonadReader Config m, MonadError Text m) => AST.MathLength -> m Int
evaluateMathLength (AST.MathLength (T.Sign isPos) uLn) =
    do
    eULn <- evaluateUnsignedMathLength uLn
    pure $ if isPos then eULn else -eULn

evaluateUnsignedMathLength :: (MonadReader Config m, MonadError Text m) => AST.UnsignedMathLength -> m Int
evaluateUnsignedMathLength = \case
    AST.NormalMathLengthAsUMathLength v -> evaluateNormalMathLength v
    AST.CoercedMathLength v -> evaluateCoercedMathLength v

evaluateNormalMathLength :: (MonadReader Config m, MonadError Text m) => AST.NormalMathLength -> m Int
evaluateNormalMathLength (AST.MathLengthSemiConstant f mathU) =
    do
    ef <- evaluateFactor f
    eu <- evaluateMathUnit mathU
    pure $ round $ ef * (fromIntegral eu)

evaluateMathUnit :: (MonadReader Config m, MonadError Text m) => AST.MathUnit -> m Int
evaluateMathUnit = \case
    AST.Mu -> pure 1
    AST.InternalMathGlueAsUnit mg -> (BL.dimen . BL.unMathGlue) <$> evaluateInternalMathGlue mg

evaluateCoercedMathLength :: (MonadReader Config m, MonadError Text m) => AST.CoercedMathLength -> m Int
evaluateCoercedMathLength (AST.InternalMathGlueAsMathLength mg) = (BL.dimen . BL.unMathGlue) <$> evaluateInternalMathGlue mg

-- Glue.

evaluateGlue :: (MonadReader Config m, MonadError Text m) => AST.Glue -> m BL.Glue
evaluateGlue = \case
    AST.ExplicitGlue dim str shr ->
        BL.Glue <$> evaluateLength dim <*> evaluateFlex str <*> evaluateFlex shr
    AST.InternalGlue (T.Sign isPos) v ->
        do
        ev <- evaluateInternalGlue v
        pure $ if isPos then ev else (BL.negateGlue ev)

evaluateFlex :: (MonadReader Config m, MonadError Text m) => Maybe AST.Flex -> m BL.GlueFlex
evaluateFlex = \case
    Just (AST.FiniteFlex ln) ->
        do
        eLn <- evaluateLength ln
        pure BL.GlueFlex { BL.factor = fromIntegral eLn, BL.order = 0 }
    Just (AST.FilFlex ln) -> evaluateFilLength ln
    Nothing -> pure BL.noFlex

evaluateFilLength :: (MonadReader Config m, MonadError Text m) => AST.FilLength -> m BL.GlueFlex
evaluateFilLength (AST.FilLength (T.Sign isPos) f filOrder) =
    do
    eF <- evaluateFactor f
    pure BL.GlueFlex { BL.factor = if isPos then eF else -eF, BL.order = filOrder }

evaluateInternalGlue :: (MonadReader Config m, MonadError Text m) => AST.InternalGlue -> m BL.Glue
evaluateInternalGlue = \case
    AST.InternalGlueVariable v -> evaluateGlueVariable v
    AST.LastGlue -> notImplemented

evaluateGlueVariable :: (MonadReader Config m, MonadError Text m) => AST.GlueVariable -> m BL.Glue
evaluateGlueVariable = \case
    AST.ParamVar p    -> asks $ lookupGlueParameter p
    AST.RegisterVar n -> getRegisterIdx n lookupGlueRegister

-- Math glue.

evaluateMathGlue :: (MonadReader Config m, MonadError Text m) => AST.MathGlue -> m BL.MathGlue
evaluateMathGlue = \case
    AST.ExplicitMathGlue mDim mStr mShr ->
        do
        g <- BL.Glue <$> evaluateMathLength mDim <*> evaluateMathFlex mStr <*> evaluateMathFlex mShr
        pure $ BL.MathGlue g
    AST.InternalMathGlue (T.Sign isPos) v ->
        do
        ev <- evaluateInternalMathGlue v
        pure $ if isPos then ev else BL.negateMathGlue ev

evaluateMathFlex :: (MonadReader Config m, MonadError Text m) => Maybe AST.MathFlex -> m BL.GlueFlex
evaluateMathFlex = \case
    Just (AST.FiniteMathFlex ln) ->
        do
        eLn <- evaluateMathLength ln
        pure BL.GlueFlex { BL.factor = fromIntegral eLn, BL.order = 0 }
    Just (AST.FilMathFlex ln) -> evaluateFilLength ln
    Nothing -> pure BL.noFlex

evaluateInternalMathGlue :: (MonadReader Config m, MonadError Text m) => AST.InternalMathGlue -> m BL.MathGlue
evaluateInternalMathGlue = \case
    AST.InternalMathGlueVariable v -> evaluateMathGlueVariable v
    AST.LastMathGlue -> notImplemented

evaluateMathGlueVariable :: (MonadReader Config m, MonadError Text m) => AST.MathGlueVariable -> m BL.MathGlue
evaluateMathGlueVariable = \case
    AST.ParamVar p    -> asks $ lookupMathGlueParameter p
    AST.RegisterVar n -> getRegisterIdx n lookupMathGlueRegister

-- Token list.

evaluateTokenListVariable :: (MonadReader Config m, MonadError Text m) => AST.TokenListVariable -> m T.BalancedText
evaluateTokenListVariable = \case
    AST.ParamVar p    -> asks $ lookupTokenListParameter p
    AST.RegisterVar n -> getRegisterIdx n lookupTokenListRegister

-- Showing internal quantities.

-- For \number, \romannumeral, \string. \meaning, \jobname, and \fontname:
-- Each character code gets category "other" , except that 32 is gets "space".
asMadeToken :: CharCode -> Lex.Token
asMadeToken c =
    let cat = if c == ' ' then Lex.Space else Lex.Other
    in Lex.CharCatToken $ Lex.CharCat c cat

stringAsMadeTokens :: [CharCode] -> [Lex.Token]
stringAsMadeTokens = fmap asMadeToken

showInternalQuantity :: (MonadReader Config m, MonadError Text m) => AST.InternalQuantity -> m [CharCode]
showInternalQuantity = \case
    AST.InternalIntegerQuantity n ->
        do
        en <- evaluateInternalInteger n
        pure $ show en
    AST.InternalLengthQuantity d ->
        do
        ed <- evaluateInternalLength d
        notImplemented
    AST.InternalGlueQuantity g ->
        do
        eg <- evaluateInternalGlue g
        notImplemented
    AST.InternalMathGlueQuantity mg ->
        notImplemented
        -- emg <- evaluateInternalMathGlue mg
    AST.FontQuantity f ->
        do
        ef <- evaluateFontRef f
        notImplemented
    AST.TokenListVariableQuantity tl ->
        do
        etl <- evaluateTokenListVariable tl
        notImplemented

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

evaluateConditionHead :: (MonadReader Config m, MonadError Text m) => AST.ConditionHead -> m ConditionBlockTarget
evaluateConditionHead = \case
    AST.CaseConditionHead n ->
        do
        en <- evaluateNumber n
        pure $ CaseBlockTarget en
    AST.IfConditionHead ifH ->
        do
        ifResult <- evaluateIfConditionHead ifH
        pure $ IfBlockTarget $ if ifResult
            then IfPreElse
            else IfPostElse

evaluateIfConditionHead :: (MonadReader Config m, MonadError Text m) => AST.IfConditionHead -> m Bool
evaluateIfConditionHead = \case
    AST.IfIntegerPairTest n1 ordering n2 ->
        do
        en1 <- evaluateNumber n1
        en2 <- evaluateNumber n2
        pure $ (ordToComp ordering) en1 en2
    AST.IfLengthPairTest d1 ordering d2 ->
        do
        ed1 <- evaluateLength d1
        ed2 <- evaluateLength d2
        pure $ (ordToComp ordering) ed1 ed2
    AST.IfIntegerOdd n ->
        do
        en <- evaluateNumber n
        pure $ en `mod` 2 == 1
    AST.IfInMode _ -> notImplemented
    -- A control sequence token is considered to have character code 256 and
    -- category code 16.
    -- This logic is hard to follow literally, because my category codee type
    -- is an explicit enumeration, not an integer. So I'll just interpret it
    -- as: control sequences are considered equal to each other, and unequal to
    -- all char-cat pairs.
    -- TODO: Unless the control sequence has been \let equal to a non-active
    -- character token.
    AST.IfTokenAttributesEqual T.CharCodeAttribute t1 t2 -> pure $ eqChars t1 t2
    AST.IfTokenAttributesEqual T.CatCodeAttribute t1 t2 -> pure $ eqCats t1 t2
    --  The condition is true if (a) the two tokens are not macros, and they
    --  both represent the same (character code, category code) pair, the same
    --  TeX primitive, the same \font or \chardef or \countdef, etc.; or if (b)
    --  the two tokens are macros, and they both have the same status with
    --  respect to \long and \outer, and they both have the same parameters and
    --  “top level” expansion.
    AST.IfTokensEqual (Lex.CharCatToken cc1) (Lex.CharCatToken cc2) -> pure $ cc1 == cc2
    AST.IfTokensEqual (Lex.ControlSequenceToken cs1) (Lex.ControlSequenceToken cs2) ->
        do
        conf <- ask
        let lkp cs = lookupCSProper cs conf
        -- Surprisingly, two notImplemented control sequences are considered equal,
        -- so we may compare the Maybe types.
        -- The 'Just' values are arranged so that I think their naïve
        -- comparison gives the desired behaviour.
        pure $ lkp cs1 == lkp cs2
    AST.IfTokensEqual _ _ -> pure False
    AST.IfBoxRegisterIs attr n ->
        do
        _ <- evaluateNumber n
        case attr of
            T.HasVerticalBox -> notImplemented
            T.HasHorizontalBox -> notImplemented
            T.IsVoid -> notImplemented
    AST.IfInputEnded n ->
        do
        _ <- evaluateNumber n
        notImplemented
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

evaluateKern :: (MonadReader Config m, MonadError Text m) => AST.Length -> m B.Kern
evaluateKern ln = B.Kern <$> evaluateLength ln

evaluatePenalty :: (MonadReader Config m, MonadError Text m) => AST.Number -> m BL.Penalty
evaluatePenalty n = (BL.Penalty . fromIntegral) <$> evaluateNumber n

evaluateCharCodeRef :: (MonadReader Config m, MonadError Text m) => AST.CharCodeRef -> m CharCode
evaluateCharCodeRef ref = case ref of
    AST.CharRef c       -> pure c
    AST.CharTokenRef c  -> pure $ chr c
    AST.CharCodeNrRef n -> chr <$> evaluateNumber n

evaluateBoxSpecification :: (MonadReader Config m, MonadError Text m) => AST.BoxSpecification -> m B.DesiredLength
evaluateBoxSpecification = \case
    AST.Natural -> pure B.Natural
    AST.To ln -> B.To <$> evaluateLength ln
    AST.Spread ln -> B.Spread <$> evaluateLength ln

evaluateFontSpecification
    :: (MonadReader Config m, MonadError Text m)
    => Rational -> AST.FontSpecification -> m Rational
evaluateFontSpecification designSizeSP = \case
    AST.NaturalFont ->
        pure 1
    AST.FontAt ln ->
        do
        eLn <- evaluateLength ln
        pure $ fromIntegral eLn / designSizeSP
    AST.FontScaled n ->
        do
        en <- evaluateNumber n
        pure $ fromIntegral en / 1000
