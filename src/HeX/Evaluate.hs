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

instance TeXEvaluable AST.TokenListAssignmentTarget where
    type EvalTarget AST.TokenListAssignmentTarget = T.BalancedText

    texEvaluate = \case
        AST.TokenListAssignmentVar tgtVar ->
            texEvaluate tgtVar
        AST.TokenListAssignmentText tgtText ->
            pure tgtText

-- Integer.

instance TeXEvaluable AST.Number where
    type EvalTarget AST.Number = IntVal

    texEvaluate (AST.Number (T.Sign isPos) u) =
        do
        size <- texEvaluate u
        pure $ if isPos then size else (-size)

instance TeXEvaluable AST.UnsignedNumber where
    type EvalTarget AST.UnsignedNumber = Int

    texEvaluate = \case
        AST.NormalIntegerAsUNumber v -> texEvaluate v
        AST.CoercedInteger v -> texEvaluate v

instance TeXEvaluable AST.NormalInteger where
    type EvalTarget AST.NormalInteger = Int

    texEvaluate = \case
        AST.IntegerConstant n -> pure n
        AST.InternalInteger v -> texEvaluate v

instance TeXEvaluable AST.InternalInteger where
    type EvalTarget AST.InternalInteger = Int

    texEvaluate = \case
        AST.InternalIntegerVariable v -> texEvaluate v
        AST.InternalSpecialInteger v  -> texEvaluate v
        AST.InternalCodeTableRef v    -> texEvaluate v
        AST.InternalCharToken n       -> pure n
        AST.InternalMathCharToken n   -> pure n
        AST.InternalFontCharRef v     -> texEvaluate v
        AST.LastPenalty               -> notImplemented
        AST.ParShape                  -> notImplemented
        AST.InputLineNr               -> notImplemented
        AST.Badness                   -> notImplemented

instance TeXEvaluable AST.EightBitNumber where
    type EvalTarget AST.EightBitNumber = EightBitInt

    texEvaluate (AST.EightBitNumber n) =
        texEvaluate n
        >>= (\i -> liftMaybe ("Number not in range: " <> show i) $ newEightBitInt i)

getRegisterIdx
    :: (MonadReader Config m, MonadError Text m)
    => AST.EightBitNumber
    -> (EightBitInt -> Config -> a)
    -> m a
getRegisterIdx n f =
    do
    en <- texEvaluate n
    asks $ f en

instance TeXEvaluable AST.IntegerVariable where
    type EvalTarget AST.IntegerVariable = Int

    texEvaluate = \case
        AST.ParamVar p -> asks $ lookupIntegerParameter p
        AST.RegisterVar n -> getRegisterIdx n lookupIntegerRegister

instance TeXEvaluable T.SpecialInteger where
    type EvalTarget T.SpecialInteger = Int

    texEvaluate p = asks $ lookupSpecialInteger p

instance TeXEvaluable AST.CodeTableRef where
    type EvalTarget AST.CodeTableRef = Int

    texEvaluate (AST.CodeTableRef q n) =
        do
        idx <- chr <$> texEvaluate n
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

instance TeXEvaluable AST.CoercedInteger where
    type EvalTarget AST.CoercedInteger = Int

    texEvaluate = \case
        AST.InternalLengthAsInt ln -> texEvaluate ln
        AST.InternalGlueAsInt g -> BL.dimen <$> texEvaluate g

-- Length.

instance TeXEvaluable AST.Length where
    type EvalTarget AST.Length = Int

    texEvaluate (AST.Length (T.Sign isPos) uLn) =
        do
        eULn <- texEvaluate uLn
        pure $ if isPos then eULn else -eULn

instance TeXEvaluable AST.UnsignedLength where
    type EvalTarget AST.UnsignedLength = Int

    texEvaluate = \case
        AST.NormalLengthAsULength v -> texEvaluate v
        AST.CoercedLength v -> texEvaluate v

instance TeXEvaluable AST.NormalLength where
    type EvalTarget AST.NormalLength = Int

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
        AST.NormalIntegerFactor n -> fromIntegral <$> texEvaluate n
        AST.RationalConstant r -> pure r

instance TeXEvaluable AST.Unit where
    type EvalTarget AST.Unit = Rational

    texEvaluate = \case
        AST.InternalUnit u -> texEvaluate u
        AST.PhysicalUnit AST.TrueFrame u -> pure $ Unit.inScaledPoint u
        AST.PhysicalUnit AST.MagnifiedFrame u ->
            do
            _mag <- asks $ lookupIntegerParameter T.Mag
            eU <- texEvaluate $ AST.PhysicalUnit AST.TrueFrame u
            pure $ eU * 1000 / fromIntegral _mag

instance TeXEvaluable AST.InternalUnit where
    type EvalTarget AST.InternalUnit = Rational

    texEvaluate = \case
        AST.Em -> (TFM.quad . fontMetrics) <$> currentFontInfo
        AST.Ex -> (TFM.xHeight . fontMetrics) <$> currentFontInfo
        AST.InternalIntegerUnit v -> fromIntegral <$> texEvaluate v
        AST.InternalLengthUnit v -> fromIntegral <$> texEvaluate v
        AST.InternalGlueUnit v -> (fromIntegral . BL.dimen) <$> texEvaluate v

instance TeXEvaluable AST.InternalLength where
    type EvalTarget AST.InternalLength = Int

    texEvaluate = \case
        AST.InternalLengthVariable v -> texEvaluate v
        AST.InternalSpecialLength v -> texEvaluate v
        AST.InternalFontDimensionRef v -> texEvaluate v
        AST.InternalBoxDimensionRef v -> texEvaluate v
        AST.LastKern -> notImplemented

instance TeXEvaluable AST.LengthVariable where
    type EvalTarget AST.LengthVariable = Int

    texEvaluate = \case
        AST.ParamVar p -> asks $ lookupLengthParameter p
        AST.RegisterVar n -> getRegisterIdx n lookupLengthRegister

instance TeXEvaluable T.SpecialLength where
    type EvalTarget T.SpecialLength = Int

    texEvaluate p = asks $ lookupSpecialLength p

instance TeXEvaluable AST.FontDimensionRef where
    type EvalTarget AST.FontDimensionRef = Int

    texEvaluate (AST.FontDimensionRef n _) =
        do
        _ <- texEvaluate n
        notImplemented

instance TeXEvaluable AST.BoxDimensionRef where
    type EvalTarget AST.BoxDimensionRef = Int

    texEvaluate (AST.BoxDimensionRef n _) =
        do
        _ <- texEvaluate n
        notImplemented

instance TeXEvaluable AST.CoercedLength where
    type EvalTarget AST.CoercedLength = Int
    texEvaluate (AST.InternalGlueAsLength g) = BL.dimen <$> texEvaluate g

-- Math length.

instance TeXEvaluable AST.MathLength where
    type EvalTarget AST.MathLength = Int

    texEvaluate (AST.MathLength (T.Sign isPos) uLn) =
        do
        eULn <- texEvaluate uLn
        pure $ if isPos then eULn else -eULn

instance TeXEvaluable AST.UnsignedMathLength where
    type EvalTarget AST.UnsignedMathLength = Int

    texEvaluate = \case
        AST.NormalMathLengthAsUMathLength v -> texEvaluate v
        AST.CoercedMathLength v -> texEvaluate v

instance TeXEvaluable AST.NormalMathLength where
    type EvalTarget AST.NormalMathLength = Int

    texEvaluate (AST.MathLengthSemiConstant f mathU) =
        do
        ef <- texEvaluate f
        eu <- texEvaluate mathU
        pure $ round $ ef * (fromIntegral eu)

instance TeXEvaluable AST.MathUnit where
    type EvalTarget AST.MathUnit = Int

    texEvaluate = \case
        AST.Mu -> pure 1
        AST.InternalMathGlueAsUnit mg -> (BL.dimen . BL.unMathGlue) <$> texEvaluate mg

instance TeXEvaluable AST.CoercedMathLength where
    type EvalTarget AST.CoercedMathLength = Int
    texEvaluate (AST.InternalMathGlueAsMathLength mg) = (BL.dimen . BL.unMathGlue) <$> texEvaluate mg

-- Glue.

instance TeXEvaluable AST.Glue where
    type EvalTarget AST.Glue = BL.Glue

    texEvaluate = \case
        AST.ExplicitGlue dim str shr ->
            BL.Glue <$> texEvaluate dim <*> evalFlex str <*> evalFlex shr
        AST.InternalGlue (T.Sign isPos) v ->
            do
            ev <- texEvaluate v
            pure $ if isPos then ev else (BL.negateGlue ev)
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

    texEvaluate (AST.FilLength (T.Sign isPos) f filOrder) =
        do
        eF <- texEvaluate f
        pure BL.GlueFlex { BL.factor = if isPos then eF else -eF, BL.order = filOrder }

instance TeXEvaluable AST.InternalGlue where
    type EvalTarget AST.InternalGlue = BL.Glue

    texEvaluate = \case
        AST.InternalGlueVariable v -> texEvaluate v
        AST.LastGlue -> notImplemented

instance TeXEvaluable AST.GlueVariable where
    type EvalTarget AST.GlueVariable = BL.Glue

    texEvaluate = \case
        AST.ParamVar p    -> asks $ lookupGlueParameter p
        AST.RegisterVar n -> getRegisterIdx n lookupGlueRegister

-- Math glue.

instance TeXEvaluable AST.MathGlue where
    type EvalTarget AST.MathGlue = BL.MathGlue

    texEvaluate = \case
        AST.ExplicitMathGlue mDim mStr mShr ->
            do
            g <- BL.Glue <$> texEvaluate mDim <*> evalMathFlex mStr <*> evalMathFlex mShr
            pure $ BL.MathGlue g
        AST.InternalMathGlue (T.Sign isPos) v ->
            do
            ev <- texEvaluate v
            pure $ if isPos then ev else BL.negateMathGlue ev
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
    type EvalTarget AST.InternalMathGlue = BL.MathGlue

    texEvaluate = \case
        AST.InternalMathGlueVariable v -> texEvaluate v
        AST.LastMathGlue -> notImplemented

instance TeXEvaluable AST.MathGlueVariable where
    type EvalTarget AST.MathGlueVariable = BL.MathGlue

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

-- For \number, \romannumeral, \string. \meaning, \jobname, and \fontname:
-- Each character code gets category "other" , except that 32 is gets "space".
asMadeToken :: CharCode -> Lex.Token
asMadeToken c =
    let cat = if c == ' ' then Lex.Space else Lex.Other
    in Lex.CharCatToken $ Lex.CharCat c cat

stringAsMadeTokens :: [CharCode] -> [Lex.Token]
stringAsMadeTokens = fmap asMadeToken

instance TeXEvaluable AST.InternalQuantity where
    type EvalTarget AST.InternalQuantity = [CharCode]

    texEvaluate = \case
        AST.InternalIntegerQuantity n ->
            do
            en <- texEvaluate n
            pure $ show en
        AST.InternalLengthQuantity d ->
            do
            _ <- texEvaluate d
            notImplemented
        AST.InternalGlueQuantity g ->
            do
            _ <- texEvaluate g
            notImplemented
        AST.InternalMathGlueQuantity mg ->
            do
            _ <- texEvaluate mg
            notImplemented
        AST.FontQuantity f ->
            do
            _ <- texEvaluate f
            notImplemented
        AST.TokenListVariableQuantity tl ->
            do
            _ <- texEvaluate tl
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
        AST.IfIntegerPairTest n1 ordering n2 ->
            do
            en1 <- texEvaluate n1
            en2 <- texEvaluate n2
            pure $ (ordToComp ordering) en1 en2
        AST.IfLengthPairTest d1 ordering d2 ->
            do
            ed1 <- texEvaluate d1
            ed2 <- texEvaluate d2
            pure $ (ordToComp ordering) ed1 ed2
        AST.IfIntegerOdd n ->
            do
            en <- texEvaluate n
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
            _ <- texEvaluate n
            case attr of
                T.HasVerticalBox -> notImplemented
                T.HasHorizontalBox -> notImplemented
                T.IsVoid -> notImplemented
        AST.IfInputEnded n ->
            do
            _ <- texEvaluate n
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
    :: (MonadReader Config m, MonadError Text m)
    => Rational -> AST.FontSpecification -> m Rational
evaluateFontSpecification designSizeSP = \case
    AST.NaturalFont ->
        pure 1
    AST.FontAt ln ->
        do
        eLn <- texEvaluate ln
        pure $ fromIntegral eLn / designSizeSP
    AST.FontScaled n ->
        do
        en <- texEvaluate n
        pure $ fromIntegral en / 1000
