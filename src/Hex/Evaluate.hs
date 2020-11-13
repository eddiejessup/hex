{-# LANGUAGE UndecidableInstances #-}

module Hex.Evaluate where

import           Hexlude

import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Sequence as Seq

import qualified TFM

import qualified Hex.Box              as B
import qualified Hex.BreakList        as BL
import qualified Hex.Config.Codes     as Code
import           Hex.Config
import qualified Hex.Lex              as Lex
import qualified Hex.Parse.AST        as AST
import qualified Hex.Resolve.Token      as T
import           Hex.Quantity

type MonadEvaluateCtx e st m =
    ( Monad m
    , MonadError e m
    , AsType EvaluationError e
    , AsType ConfigError e
    , MonadState st m
    , HasType Config st
    )

newtype EvaluationError
    = EvaluationError Text
    deriving stock (Show)

class Monad m => MonadEvaluate m a where
    type EvalTarget a

    astEval :: a -> m (EvalTarget a)

instance (Monad m, MonadEvaluate m AST.TokenListVariable) => MonadEvaluate m AST.TokenListAssignmentTarget where
    type EvalTarget AST.TokenListAssignmentTarget = T.BalancedText

    astEval = \case
        AST.TokenListAssignmentVar tgtVar ->
            astEval tgtVar
        AST.TokenListAssignmentText tgtText ->
            pure tgtText

signedTeXEval
    :: ( Monad m,
         MonadEvaluate m a
       , Num (EvalTarget a)
       )
    => T.Signed a
    -> m (EvalTarget a)
signedTeXEval (T.Signed sign u) =
    do
    eu <- astEval u
    pure $ T.evalSigned (T.Signed sign eu)

instance MonadEvaluate m AST.UnsignedTeXInt => MonadEvaluate m AST.TeXInt where
    type EvalTarget AST.TeXInt = TeXInt

    astEval = signedTeXEval

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.UnsignedTeXInt where
    type EvalTarget AST.UnsignedTeXInt = TeXInt

    astEval = \case
        AST.NormalTeXIntAsUTeXInt v -> astEval v
        AST.CoercedTeXInt v -> astEval v

instance (Monad m, MonadEvaluate m AST.InternalTeXInt) => MonadEvaluate m AST.NormalTeXInt where
    type EvalTarget AST.NormalTeXInt = TeXInt

    astEval = \case
        AST.TeXIntConstant n -> pure n
        AST.InternalTeXInt v -> astEval v

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.InternalTeXInt where
    type EvalTarget AST.InternalTeXInt = TeXInt

    astEval = \case
        AST.InternalTeXIntVariable v -> astEval v
        AST.InternalSpecialTeXInt v  -> astEval v
        AST.InternalCodeTableRef v    -> astEval v
        AST.InternalCharToken n       -> pure n
        AST.InternalMathCharToken n   -> pure n
        AST.InternalFontCharRef v     -> astEval v
        AST.LastPenalty               -> panic "Not implemented: evaluate LastPenalty"
        AST.ParShape                  -> panic "Not implemented: evaluate ParShape"
        AST.InputLineNr               -> panic "Not implemented: evaluate InputLineNr"
        AST.Badness                   -> panic "Not implemented: evaluate Badness"

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.EightBitTeXInt where
    type EvalTarget AST.EightBitTeXInt = EightBitInt

    astEval (AST.EightBitTeXInt n) =
        astEval n >>= toEightBit
      where
        toEightBit tn@(TeXInt i) =
            note
                (injectTyped (EvaluationError ("TeXInt not in range: " <> show tn)))
                (newEightBitInt i)

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.TeXIntVariable where
    type EvalTarget AST.TeXIntVariable = TeXInt

    astEval = \case
        AST.ParamVar p -> uses (typed @Config) $ lookupTeXIntParameter p
        AST.RegisterVar n -> astEval n >>= (\i -> uses (typed @Config) (lookupTeXIntRegister i))

instance (Monad m, MonadError e m, MonadState st m, HasType Config st) => MonadEvaluate m T.SpecialTeXInt where
    type EvalTarget T.SpecialTeXInt = TeXInt

    astEval p = uses (typed @Config) (lookupSpecialTeXInt p)

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.CodeTableRef where
    type EvalTarget AST.CodeTableRef = TeXInt

    astEval (AST.CodeTableRef q n) =
        do
        conf <- use (typed @Config)
        idxInt <- astEval n
        idxChar <- note
            (injectTyped (EvaluationError ("Outside range: " <> show idxInt)))
            (fromTeXInt idxInt)
        let
            lookupFrom :: TeXCode v => (Scope -> CharCodeMap v) -> Maybe TeXInt
            lookupFrom getMap = toTeXInt <$> scopedMapLookup getMap idxChar conf

            lookupFromHashMap :: forall v. TeXCode v => (Scope -> HashMap.HashMap CharCode v) -> Maybe TeXInt
            lookupFromHashMap getMap = toTeXInt <$> scopedMapLookup getMap idxChar conf
        note (injectTyped (EvaluationError "err")) $ case q of
            T.CategoryCodeType            -> lookupFromHashMap catCodes
            T.MathCodeType                -> lookupFrom mathCodes
            T.ChangeCaseCodeType Upward   -> lookupFrom uppercaseCodes
            T.ChangeCaseCodeType Downward -> lookupFrom lowercaseCodes
            T.SpaceFactorCodeType         -> lookupFrom spaceFactors
            T.DelimiterCodeType           -> lookupFrom delimiterCodes

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.FontCharRef where
    type EvalTarget AST.FontCharRef = TeXInt

    astEval (AST.FontCharRef fChar fontRef) =
        do
        eFontRef <- astEval fontRef
        fontInfo <- use (typed @Config) >>= \c -> lookupFontInfo c eFontRef
        pure $ case fChar of
            T.HyphenChar -> hyphenChar fontInfo
            T.SkewChar   -> skewChar fontInfo

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.FontRef where
    type EvalTarget AST.FontRef = TeXInt

    astEval = \case
        AST.FontTokenRef fNr -> pure fNr
        AST.CurrentFontRef ->
            uses (typed @Config) lookupCurrentFontNr
                >>= note (injectTyped $ ConfigError "Font number isn't set")
        AST.FamilyMemberFontRef v -> do
            foo <- astEval v
            use (typed @Config) >>= \c -> lookupFontFamilyMember c foo

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.FamilyMember where
    type EvalTarget AST.FamilyMember = (T.FontRange, TeXInt)
    astEval (AST.FamilyMember rng n) = (rng,) <$> astEval n

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.CoercedTeXInt where
    type EvalTarget AST.CoercedTeXInt = TeXInt

    astEval = \case
        AST.InternalLengthAsInt ln -> lengthToInt <$> astEval ln
        AST.InternalGlueAsInt g -> lengthToInt . BL.dimen <$> astEval g

-- Length.

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.Length where
    type EvalTarget AST.Length = Length

    astEval = signedTeXEval

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.UnsignedLength where
    type EvalTarget AST.UnsignedLength = Length

    astEval = \case
        AST.NormalLengthAsULength v -> astEval v
        AST.CoercedLength v -> astEval v

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.NormalLength where
    type EvalTarget AST.NormalLength = Length

    astEval = \case
        AST.LengthSemiConstant f u ->
            do
            ef <- astEval f
            eu <- astEval u
            pure $ round $ ef * eu
        AST.InternalLength v ->
            astEval v

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.Factor where
    type EvalTarget AST.Factor = Rational

    astEval = \case
        AST.NormalTeXIntFactor n -> fromIntegral <$> astEval n
        AST.RationalConstant r -> pure r

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.Unit where
    type EvalTarget AST.Unit = Rational

    astEval = \case
        AST.InternalUnit u -> astEval u
        AST.PhysicalUnit AST.TrueFrame u -> pure $ inScaledPoint u
        AST.PhysicalUnit AST.MagnifiedFrame u ->
            do
            mag_ <- uses (typed @Config) (lookupTeXIntParameter T.Mag)
            eU <- astEval $ AST.PhysicalUnit AST.TrueFrame u
            pure $ eU * 1000 / fromIntegral mag_

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.InternalUnit where
    type EvalTarget AST.InternalUnit = Rational

    astEval = \case
        AST.Em -> use (typed @Config) >>= currentFontInfo <&> (fontMetrics >>> TFM.quad)
        AST.Ex -> use (typed @Config) >>= currentFontInfo <&> (fontMetrics >>> TFM.xHeight)
        AST.InternalTeXIntUnit v -> fromIntegral <$> astEval v
        AST.InternalLengthUnit v -> fromIntegral <$> astEval v
        AST.InternalGlueUnit v -> fromIntegral . BL.dimen <$> astEval v

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.InternalLength where
    type EvalTarget AST.InternalLength = Length

    astEval = \case
        AST.InternalLengthVariable v -> astEval v
        AST.InternalSpecialLength v -> astEval v
        AST.InternalFontDimensionRef v -> astEval v
        AST.InternalBoxDimensionRef v -> astEval v
        AST.LastKern -> panic "Not implemented: evaluate LastKern"

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.LengthVariable where
    type EvalTarget AST.LengthVariable = Length

    astEval = \case
        AST.ParamVar p -> uses (typed @Config) (lookupLengthParameter p)
        AST.RegisterVar n -> astEval n >>= (\i -> uses (typed @Config ) (lookupLengthRegister i))

instance (Monad m, MonadState st m, HasType Config st) => MonadEvaluate m T.SpecialLength where
    type EvalTarget T.SpecialLength = Length

    astEval p = uses (typed @Config) (lookupSpecialLength p)

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.FontDimensionRef where
    type EvalTarget AST.FontDimensionRef = Length

    astEval (AST.FontDimensionRef n _) =
        do
        _ <- astEval n
        panic "Not implemented: evaluate FontDimensionRef"

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.BoxDimensionRef where
    type EvalTarget AST.BoxDimensionRef = Length

    astEval (AST.BoxDimensionRef idx boxDim) =
        do
        eIdx <- astEval idx
        mayBoxReg <- uses (typed @Config) (lookupBoxRegister eIdx)
        pure $ maybe 0 (naturalLength boxDim) mayBoxReg

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.CoercedLength where
    type EvalTarget AST.CoercedLength = Length
    astEval (AST.InternalGlueAsLength g) = BL.dimen <$> astEval g

-- Math length.

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.MathLength where
    type EvalTarget AST.MathLength = MathLength

    astEval = signedTeXEval

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.UnsignedMathLength where
    type EvalTarget AST.UnsignedMathLength = MathLength

    astEval = \case
        AST.NormalMathLengthAsUMathLength v -> astEval v
        AST.CoercedMathLength v -> astEval v

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.NormalMathLength where
    type EvalTarget AST.NormalMathLength = MathLength

    astEval (AST.MathLengthSemiConstant f mathU) =
        do
        ef <- astEval f
        eu <- astEval mathU
        pure $ round $ ef * fromIntegral eu

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.MathUnit where
    type EvalTarget AST.MathUnit = MathLength

    astEval = \case
        AST.Mu ->
            pure 1
        AST.InternalMathGlueAsUnit mg ->
            BL.dimen <$> astEval mg

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.CoercedMathLength where
    type EvalTarget AST.CoercedMathLength = MathLength
    astEval (AST.InternalMathGlueAsMathLength mg) = BL.dimen <$> astEval mg

-- Glue.

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.Glue where
    type EvalTarget AST.Glue = BL.Glue Length

    astEval = \case
        AST.ExplicitGlue dim str shr ->
            BL.Glue <$> astEval dim <*> evalFlex str <*> evalFlex shr
        AST.InternalGlue (T.Signed sign v) ->
            do
            ev <- astEval v
            pure $ case sign of
                T.Positive -> ev
                T.Negative -> BL.negateGlue ev
      where
        evalFlex = \case
            Just f -> astEval f
            Nothing -> pure BL.noFlex

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.Flex where
    type EvalTarget AST.Flex = BL.GlueFlex

    astEval = \case
        AST.FiniteFlex ln ->
            do
            eLn <- astEval ln
            pure BL.GlueFlex { BL.factor = fromIntegral eLn, BL.order = 0 }
        AST.FilFlex ln -> astEval ln

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.FilLength where
    type EvalTarget AST.FilLength = BL.GlueFlex

    astEval (AST.FilLength fl filOrder) =
        do
        efl <- signedTeXEval fl
        pure BL.GlueFlex { BL.factor = efl, BL.order = filOrder }

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.InternalGlue where
    type EvalTarget AST.InternalGlue = BL.Glue Length

    astEval = \case
        AST.InternalGlueVariable v -> astEval v
        AST.LastGlue -> panic "Not implemented: evaluate LastGlue"

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.GlueVariable where
    type EvalTarget AST.GlueVariable = BL.Glue Length

    astEval = \case
        AST.ParamVar p    -> uses (typed @Config) (lookupGlueParameter p)
        AST.RegisterVar n -> astEval n >>= (\i -> uses (typed @Config ) (lookupGlueRegister i))

-- Math glue.

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.MathGlue where
    type EvalTarget AST.MathGlue = BL.Glue MathLength

    astEval = \case
        AST.ExplicitMathGlue mDim mStr mShr ->
            BL.Glue <$> astEval mDim <*> evalMathFlex mStr <*> evalMathFlex mShr
        AST.InternalMathGlue sign v ->
            do
            ev <- astEval v
            pure $ case sign of
                T.Positive -> ev
                T.Negative -> BL.negateGlue ev
      where
        evalMathFlex = \case
            Just f -> astEval f
            Nothing -> pure BL.noFlex

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.MathFlex where
    type EvalTarget AST.MathFlex = BL.GlueFlex

    astEval = \case
        AST.FiniteMathFlex ln ->
            do
            eLn <- astEval ln
            pure BL.GlueFlex { BL.factor = fromIntegral eLn, BL.order = 0 }
        AST.FilMathFlex ln -> astEval ln

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.InternalMathGlue where
    type EvalTarget AST.InternalMathGlue = BL.Glue MathLength

    astEval = \case
        AST.InternalMathGlueVariable v -> astEval v
        AST.LastMathGlue -> panic "Not implemented: evaluate LastMathGlue"

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.MathGlueVariable where
    type EvalTarget AST.MathGlueVariable = BL.Glue MathLength

    astEval = \case
        AST.ParamVar p    -> uses (typed @Config) (lookupMathGlueParameter p)
        AST.RegisterVar n -> astEval n >>= (\i -> uses (typed @Config) (lookupMathGlueRegister i))

-- Token list.

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.TokenListVariable where
    type EvalTarget AST.TokenListVariable = T.BalancedText

    astEval = \case
        AST.ParamVar p    -> uses (typed @Config) (lookupTokenListParameter p)
        AST.RegisterVar n -> astEval n >>= (\i -> uses (typed @Config) (lookupTokenListRegister i))

-- Showing internal quantities.

-- For \number, \romannumeral, \string. \meaning, \jobname, and \fontname: Each
-- character code gets category "other" , except that 32 gets "space".
charCodeAsMadeToken :: CharCode -> Lex.Token
charCodeAsMadeToken ch =
    Lex.CharCatToken $ Lex.CharCat ch $ case ch of
        Code.CharCode_ ' ' -> Code.Space
        _                  -> Code.Other

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.InternalQuantity where
    type EvalTarget AST.InternalQuantity = Seq CharCode

    astEval = \case
        AST.InternalTeXIntQuantity n ->
            do
            en <- astEval n
            pure $ Seq.fromList $ unsafeCodeFromChar <$> show (unInt en)
        AST.InternalLengthQuantity d ->
            do
            _ <- astEval d
            panic "Not implemented: evaluate InternalLengthQuantity"
        AST.InternalGlueQuantity g ->
            do
            _ <- astEval g
            panic "Not implemented: evaluate InternalGlueQuantity"
        AST.InternalMathGlueQuantity mg ->
            do
            _ <- astEval mg
            panic "Not implemented: evaluate InternalMathGlueQuantity"
        AST.FontQuantity f ->
            do
            _ <- astEval f
            panic "Not implemented: evaluate FontQuantity"
        AST.TokenListVariableQuantity tl ->
            do
            _ <- astEval tl
            panic "Not implemented: evaluate TokenListVariableQuantity"

-- Condition

data IfBodyState
    = IfPreElse
    | IfPostElse
    deriving stock (Show, Eq)

data ConditionBlockTarget
    = IfBlockTarget IfBodyState
    | CaseBlockTarget TeXInt
    deriving stock (Show)

data CaseBodyState
    = CasePostOr
    | CasePostElse
    deriving stock (Show)

data ConditionBodyState
    = IfBodyState IfBodyState
    | CaseBodyState CaseBodyState
    deriving stock (Show)

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.ConditionHead where
    type EvalTarget AST.ConditionHead = ConditionBlockTarget

    astEval = \case
        AST.CaseConditionHead n ->
            do
            en <- astEval n
            pure $ CaseBlockTarget en
        AST.IfConditionHead ifH ->
            do
            ifResult <- astEval ifH
            pure $ IfBlockTarget $ if ifResult
                then IfPreElse
                else IfPostElse

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.IfConditionHead where
    type EvalTarget AST.IfConditionHead = Bool

    astEval = \case
        AST.IfTeXIntPairTest n1 ordering n2 ->
            do
            en1 <- astEval n1
            en2 <- astEval n2
            pure $ ordToComp ordering en1 en2
        AST.IfLengthPairTest d1 ordering d2 ->
            do
            ed1 <- astEval d1
            ed2 <- astEval d2
            pure $ ordToComp ordering ed1 ed2
        AST.IfTeXIntOdd n ->
            do
            en <- astEval n
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
            conf <- use (typed @Config)
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
            _ <- astEval n
            case attr of
                T.HasVerticalBox ->
                    panic "Not implemented: evaluate IfBoxRegister HasVerticalBox"
                T.HasHorizontalBox ->
                    panic "Not implemented: evaluate IfBoxRegister HasHorizontalBox"
                T.IsVoid ->
                    panic "Not implemented: evaluate IfBoxRegister IsVoid"
        AST.IfInputEnded n ->
            do
            _ <- astEval n
            panic "Not implemented: evaluate IfInputEnded"
        AST.IfConst b -> pure b
      where
        ordToComp GT = (>)
        ordToComp LT = (<)
        ordToComp EQ = (==)

        eqChars
            (T.UnresolvedTok (Lex.CharCatToken (Lex.CharCat c1 _)))
            (T.UnresolvedTok (Lex.CharCatToken (Lex.CharCat c2 _))) = c1 == c2
        eqChars _ _ = True

        eqCats
            (T.UnresolvedTok (Lex.CharCatToken (Lex.CharCat _ c1)))
            (T.UnresolvedTok (Lex.CharCatToken (Lex.CharCat _ c2))) = c1 == c2
        eqCats _ _ = True

-- Other.

instance MonadEvaluateCtx e st m => MonadEvaluate m AST.CharCodeRef where
    type EvalTarget AST.CharCodeRef = CharCode

    astEval = \case
        AST.CharRef ch -> pure ch
        AST.CharTokenRef ct  -> noteRange ct
        AST.CharCodeNrRef n -> astEval n >>= noteRange
      where
        noteRange x =
            note (injectTyped (EvaluationError ("TeXInt not in range: " <> show x)))
            (fromTeXInt x)

instance (Monad m, MonadEvaluate m AST.Length)  => MonadEvaluate m AST.BoxSpecification where
    type EvalTarget AST.BoxSpecification = B.DesiredLength

    astEval = \case
        AST.Natural -> pure B.Natural
        AST.To ln -> B.To <$> astEval ln
        AST.Spread ln -> B.Spread <$> astEval ln

evaluateFontSpecification
    :: (MonadEvaluate m AST.Length, MonadEvaluate m AST.TeXInt)
    => Length
    -> AST.FontSpecification
    -> m Rational
evaluateFontSpecification designSizeSP = \case
    AST.NaturalFont ->
        pure 1
    AST.FontAt ln ->
        do
        eLn <- astEval ln
        pure $ fromIntegral eLn `mkRatio` fromIntegral designSizeSP
    AST.FontScaled n ->
        do
        en <- astEval n
        pure $ fromIntegral en `mkRatio` 1000
