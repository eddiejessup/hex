{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module HeX.Evaluate where

import           Control.Monad.Except           ( MonadError
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , asks
                                                , ask
                                                )
import           Data.Char                      ( chr )
import qualified Data.HashMap.Strict           as HMap

import qualified TFM

import           HeXPrelude
import           HeX.Type
import qualified HeX.Lex                       as Lex
import qualified HeX.Box                       as B
import qualified HeX.BreakList                 as BL
import           HeX.Categorise                 ( CharCode )
import           HeX.Config
import qualified HeX.Parse.AST                 as AST
import qualified HeX.Parse.Token               as T
import qualified HeX.Unit                      as Unit

-- Integer.

evaluateNumber :: (MonadReader Config m, MonadError String m) => AST.Number -> m Int
evaluateNumber (AST.Number (T.Sign isPos) u) =
    do
    size <- evaluateUnsignedNumber u
    pure $ if isPos then size else (-size)

evaluateUnsignedNumber :: (MonadReader Config m, MonadError String m) => AST.UnsignedNumber -> m Int
evaluateUnsignedNumber = \case
    AST.NormalIntegerAsUNumber v -> evaluateNormalInteger v
    AST.CoercedInteger v -> evaluateCoercedInteger v

evaluateNormalInteger :: (MonadReader Config m, MonadError String m) => AST.NormalInteger -> m Int
evaluateNormalInteger = \case
    AST.IntegerConstant n -> pure n
    AST.InternalInteger v -> evaluateInternalInteger v

evaluateInternalInteger :: (MonadReader Config m, MonadError String m) => AST.InternalInteger -> m Int
evaluateInternalInteger = \case
    AST.InternalIntegerVariable v -> evaluateIntegerVariable v
    AST.InternalSpecialInteger v  -> evaluateSpecialInteger v
    AST.InternalCodeTableRef v    -> evaluateCodeTableRef v
    AST.InternalCharToken n       -> pure n
    AST.InternalMathCharToken n   -> pure n
    AST.InternalFontCharRef v     -> evaluateFontCharRef v
    AST.LastPenalty               -> undefined
    AST.ParShape                  -> undefined
    AST.InputLineNr               -> undefined
    AST.Badness                   -> undefined

evaluateEightBitInt :: (MonadReader Config m,  MonadError String m) => AST.Number -> m EightBitInt
evaluateEightBitInt n = evaluateNumber n >>= (\i -> liftMaybe ("Number not in range: " ++ show i) $ newEightBitInt i)

getRegisterIdx
    :: (MonadReader Config m, MonadError String m)
    => AST.Number
    -> a
    -> (Config -> HMap.HashMap EightBitInt a)
    -> m a
getRegisterIdx n z f = HMap.lookupDefault z <$> evaluateEightBitInt n <*> asks f

evaluateIntegerVariable :: (MonadReader Config m, MonadError String m) => AST.IntegerVariable -> m Int
evaluateIntegerVariable = \case
    AST.ParamVar p -> asks (getIntParam p . params)
    AST.RegisterVar n -> getRegisterIdx n 0 integerRegister

evaluateSpecialInteger :: (MonadReader Config m, MonadError String m) => T.SpecialInteger -> m Int
evaluateSpecialInteger p = asks (getSpecialInt p . params)

evaluateCodeTableRef :: (MonadReader Config m, MonadError String m) => AST.CodeTableRef -> m Int
evaluateCodeTableRef (AST.CodeTableRef q n) =
    do
    idx <- chr <$> evaluateNumber n
    let lookupFrom getMap = asks (scopedMapLookup idx getMap) >>= liftMaybe "err"
    case q of
        T.CategoryCodeType       -> fromEnum <$> lookupFrom catCodeMap
        T.MathCodeType           -> fromEnum <$> lookupFrom mathCodeMap
        T.ChangeCaseCodeType dir -> fromEnum <$> lookupFrom (case dir of
            Upward   -> uppercaseMap
            Downward -> lowercaseMap)
        T.SpaceFactorCodeType    -> fromEnum <$> lookupFrom spaceFactorMap
        T.DelimiterCodeType      -> fromEnum <$> lookupFrom delimiterCodeMap

evaluateFontCharRef :: (MonadReader Config m, MonadError String m) => AST.FontCharRef -> m Int
evaluateFontCharRef (AST.FontCharRef fChar fontRef) =
    do
    fontInfo <- evaluateFontRef fontRef
    pure $ case fChar of
        T.HyphenChar -> hyphenChar fontInfo
        T.SkewChar   -> skewChar fontInfo

evaluateFontRef :: (MonadReader Config m, MonadError String m) => AST.FontRef -> m FontInfo
evaluateFontRef = \case
    AST.FontTokenRef fNr -> lookupFontInfo fNr
    AST.CurrentFontRef -> currentFontInfo
    AST.FamilyMemberFontRef v -> evaluateFamilyMember v

evaluateFamilyMember :: (MonadReader Config m, MonadError String m) => AST.FamilyMember -> m FontInfo
evaluateFamilyMember (AST.FamilyMember _ n) =
    do
    _ <- evaluateNumber n
    undefined

evaluateCoercedInteger :: (MonadReader Config m, MonadError String m) => AST.CoercedInteger -> m Int
evaluateCoercedInteger = \case
    AST.InternalLengthAsInt ln -> evaluateInternalLength ln
    AST.InternalGlueAsInt g -> BL.dimen <$> evaluateInternalGlue g

-- Length.

evaluateLength :: (MonadReader Config m, MonadError String m) => AST.Length -> m Int
evaluateLength (AST.Length (T.Sign isPos) uLn) =
    do
    eULn <- evaluateUnsignedLength uLn
    pure $ if isPos then eULn else -eULn

evaluateUnsignedLength :: (MonadReader Config m, MonadError String m) => AST.UnsignedLength -> m Int
evaluateUnsignedLength = \case
    AST.NormalLengthAsULength v -> evaluateNormalLength v
    AST.CoercedLength v -> evaluateCoercedLength v

evaluateNormalLength :: (MonadReader Config m, MonadError String m) => AST.NormalLength -> m Int
evaluateNormalLength (AST.LengthSemiConstant f u) =
    do
    ef <- evaluateFactor f
    eu <- evaluateUnit u
    pure $ round $ ef * eu
evaluateNormalLength (AST.InternalLength v) = evaluateInternalLength v

evaluateFactor :: (MonadReader Config m, MonadError String m) => AST.Factor -> m Rational
evaluateFactor = \case
    AST.NormalIntegerFactor n -> fromIntegral <$> evaluateNormalInteger n
    AST.RationalConstant r -> pure r

evaluateUnit :: (MonadReader Config m, MonadError String m) => AST.Unit -> m Rational
evaluateUnit = \case
    AST.InternalUnit u -> evaluateInternalUnit u
    AST.PhysicalUnit AST.TrueFrame u -> pure $ Unit.inScaledPoint u
    AST.PhysicalUnit AST.MagnifiedFrame u ->
        do
        _mag <- asks $ mag . params
        eU <- evaluateUnit $ AST.PhysicalUnit AST.TrueFrame u
        pure $ eU * 1000 / fromIntegral _mag

evaluateInternalUnit :: (MonadReader Config m, MonadError String m) => AST.InternalUnit -> m Rational
evaluateInternalUnit = \case
    AST.Em -> (TFM.quad . fontMetrics) <$> currentFontInfo
    AST.Ex -> (TFM.xHeight . fontMetrics) <$> currentFontInfo
    AST.InternalIntegerUnit v -> fromIntegral <$> evaluateInternalInteger v
    AST.InternalLengthUnit v -> fromIntegral <$> evaluateInternalLength v
    AST.InternalGlueUnit v -> (fromIntegral . BL.dimen) <$> evaluateInternalGlue v

evaluateInternalLength :: (MonadReader Config m, MonadError String m) => AST.InternalLength -> m Int
evaluateInternalLength = \case
    AST.InternalLengthVariable v -> evaluateLengthVariable v
    AST.InternalSpecialLength v -> evaluateSpecialLength v
    AST.InternalFontDimensionRef v -> evaluateFontDimensionRef v
    AST.InternalBoxDimensionRef v -> evaluateBoxDimensionRef v
    AST.LastKern -> undefined

evaluateLengthVariable :: (MonadReader Config m, MonadError String m) => AST.LengthVariable -> m Int
evaluateLengthVariable = \case
    AST.ParamVar p -> asks (getLenParam p . params)
    AST.RegisterVar n -> getRegisterIdx n 0 lengthRegister

evaluateSpecialLength :: (MonadReader Config m, MonadError String m) => T.SpecialLength -> m Int
evaluateSpecialLength p = asks (getSpecialLen p . params)

evaluateFontDimensionRef :: (MonadReader Config m, MonadError String m) => AST.FontDimensionRef -> m Int
evaluateFontDimensionRef (AST.FontDimensionRef n _) =
    do
    _ <- evaluateNumber n
    undefined

evaluateBoxDimensionRef :: (MonadReader Config m, MonadError String m) => AST.BoxDimensionRef -> m Int
evaluateBoxDimensionRef (AST.BoxDimensionRef n _) =
    do
    _ <- evaluateNumber n
    undefined

evaluateCoercedLength :: (MonadReader Config m, MonadError String m) => AST.CoercedLength -> m Int
evaluateCoercedLength (AST.InternalGlueAsLength g) = BL.dimen <$> evaluateInternalGlue g

-- Glue.

evaluateGlue :: (MonadReader Config m, MonadError String m) => AST.Glue -> m BL.Glue
evaluateGlue = \case
    AST.ExplicitGlue dim str shr ->
        BL.Glue <$> evaluateLength dim <*> evaluateFlex str <*> evaluateFlex shr
    AST.InternalGlue (T.Sign isPos) v ->
        do
        ev <- evaluateInternalGlue v
        pure $ if isPos then ev else (BL.negateGlue ev)

evaluateFlex :: (MonadReader Config m, MonadError String m) => Maybe AST.Flex -> m BL.GlueFlex
evaluateFlex = \case
    Just (AST.FiniteFlex ln) ->
        do
        eLn <- evaluateLength ln
        pure BL.GlueFlex{factor = fromIntegral eLn, order = 0}
    Just (AST.FilFlex ln) -> evaluateFilLength ln
    Nothing -> pure BL.noFlex

evaluateFilLength :: (MonadReader Config m, MonadError String m) => AST.FilLength -> m BL.GlueFlex
evaluateFilLength (AST.FilLength (T.Sign isPos) f ord) =
    do
    eF <- evaluateFactor f
    pure BL.GlueFlex{factor = if isPos then eF else -eF, order = ord}

evaluateInternalGlue :: (MonadReader Config m, MonadError String m) => AST.InternalGlue -> m BL.Glue
evaluateInternalGlue = \case
    AST.InternalGlueVariable v -> evaluateGlueVariable v
    AST.LastGlue -> undefined

evaluateGlueVariable :: (MonadReader Config m, MonadError String m) => AST.GlueVariable -> m BL.Glue
evaluateGlueVariable = \case
    AST.ParamVar p    -> asks (getGlueParam p . params)
    AST.RegisterVar n -> getRegisterIdx n mempty glueRegister

-- Token list.

evaluateTokenListVariable :: (MonadReader Config m, MonadError String m) => AST.TokenListVariable -> m T.BalancedText
evaluateTokenListVariable = \case
    AST.ParamVar p    -> asks (getTokenListParam p . params)
    AST.RegisterVar n -> getRegisterIdx n mempty tokenListRegister

-- Showing internal quantities.

-- For \number, \romannumeral, \string. \meaning, \jobname, and \fontname:
-- Each character code gets category "other" , except that 32 is gets "space".
asMadeToken :: CharCode -> Lex.Token
asMadeToken c =
    let cat = if c == ' ' then Lex.Space else Lex.Other
    in Lex.CharCatToken $ Lex.CharCat c cat

stringAsMadeTokens :: [CharCode] -> [Lex.Token]
stringAsMadeTokens = fmap asMadeToken

showInternalQuantity :: (MonadReader Config m, MonadError String m) => AST.InternalQuantity -> m String
showInternalQuantity = \case
    AST.InternalIntegerQuantity n ->
        do
        en <- evaluateInternalInteger n
        pure $ show en
    AST.InternalLengthQuantity d ->
        do
        ed <- evaluateInternalLength d
        undefined
    AST.InternalGlueQuantity g ->
        do
        eg <- evaluateInternalGlue g
        undefined
    AST.InternalMathGlueQuantity mg ->
        do
        undefined
        -- emg <- evaluateInternalMathGlue mg
    AST.FontQuantity f ->
        do
        ef <- evaluateFontRef f
        undefined
    AST.TokenListVariableQuantity tl ->
        do
        etl <- evaluateTokenListVariable tl
        undefined

-- Condition

data ElseState
    = PreElse
    | PostElse
    deriving (Show, Eq)

data BlockTarget
    = IfBlockTarget { tgtIfElseState :: ElseState }
    | CaseBlockTarget { currentCaseBlock, targetCaseBlock :: Int }
    deriving (Show)

data ConditionBlockState = ConditionBlockState ElseState BlockTarget
    deriving (Show)

evaluateConditionHead :: (MonadReader Config m, MonadError String m) => AST.ConditionHead -> m BlockTarget
evaluateConditionHead = \case
    AST.CaseConditionHead n ->
        do
        en <- evaluateNumber n
        pure $ CaseBlockTarget 0 en
    AST.IfConditionHead ifH ->
        do
        bool <- evaluateIfConditionHead ifH
        pure $ IfBlockTarget $ if bool
            then PreElse
            else PostElse

evaluateIfConditionHead :: (MonadReader Config m, MonadError String m) => AST.IfConditionHead -> m Bool
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
    AST.IfInMode _ -> undefined
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
        -- Surprisingly, two undefined control sequences are considered equal,
        -- so we may compare the Maybe types.
        -- The 'Just' values are arranged so that I think their naïve
        -- comparison gives the desired behaviour.
        pure $ lkp cs1 == lkp cs2
    AST.IfTokensEqual _ _ -> pure False
    AST.IfBoxRegisterIs attr n ->
        do
        _ <- evaluateNumber n
        case attr of
            T.HasVerticalBox -> undefined
            T.HasHorizontalBox -> undefined
            T.IsVoid -> undefined
    AST.IfInputEnded n ->
        do
        _ <- evaluateNumber n
        undefined
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

evaluateKern :: (MonadReader Config m, MonadError String m) => AST.Length -> m B.Kern
evaluateKern ln = B.Kern <$> evaluateLength ln

evaluatePenalty :: (MonadReader Config m, MonadError String m) => AST.Number -> m BL.Penalty
evaluatePenalty n = (BL.Penalty . fromIntegral) <$> evaluateNumber n

evaluateCharCodeRef :: (MonadReader Config m, MonadError String m) => AST.CharCodeRef -> m CharCode
evaluateCharCodeRef ref = case ref of
    AST.CharRef c       -> pure c
    AST.CharTokenRef c  -> pure $ chr c
    AST.CharCodeNrRef n -> chr <$> evaluateNumber n
