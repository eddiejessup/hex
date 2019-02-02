{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module HeX.Evaluate where

import           Control.Monad.Except           ( MonadError
                                                )
import           Control.Monad.State.Lazy       ( MonadState
                                                , gets
                                                )
import           Data.Char                      ( chr )

import qualified TFM

import qualified HeX.Box                       as B
import qualified HeX.BreakList                 as BL
import           HeX.Categorise                 ( CharCode )
import           HeX.Config
import qualified HeX.Parse.AST                 as AST
import qualified HeX.Parse.Token               as T
import qualified HeX.Unit                      as Unit

-- Integer.

evaluateNumber :: (MonadState Config m, MonadError String m) => AST.Number -> m Int
evaluateNumber (AST.Number (T.Sign isPos) u) =
    do
    size <- evaluateUnsignedNumber u
    pure $ if isPos then size else (-size)

evaluateUnsignedNumber :: (MonadState Config m, MonadError String m) => AST.UnsignedNumber -> m Int
evaluateUnsignedNumber = \case
    AST.NormalIntegerAsUNumber v -> evaluateNormalInteger v
    AST.CoercedInteger v -> evaluateCoercedInteger v

evaluateNormalInteger :: (MonadState Config m, MonadError String m) => AST.NormalInteger -> m Int
evaluateNormalInteger = \case
    AST.IntegerConstant n -> pure n
    AST.InternalInteger v -> evaluateInternalInteger v

evaluateInternalInteger :: (MonadState Config m, MonadError String m) => AST.InternalInteger -> m Int
evaluateInternalInteger = \case
    AST.InternalIntegerVariable v -> evaluateIntegerVariable v
    AST.InternalSpecialInteger v -> evaluateSpecialInteger v
    AST.InternalCodeTableRef v -> evaluateCodeTableRef v
    AST.InternalCharToken n -> pure n
    AST.InternalMathCharToken n -> pure n
    AST.InternalFontCharRef v -> evaluateFontCharRef v
    AST.LastPenalty -> undefined
    AST.ParShape -> undefined
    AST.InputLineNr -> undefined
    AST.Badness -> undefined

evaluateIntegerVariable :: (MonadState Config m, MonadError String m) => AST.IntegerVariable -> m Int
evaluateIntegerVariable = \case
    AST.ParamVar p -> gets (getIntParam p . params)
    AST.RegisterVar n ->
        do
        _ <- evaluateNumber n
        undefined

evaluateSpecialInteger :: (MonadState Config m, MonadError String m) => T.SpecialInteger -> m Int
evaluateSpecialInteger p = gets (getSpecialInt p . params)

evaluateCodeTableRef :: (MonadState Config m, MonadError String m) => AST.CodeTableRef -> m Int
evaluateCodeTableRef (AST.CodeTableRef _ n) =
    do
    _ <- evaluateNumber n
    undefined

evaluateFontCharRef :: (MonadState Config m, MonadError String m) => AST.FontCharRef -> m Int
evaluateFontCharRef (AST.FontCharRef fChar fontRef) =
    do
    fontInfo <- evaluateFontRef fontRef
    pure $ case fChar of
        T.HyphenChar -> hyphenChar fontInfo
        T.SkewChar -> skewChar fontInfo

evaluateFontRef :: (MonadState Config m, MonadError String m) => AST.FontRef -> m FontInfo
evaluateFontRef = \case
    AST.FontTokenRef fNr -> lookupFontInfo fNr
    AST.CurrentFontRef -> currentFontInfo
    AST.FamilyMemberFontRef v -> evaluateFamilyMember v

evaluateFamilyMember :: (MonadState Config m, MonadError String m) => AST.FamilyMember -> m FontInfo
evaluateFamilyMember (AST.FamilyMember _ n) =
    do
    _ <- evaluateNumber n
    undefined

evaluateCoercedInteger :: (MonadState Config m, MonadError String m) => AST.CoercedInteger -> m Int
evaluateCoercedInteger = \case
    AST.InternalLengthAsInt ln -> evaluateInternalLength ln
    AST.InternalGlueAsInt g -> BL.dimen <$> evaluateInternalGlue g

-- Length.

evaluateLength :: (MonadState Config m, MonadError String m) => AST.Length -> m Int
evaluateLength (AST.Length (T.Sign isPos) uLn) =
    do
    eULn <- evaluateUnsignedLength uLn
    pure $ if isPos then eULn else -eULn

evaluateUnsignedLength :: (MonadState Config m, MonadError String m) => AST.UnsignedLength -> m Int
evaluateUnsignedLength = \case
    AST.NormalLengthAsULength v -> evaluateNormalLength v
    AST.CoercedLength v -> evaluateCoercedLength v

evaluateNormalLength :: (MonadState Config m, MonadError String m) => AST.NormalLength -> m Int
evaluateNormalLength (AST.LengthSemiConstant f u) =
    do
    ef <- evaluateFactor f
    eu <- evaluateUnit u
    pure $ round $ ef * eu
evaluateNormalLength (AST.InternalLength v) = evaluateInternalLength v

evaluateFactor :: (MonadState Config m, MonadError String m) => AST.Factor -> m Rational
evaluateFactor = \case
    AST.NormalIntegerFactor n -> fromIntegral <$> evaluateNormalInteger n
    AST.RationalConstant r -> pure r

evaluateUnit :: (MonadState Config m, MonadError String m) => AST.Unit -> m Rational
evaluateUnit = \case
    AST.InternalUnit u -> evaluateInternalUnit u
    AST.PhysicalUnit AST.TrueFrame u -> pure $ Unit.inScaledPoint u
    AST.PhysicalUnit AST.MagnifiedFrame u ->
        do
        _mag <- gets (mag . params)
        eU <- evaluateUnit (AST.PhysicalUnit AST.TrueFrame u)
        pure $ eU * 1000 / fromIntegral _mag

evaluateInternalUnit :: (MonadState Config m, MonadError String m) => AST.InternalUnit -> m Rational
evaluateInternalUnit = \case
    AST.Em -> (TFM.quad . fontMetrics) <$> currentFontInfo
    AST.Ex -> (TFM.xHeight . fontMetrics) <$> currentFontInfo
    AST.InternalIntegerUnit v -> fromIntegral <$> evaluateInternalInteger v
    AST.InternalLengthUnit v -> fromIntegral <$> evaluateInternalLength v
    AST.InternalGlueUnit v -> (fromIntegral . BL.dimen) <$> evaluateInternalGlue v

evaluateInternalLength :: (MonadState Config m, MonadError String m) => AST.InternalLength -> m Int
evaluateInternalLength = \case
    AST.InternalLengthVariable v -> evaluateLengthVariable v
    AST.InternalSpecialLength v -> evaluateSpecialLength v
    AST.InternalFontDimensionRef v -> evaluateFontDimensionRef v
    AST.InternalBoxDimensionRef v -> evaluateBoxDimensionRef v
    AST.LastKern -> undefined

evaluateLengthVariable :: (MonadState Config m, MonadError String m) => AST.LengthVariable -> m Int
evaluateLengthVariable = \case
    AST.ParamVar p -> gets (getLenParam p . params)
    AST.RegisterVar n ->
        do
        _ <- evaluateNumber n
        undefined

evaluateSpecialLength :: (MonadState Config m, MonadError String m) => T.SpecialLength -> m Int
evaluateSpecialLength p = gets (getSpecialLen p . params)

evaluateFontDimensionRef :: (MonadState Config m, MonadError String m) => AST.FontDimensionRef -> m Int
evaluateFontDimensionRef (AST.FontDimensionRef n _) =
    do
    _ <- evaluateNumber n
    undefined

evaluateBoxDimensionRef :: (MonadState Config m, MonadError String m) => AST.BoxDimensionRef -> m Int
evaluateBoxDimensionRef (AST.BoxDimensionRef n _) =
    do
    _ <- evaluateNumber n
    undefined

evaluateCoercedLength :: (MonadState Config m, MonadError String m) => AST.CoercedLength -> m Int
evaluateCoercedLength (AST.InternalGlueAsLength g) = BL.dimen <$> evaluateInternalGlue g

-- Glue.

evaluateGlue :: (MonadState Config m, MonadError String m) => AST.Glue -> m BL.Glue
evaluateGlue = \case
    AST.ExplicitGlue dim str shr ->
        BL.Glue <$> evaluateLength dim <*> evaluateFlex str <*> evaluateFlex shr
    AST.InternalGlue (T.Sign isPos) v ->
        do
        ev <- evaluateInternalGlue v
        pure $ if isPos then ev else (BL.negateGlue ev)

evaluateFlex :: (MonadState Config m, MonadError String m) => Maybe AST.Flex -> m BL.GlueFlex
evaluateFlex = \case
    Just (AST.FiniteFlex ln) ->
        do
        eLn <- evaluateLength ln
        pure BL.GlueFlex{factor = fromIntegral eLn, order = 0}
    Just (AST.FilFlex ln) -> evaluateFilLength ln
    Nothing -> pure BL.noFlex

evaluateFilLength :: (MonadState Config m, MonadError String m) => AST.FilLength -> m BL.GlueFlex
evaluateFilLength (AST.FilLength (T.Sign isPos) f ord) =
    do
    eF <- evaluateFactor f
    pure BL.GlueFlex{factor = if isPos then eF else -eF, order = ord}

evaluateInternalGlue :: (MonadState Config m, MonadError String m) => AST.InternalGlue -> m BL.Glue
evaluateInternalGlue = \case
    AST.InternalGlueVariable v -> evaluateGlueVariable v
    AST.LastGlue -> undefined

evaluateGlueVariable :: (MonadState Config m, MonadError String m) => AST.GlueVariable -> m BL.Glue
evaluateGlueVariable = \case
    AST.ParamVar p -> gets (getGlueParam p . params)
    AST.RegisterVar n ->
        do
        _ <- evaluateNumber n
        undefined

-- Other.

evaluateKern :: (MonadState Config m, MonadError String m) => AST.Length -> m B.Kern
evaluateKern ln = B.Kern <$> evaluateLength ln

evaluatePenalty :: (MonadState Config m, MonadError String m) => AST.Number -> m BL.Penalty
evaluatePenalty n = (BL.Penalty . fromIntegral) <$> evaluateNumber n

evaluateCharCodeRef :: (MonadState Config m, MonadError String m) => AST.CharCodeRef -> m CharCode
evaluateCharCodeRef ref = case ref of
    AST.CharRef c       -> pure c
    AST.CharTokenRef c  -> pure $ chr c
    AST.CharCodeNrRef n -> chr <$> evaluateNumber n
