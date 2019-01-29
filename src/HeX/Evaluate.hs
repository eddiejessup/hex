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
evaluateUnsignedNumber (AST.NormalIntegerAsUNumber v) = evaluateNormalInteger v
evaluateUnsignedNumber (AST.CoercedInteger v) = evaluateCoercedInteger v

evaluateNormalInteger :: (MonadState Config m, MonadError String m) => AST.NormalInteger -> m Int
evaluateNormalInteger (AST.IntegerConstant n) = pure n
evaluateNormalInteger (AST.InternalInteger v) = evaluateInternalInteger v

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
evaluateIntegerVariable (AST.ParamVar _) = undefined
evaluateIntegerVariable (AST.RegisterVar n) =
    do
    _ <- evaluateNumber n
    undefined

evaluateSpecialInteger :: (MonadState Config m, MonadError String m) => T.SpecialInteger -> m Int
evaluateSpecialInteger _ = undefined

evaluateCodeTableRef :: (MonadState Config m, MonadError String m) => AST.CodeTableRef -> m Int
evaluateCodeTableRef (AST.CodeTableRef _ n) =
    do
    _ <- evaluateNumber n
    undefined

evaluateFontCharRef :: (MonadState Config m, MonadError String m) => AST.FontCharRef -> m Int
evaluateFontCharRef (AST.FontCharRef _ _) = undefined

evaluateCoercedInteger :: (MonadState Config m, MonadError String m) => AST.CoercedInteger -> m Int
evaluateCoercedInteger (AST.InternalLengthAsInt ln) = evaluateInternalLength ln
evaluateCoercedInteger (AST.InternalGlueAsInt g) = BL.dimen <$> evaluateInternalGlue g

-- Length.

evaluateLength :: (MonadState Config m, MonadError String m) => AST.Length -> m Int
evaluateLength (AST.Length (T.Sign isPos) uLn) =
    do
    eULn <- evaluateUnsignedLength uLn
    pure $ if isPos then eULn else -eULn

evaluateUnsignedLength :: (MonadState Config m, MonadError String m) => AST.UnsignedLength -> m Int
evaluateUnsignedLength (AST.NormalLengthAsULength v) = evaluateNormalLength v
evaluateUnsignedLength (AST.CoercedLength v) = evaluateCoercedLength v

evaluateNormalLength :: (MonadState Config m, MonadError String m) => AST.NormalLength -> m Int
evaluateNormalLength (AST.LengthSemiConstant f u) =
    do
    ef <- evaluateFactor f
    eu <- evaluateUnit u
    pure $ round $ ef * eu
evaluateNormalLength (AST.InternalLength v) = evaluateInternalLength v

evaluateFactor :: (MonadState Config m, MonadError String m) => AST.Factor -> m Rational
evaluateFactor (AST.NormalIntegerFactor n) = fromIntegral <$> evaluateNormalInteger n
evaluateFactor (AST.RationalConstant r) = pure r

evaluateUnit :: (MonadState Config m, MonadError String m) => AST.Unit -> m Rational
evaluateUnit (AST.PhysicalUnit AST.TrueFrame u) = pure $ Unit.inScaledPoint u
evaluateUnit (AST.PhysicalUnit AST.MagnifiedFrame u) =
    do
    _mag <- gets (mag . params)
    eU <- evaluateUnit (AST.PhysicalUnit AST.TrueFrame u)
    pure $ eU * 1000 / fromIntegral _mag
evaluateUnit (AST.InternalUnit u) = evaluateInternalUnit u

evaluateInternalUnit :: (MonadState Config m, MonadError String m) => AST.InternalUnit -> m Rational
evaluateInternalUnit AST.Em = TFM.quad <$> currentFontInfo
evaluateInternalUnit AST.Ex = TFM.xHeight <$> currentFontInfo
evaluateInternalUnit (AST.InternalIntegerUnit v) = fromIntegral <$> evaluateInternalInteger v
evaluateInternalUnit (AST.InternalLengthUnit v) = fromIntegral <$> evaluateInternalLength v
evaluateInternalUnit (AST.InternalGlueUnit v) = (fromIntegral . BL.dimen) <$> evaluateInternalGlue v

evaluateInternalLength :: (MonadState Config m, MonadError String m) => AST.InternalLength -> m Int
evaluateInternalLength (AST.InternalLengthVariable v) = evaluateLengthVariable v
evaluateInternalLength (AST.InternalSpecialLength v) = evaluateSpecialLength v
evaluateInternalLength (AST.InternalFontDimensionRef v) = evaluateFontDimensionRef v
evaluateInternalLength (AST.InternalBoxDimensionRef v)  = evaluateBoxDimensionRef v
evaluateInternalLength (AST.LastKern) = undefined

evaluateLengthVariable :: (MonadState Config m, MonadError String m) => AST.LengthVariable -> m Int
evaluateLengthVariable (AST.ParamVar _) = undefined
evaluateLengthVariable (AST.RegisterVar n) =
    do
    _ <- evaluateNumber n
    undefined

evaluateSpecialLength :: (MonadState Config m, MonadError String m) => T.SpecialLength -> m Int
evaluateSpecialLength _ = undefined

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
evaluateGlue (AST.ExplicitGlue dim str shr) =
    BL.Glue <$> evaluateLength dim <*> evaluateFlex str <*> evaluateFlex shr
evaluateGlue (AST.InternalGlue (T.Sign isPos) v) =
    do
    ev <- evaluateInternalGlue v
    pure $ if isPos then ev else (BL.negateGlue ev)

evaluateFlex :: (MonadState Config m, MonadError String m) => Maybe AST.Flex -> m BL.GlueFlex
evaluateFlex (Just (AST.FiniteFlex ln)) =
    do
    eLn <- evaluateLength ln
    pure BL.GlueFlex{factor = fromIntegral eLn, order = 0}
evaluateFlex (Just (AST.FilFlex ln)) =
    evaluateFilLength ln
evaluateFlex Nothing =
    pure BL.noFlex

evaluateFilLength :: (MonadState Config m, MonadError String m) => AST.FilLength -> m BL.GlueFlex
evaluateFilLength (AST.FilLength (T.Sign isPos) f ord) =
    do
    eF <- evaluateFactor f
    pure BL.GlueFlex{factor = if isPos then eF else -eF, order = ord}

evaluateInternalGlue :: (MonadState Config m, MonadError String m) => AST.InternalGlue -> m BL.Glue
evaluateInternalGlue (AST.InternalGlueVariable v) = evaluateGlueVariable v
evaluateInternalGlue AST.LastGlue = undefined

evaluateGlueVariable :: (MonadState Config m, MonadError String m) => AST.GlueVariable -> m BL.Glue
evaluateGlueVariable (AST.ParamVar _) = undefined
evaluateGlueVariable (AST.RegisterVar n) =
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
