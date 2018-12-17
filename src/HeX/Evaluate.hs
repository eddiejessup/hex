{-# LANGUAGE DuplicateRecordFields #-}

module HeX.Evaluate where

import           HeX.Config
import qualified HeX.Parse.Expanded            as E
import qualified HeX.Unit                      as Unit
import qualified HeX.BreakList                 as BL
import qualified HeX.Box                       as B

evaluateNormalInteger :: E.NormalInteger -> Integer
evaluateNormalInteger (E.IntegerConstant n) = n

evaluateUnsignedNumber :: E.UnsignedNumber -> Integer
evaluateUnsignedNumber (E.NormalIntegerAsUNumber n) = evaluateNormalInteger n

evaluateNumber :: E.Number -> Integer
evaluateNumber (E.Number True u) = evaluateUnsignedNumber u
evaluateNumber (E.Number False u) = -(evaluateUnsignedNumber u)

evaluateFactor :: E.Factor -> Rational
evaluateFactor (E.NormalIntegerFactor n) =
  fromIntegral $ evaluateNormalInteger n
evaluateFactor (E.RationalConstant r) = r

evaluateUnit :: E.Unit -> Rational
evaluateUnit (E.PhysicalUnit _ u) = Unit.inScaledPoint u
-- TODO:
evaluateUnit (E.InternalUnit E.Em) = 10
evaluateUnit (E.InternalUnit E.Ex) = 10

evaluateNormalLength :: (IntParamVal Mag) -> E.NormalLength -> Int
evaluateNormalLength m (E.LengthSemiConstant f u@(E.PhysicalUnit isTrue _)) =
  round $ evalF isTrue * evaluateUnit u
  where
    evalF False = evaluateFactor f
    evalF True = evalF False * 1000 / fromIntegral m
evaluateNormalLength _ (E.LengthSemiConstant f u) =
  round $ evaluateFactor f * evaluateUnit u

evaluateULength :: (IntParamVal Mag) -> E.UnsignedLength -> Int
evaluateULength m (E.NormalLengthAsULength nLn) = evaluateNormalLength m nLn

evaluateLength :: (IntParamVal Mag) -> E.Length -> Int
evaluateLength m (E.Length True uLn) = evaluateULength m uLn
evaluateLength m (E.Length False uLn) = -(evaluateULength m uLn)

evaluateFlex :: (IntParamVal Mag) -> Maybe E.Flex -> BL.GlueFlex
evaluateFlex m (Just (E.FiniteFlex ln)) =
  BL.GlueFlex {factor = fromIntegral $ evaluateLength m ln, order = 0}
evaluateFlex _ (Just (E.FilFlex (E.FilLength True f ord))) =
  BL.GlueFlex {factor = evaluateFactor f, order = ord}
evaluateFlex _ (Just (E.FilFlex (E.FilLength False f ord))) =
  BL.GlueFlex {factor = -(evaluateFactor f), order = ord}
evaluateFlex _ Nothing = BL.noFlex

evaluateGlue :: (IntParamVal Mag) -> E.Glue -> BL.Glue
evaluateGlue m (E.ExplicitGlue dim str shr) =
  BL.Glue
  { dimen = evaluateLength m dim
  , stretch = evaluateFlex m str
  , shrink = evaluateFlex m shr
  }

evaluateKern :: (IntParamVal Mag) -> E.Length -> B.Kern
evaluateKern m = B.Kern . evaluateLength m

evaluatePenalty :: E.Number -> BL.Penalty
evaluatePenalty = BL.Penalty . fromIntegral . evaluateNumber
