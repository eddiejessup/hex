{-# LANGUAGE DuplicateRecordFields #-}

module HeX.Evaluate where

import           HeX.Config
import qualified HeX.Parse                     as HP
import qualified HeX.Unit                      as Unit
import qualified HeX.BreakList                 as BL
import qualified HeX.Box                       as B

evaluateNormalInteger :: HP.NormalInteger -> Integer
evaluateNormalInteger (HP.IntegerConstant n) = n

evaluateUnsignedNumber :: HP.UnsignedNumber -> Integer
evaluateUnsignedNumber (HP.NormalIntegerAsUNumber n) = evaluateNormalInteger n

evaluateNumber :: HP.Number -> Integer
evaluateNumber (HP.Number True u) = evaluateUnsignedNumber u
evaluateNumber (HP.Number False u) = -(evaluateUnsignedNumber u)

evaluateFactor :: HP.Factor -> Rational
evaluateFactor (HP.NormalIntegerFactor n) =
  fromIntegral $ evaluateNormalInteger n
evaluateFactor (HP.RationalConstant r) = r

evaluateUnit :: HP.Unit -> Rational
evaluateUnit (HP.PhysicalUnit _ u) = Unit.inScaledPoint u
-- TODO:
evaluateUnit (HP.InternalUnit HP.Em) = 10
evaluateUnit (HP.InternalUnit HP.Ex) = 10

evaluateNormalLength :: (IntParamVal Mag) -> HP.NormalLength -> Int
evaluateNormalLength m (HP.LengthSemiConstant f u@(HP.PhysicalUnit isTrue _)) =
  round $ evalF isTrue * evaluateUnit u
  where
    evalF False = evaluateFactor f
    evalF True = evalF False * 1000 / fromIntegral m
evaluateNormalLength _ (HP.LengthSemiConstant f u) =
  round $ evaluateFactor f * evaluateUnit u

evaluateULength :: (IntParamVal Mag) -> HP.UnsignedLength -> Int
evaluateULength m (HP.NormalLengthAsULength nLn) = evaluateNormalLength m nLn

evaluateLength :: (IntParamVal Mag) -> HP.Length -> Int
evaluateLength m (HP.Length True uLn) = evaluateULength m uLn
evaluateLength m (HP.Length False uLn) = -(evaluateULength m uLn)

evaluateFlex :: (IntParamVal Mag) -> Maybe HP.Flex -> BL.GlueFlex
evaluateFlex m (Just (HP.FiniteFlex ln)) =
  BL.GlueFlex {factor = fromIntegral $ evaluateLength m ln, order = 0}
evaluateFlex _ (Just (HP.FilFlex (HP.FilLength True f ord))) =
  BL.GlueFlex {factor = evaluateFactor f, order = ord}
evaluateFlex _ (Just (HP.FilFlex (HP.FilLength False f ord))) =
  BL.GlueFlex {factor = -(evaluateFactor f), order = ord}
evaluateFlex _ Nothing = BL.noFlex

evaluateGlue :: (IntParamVal Mag) -> HP.Glue -> BL.Glue
evaluateGlue m (HP.ExplicitGlue dim str shr) =
  BL.Glue
  { dimen = evaluateLength m dim
  , stretch = evaluateFlex m str
  , shrink = evaluateFlex m shr
  }

evaluateKern :: (IntParamVal Mag) -> HP.Length -> B.Kern
evaluateKern m = B.Kern . evaluateLength m

evaluatePenalty :: HP.Number -> BL.Penalty
evaluatePenalty = BL.Penalty . fromIntegral . evaluateNumber
