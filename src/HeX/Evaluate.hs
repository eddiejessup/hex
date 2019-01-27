{-# LANGUAGE DuplicateRecordFields #-}

module HeX.Evaluate where

import           Data.Char                      ( chr )

import           HeX.Categorise                 ( CharCode )
import           HeX.Config
import qualified HeX.Parse.AST                 as AST
import qualified HeX.Parse.Token               as T
import qualified HeX.Unit                      as Unit
import qualified HeX.BreakList                 as BL
import qualified HeX.Box                       as B

evaluateNormalInteger :: AST.NormalInteger -> Int
evaluateNormalInteger (AST.IntegerConstant n) = n

evaluateUnsignedNumber :: AST.UnsignedNumber -> Int
evaluateUnsignedNumber (AST.NormalIntegerAsUNumber n) = evaluateNormalInteger n

evaluateNumber :: AST.Number -> Int
evaluateNumber (AST.Number (T.Sign isPos) u)
    | isPos = size
    | otherwise = -size
  where
    size = evaluateUnsignedNumber u

evaluateFactor :: AST.Factor -> Rational
evaluateFactor (AST.NormalIntegerFactor n) =
    fromIntegral $ evaluateNormalInteger n
evaluateFactor (AST.RationalConstant r) = r

evaluateUnit :: AST.Unit -> Rational
evaluateUnit (AST.PhysicalUnit _ u)   = Unit.inScaledPoint u
-- TODO:
evaluateUnit (AST.InternalUnit AST.Em) = 10
evaluateUnit (AST.InternalUnit AST.Ex) = 10

evaluateNormalLength :: (IntParamVal Mag) -> AST.NormalLength -> Int
evaluateNormalLength m (AST.LengthSemiConstant f u@(AST.PhysicalUnit uFrame _)) =
    round $ evalF uFrame * evaluateUnit u
  where
    evalF AST.TrueFrame = evaluateFactor f
    evalF AST.MagnifiedFrame = evalF AST.TrueFrame * 1000 / fromIntegral m
evaluateNormalLength _ (AST.LengthSemiConstant f u) =
    round $ evaluateFactor f * evaluateUnit u

evaluateULength :: (IntParamVal Mag) -> AST.UnsignedLength -> Int
evaluateULength m (AST.NormalLengthAsULength nLn) = evaluateNormalLength m nLn

evaluateLength :: (IntParamVal Mag) -> AST.Length -> Int
evaluateLength m (AST.Length (T.Sign isPos) uLn)
    | isPos = size
    | otherwise = -size
  where
    size = evaluateULength m uLn

evaluateFlex :: (IntParamVal Mag) -> Maybe AST.Flex -> BL.GlueFlex
evaluateFlex m (Just (AST.FiniteFlex ln)) =
    BL.GlueFlex{factor = fromIntegral $ evaluateLength m ln, order = 0}
evaluateFlex _ (Just (AST.FilFlex (AST.FilLength (T.Sign isPos) f ord)))
    | isPos     = BL.GlueFlex{factor = size,    order = ord}
    | otherwise = BL.GlueFlex{factor = -size, order = ord}
  where
    size = evaluateFactor f
evaluateFlex _ Nothing =
    BL.noFlex

evaluateGlue :: (IntParamVal Mag) -> AST.Glue -> BL.Glue
evaluateGlue m (AST.ExplicitGlue dim str shr) = BL.Glue
    { dimen   = evaluateLength m dim
    , stretch = evaluateFlex   m str
    , shrink  = evaluateFlex   m shr
    }

evaluateKern :: (IntParamVal Mag) -> AST.Length -> B.Kern
evaluateKern m = B.Kern . evaluateLength m

evaluatePenalty :: AST.Number -> BL.Penalty
evaluatePenalty = BL.Penalty . fromIntegral . evaluateNumber

evaluateCharCodeRef :: AST.CharCodeRef -> CharCode
evaluateCharCodeRef ref = case ref of
    AST.CharRef c       -> c
    AST.CharTokenRef c  -> c
    AST.CharCodeNrRef n -> chr $ evaluateNumber n
