module HeX.Unit where

import Data.Ratio ((%))
import Text.Printf (printf)

-- Functions related to units used in the TeX world.
-- A scaled point is defined as a fraction:
-- * num = 2.54 * 1e7 = 25400000
-- * den = 7227 * 2^16 = 473628672
-- * 1 scaled point = num / den = 5.4 nm
-- The DVI format's base unit is 100 nm.
-- Basic facts.
-- 1 pica is 12 points.
picaInPoint :: Integer
picaInPoint = 12

-- 1 inch is 72.27 points.
inchInPoint :: Rational
inchInPoint = 7227 % 100

-- 1 inch is 72 big points.
inchInBigPoint :: Integer
inchInBigPoint = 72

-- 1 inch is 25.4 mm.
inchInMM :: Rational
inchInMM = 254 % 10

-- 1 centimetre is 10 millimetres.
cmInMM :: Integer
cmInMM = 10

-- 1 didot is 1238/1157 points.
didotInPoint :: Rational
didotInPoint = 1238 % 1157

-- 1 cicero is 12 didots.
ciceroInDidot :: Integer
ciceroInDidot = 12

-- 1 point is 2^16 scaled points.
pointInScaledPoint :: Integer
pointInScaledPoint = 2 ^ (16 :: Int)

data PhysicalUnit
  = Point -- 'pt'
  | Pica -- 'pc'
  | Inch -- 'in'
  | BigPoint -- 'bp'
  | Centimetre -- 'cm'
  | Millimetre -- 'mm'
  | Didot -- 'dd'
  | Cicero -- 'cc'
  | ScaledPoint -- 'sp'
  deriving (Show)

oneKPt :: Int
oneKPt = toScaledPointApprox (1000 :: Int) Point

class Length u where
  inScaledPoint :: u -> Rational
  scaledPointIn :: u -> Rational
  scaledPointIn = recip . inScaledPoint
  toScaledPoint :: Real n => n -> u -> Rational
  toScaledPoint n u = realToFrac n * inScaledPoint u
  toScaledPointApprox :: Real n => n -> u -> Int
  toScaledPointApprox n u = round $ toScaledPoint n u
  fromScaledPoint :: u -> Rational
  fromScaledPoint = recip . inScaledPoint

-- pointToScaledPoint a = a * fromIntegral pointInScaledPoint
instance Length PhysicalUnit where
  inScaledPoint u =
    case u of
      Point -> fromIntegral pointInScaledPoint
      Pica -> fromIntegral $ picaInPoint * pointInScaledPoint
      Inch -> inchInPoint * inScaledPoint Point
      BigPoint ->
        inchInPoint * inScaledPoint Point / fromIntegral inchInBigPoint
      Centimetre -> 10 * inScaledPoint Millimetre
      Millimetre -> inScaledPoint Inch / inchInMM
      Didot -> didotInPoint * inScaledPoint Point
      Cicero -> 12 * inScaledPoint Didot
      ScaledPoint -> 1

showFrac :: Real n => n -> String
showFrac n = printf "%.1f" (realToFrac n :: Double)

showSP :: Real n => n -> String
showSP n =
  showFrac ((realToFrac n * realToFrac (scaledPointIn Point)) :: Double) ++ "pt"
