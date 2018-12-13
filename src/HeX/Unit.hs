module HeX.Unit where

import           Data.Ratio                     ( (%) )
import           Text.Printf                    ( printf )

tenK :: Int
tenK = 10000

hunK :: Int
hunK = 100000

oneKPt :: Int
oneKPt = toScaledPointApprox (1000 :: Int) Point

-- Functions related to units used in the TeX world.
-- A scaled point is defined as a fraction:
-- > num = 2.54 * 1e7 = 25400000
-- > den = 7227 * 2^16 = 473628672
-- > 1 scaled point = num / den = 5.4 nm
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

inScaledPoint :: PhysicalUnit -> Rational
inScaledPoint Point = fromIntegral pointInScaledPoint
inScaledPoint Pica = fromIntegral $ picaInPoint * pointInScaledPoint
inScaledPoint Inch = inchInPoint * inScaledPoint Point
inScaledPoint BigPoint = inchInPoint * inScaledPoint Point / fromIntegral inchInBigPoint
inScaledPoint Centimetre = 10 * inScaledPoint Millimetre
inScaledPoint Millimetre = inScaledPoint Inch / inchInMM
inScaledPoint Didot = didotInPoint * inScaledPoint Point
inScaledPoint Cicero = 12 * inScaledPoint Didot
inScaledPoint ScaledPoint = 1

scaledPointIn :: PhysicalUnit -> Rational
scaledPointIn = recip . inScaledPoint

toScaledPoint :: Real n => n -> PhysicalUnit -> Rational
toScaledPoint n u = realToFrac n * inScaledPoint u

toScaledPointApprox :: Real n => n -> PhysicalUnit -> Int
toScaledPointApprox n u = round $ toScaledPoint n u

fromScaledPoint :: PhysicalUnit -> Rational
fromScaledPoint = recip . inScaledPoint

showFrac :: Real n => n -> String
showFrac n = printf "%.1f" (realToFrac n :: Double)

showSP :: Real n => n -> String
showSP n =
  showFrac ((realToFrac n * realToFrac (scaledPointIn Point)) :: Double) ++ "pt"
