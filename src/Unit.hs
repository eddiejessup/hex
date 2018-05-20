module Unit where

import Data.Ratio ((%))

-- Functions related to units used in the TeX world.

-- * 'mm' stands for millimetre
-- * An inch corresponds to 25.4 mm
-- * A 'point' stands for a TeX point. There are 72.27 points in one inch
-- * There are 2^16 'scaled points' in one point
-- * 'DPI' stands for 'dots per inch'.

-- A scaled point is defined as a fraction:
-- * num = 2.54 * 1e7 = 25400000
-- * den = 7227 * 2^16 = 473628672
-- * 1 scaled point = num / den = 5.4 nm

-- The DVI format's base unit is 100 nm.

-- For a resolution of 1200 dpi, a pixel measures 21 um.

-- Basic facts.
bigPointInInch :: Integer
bigPointInInch = 72
didotInCicero :: Integer
didotInCicero = 12
didotInPoint :: Rational
didotInPoint = 1157 % 1238
mmInInch :: Rational
mmInInch = 254 % 10
pointInInch :: Rational
pointInInch = 7227 % 100
scaledPointInPoint :: Integer
scaledPointInPoint = 2 ^ (16 :: Integer)

-- Derived conversions.
inchInMM :: Rational
inchInMM = recip mmInInch
pointInMM :: Rational
pointInMM = pointInInch * inchInMM
mmInPoint :: Rational
mmInPoint = recip pointInMM
pointInScaledPoint :: Rational
pointInScaledPoint = 1 % scaledPointInPoint

scaledPointToPoint :: Rational -> Rational
scaledPointToPoint = (pointInScaledPoint *)
pointToScaledPoint :: Rational -> Rational
pointToScaledPoint = (fromInteger scaledPointInPoint *)

pointToMM :: Rational -> Rational
pointToMM = (mmInPoint *)
mmToPoint :: Rational -> Rational
mmToPoint = (pointInMM *)

scaledPointToMM :: Rational -> Rational
scaledPointToMM = pointToMM . scaledPointToPoint
mmToScaledPoint :: Rational -> Rational
mmToScaledPoint = pointToScaledPoint . mmToPoint
