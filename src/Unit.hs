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
-- 1 inch is 72 big points.
inchInBigPoints :: Integer
inchInBigPoints = 72
-- 1 cicero is 12 didots.
ciceroInDidot :: Integer
ciceroInDidot = 12
-- 1 point is 1157/1238 didot
pointIndidot :: Rational
pointIndidot = 1157 % 1238
-- 1 inch is 2.54 mm.
inchInMM :: Rational
inchInMM = 254 % 10
-- 1 inch is 72.27 points.
inchInPoint :: Rational
inchInPoint = 7227 % 100
-- 1 point is 2^16 scaled points.
pointInScaledPoint :: Integer
pointInScaledPoint = 2 ^ (16 :: Integer)

pointToScaledPoint :: Rational -> Rational
pointToScaledPoint a = a * (fromIntegral pointInScaledPoint)
