module DVI.Unit where

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

bigPointInInch = 72
didotInCicero = 12
didotInPoint = 1157 / 1238
mmInInch = 254 / 10
pointInInch = 7227 / 100
scaledPointInPoint = 2 ^ 16

inchInMM = 1 / mmInInch
pointInMM = pointInInch * inchInMM
inchInPoint = 1 / pointInInch

mmInPoint = 1 / pointInMM
pointInScaledPoint = 1 / scaledPointInPoint

mmToInch = (* inchInMM)
inchToMM = (* mmInInch)
dpiToMM = (mmInInch /)
inchToPoint = (* pointInInch)
pointToInch = (* inchInPoint)
pointToMM = (* mmInPoint)
scaledPointToPoint = (* pointInScaledPoint)
scaledPointToInch = pointToInch . scaledPointToPoint
scaledPointToMM = pointToMM . scaledPointToPoint
scaledPointToDPI = inchToPoint . scaledPointToPoint
pointToScaledPoint = (* scaledPointInPoint)
inchToScaledPoint = pointToScaledPoint . inchToPoint
