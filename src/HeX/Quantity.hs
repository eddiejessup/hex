module Hex.Quantity where

import           Hexlude hiding ((%))
import Data.Ratio              ((%))

tenK :: TeXInt
tenK = 10000

hunK :: TeXInt
hunK = 100000

oneKPt :: Length
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
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON)

instance Describe PhysicalUnit where
    describe = \case
        Point       -> singleLine "PhysicalUnit/pt"
        Pica        -> singleLine "PhysicalUnit/pc"
        Inch        -> singleLine "PhysicalUnit/in"
        BigPoint    -> singleLine "PhysicalUnit/bp"
        Centimetre  -> singleLine "PhysicalUnit/cm"
        Millimetre  -> singleLine "PhysicalUnit/mm"
        Didot       -> singleLine "PhysicalUnit/dd"
        Cicero      -> singleLine "PhysicalUnit/cc"
        ScaledPoint -> singleLine "PhysicalUnit/sp"

inScaledPoint :: PhysicalUnit -> Rational
inScaledPoint u = case u of
    Point       -> fromIntegral pointInScaledPoint
    Pica        -> fromIntegral $ picaInPoint * pointInScaledPoint
    Inch        -> inchInPoint * inScaledPoint Point
    BigPoint    -> inchInPoint * inScaledPoint Point / fromIntegral inchInBigPoint
    Centimetre  -> 10 * inScaledPoint Millimetre
    Millimetre  -> inScaledPoint Inch / inchInMM
    Didot       -> didotInPoint * inScaledPoint Point
    Cicero      -> 12 * inScaledPoint Didot
    ScaledPoint -> 1

roundToDec :: RealFrac a => Int -> a -> a
roundToDec n v = fromInteger (round $ v * (10 ^ n)) / (10.0 ^^ n)

scaledPointIn :: PhysicalUnit -> Rational
scaledPointIn = recip . inScaledPoint

toScaledPoint :: Real n => n -> PhysicalUnit -> Rational
toScaledPoint n u = realToFrac n * inScaledPoint u

toScaledPointApprox :: (Real n, Integral i) => n -> PhysicalUnit -> i
toScaledPointApprox n u = round $ toScaledPoint n u

fromScaledPoint :: PhysicalUnit -> Rational
fromScaledPoint = recip . inScaledPoint

showFrac :: Real n => n -> Text
showFrac n = show $ roundToDec 1 (realToFrac n :: Double)

showSP :: Real n => n -> Text
showSP n =
  showFrac ((realToFrac n * realToFrac (scaledPointIn Point)) :: Double) <> "pt"

newtype TeXInt = TeXInt { unInt :: Int }
    deriving stock (Show)
    deriving newtype (Num, Eq, Ord, Enum, Bounded, Real, Integral, Hashable, Bits,
                      FiniteBits, Describe, ToJSON)

newtype Length = Length { unLength :: Int }
    deriving stock (Show)
    deriving newtype (Num, Eq, Ord, Enum, Real, Integral, ToJSON)

lengthToInt :: Length -> TeXInt
lengthToInt (Length x) = TeXInt x

scaleLength :: Length -> TeXInt -> Length
scaleLength (Length d) (TeXInt n) = Length (d * n)

shrinkLength :: Length -> TeXInt -> Length
shrinkLength (Length d) (TeXInt n) = Length (d `quot` n)

instance Describe Length where
    describe = singleLine . showSP

newtype MathLength = MathLength { unMathLength :: Int }
    deriving stock (Show)
    deriving newtype (Num, Eq, Ord, Enum, Real, Integral)

scaleMathLength :: MathLength -> TeXInt -> MathLength
scaleMathLength (MathLength d) (TeXInt n) = MathLength (d * n)

shrinkMathLength :: MathLength -> TeXInt -> MathLength
shrinkMathLength (MathLength d) (TeXInt n) = MathLength (d `quot` n)

newNBitInt :: Alternative f => (Int -> a) -> Int ->  Int -> f a
newNBitInt f nBits n
    | n < 0 = empty
    | n >= (2 ^ nBits) = empty
    | otherwise = pure $ f n

-- 8-bit.

newtype EightBitInt = EightBitInt Int
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, Enum)

-- instance Bounded EightBitInt where
--     minBound = EightBitInt 0
--     maxBound = EightBitInt (2 ^ (8 :: Int) - 1)

newEightBitInt :: Alternative f => Int -> f EightBitInt
newEightBitInt = newNBitInt EightBitInt 8

-- 4-bit.

newtype FourBitInt = FourBitInt Int
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, Enum, Hashable)

-- instance Bounded FourBitInt where
--     minBound = FourBitInt 0
--     maxBound = FourBitInt (2 ^ (4 :: Int) - 1)

newFourBitInt :: Alternative f => Int -> f FourBitInt
newFourBitInt = newNBitInt FourBitInt 4

class Dimensioned a where
    naturalLength :: BoxDim -> a -> Length

naturalWidth, naturalHeight, naturalDepth :: Dimensioned a => a -> Length
naturalWidth  = naturalLength BoxWidth
naturalHeight = naturalLength BoxHeight
naturalDepth  = naturalLength BoxDepth

axisNaturalSpan :: Dimensioned a => Axis -> a -> Length
axisNaturalSpan Vertical   a = naturalHeight a + naturalDepth a
axisNaturalSpan Horizontal a = naturalWidth a
