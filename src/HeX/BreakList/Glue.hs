module Hex.BreakList.Glue where

import Hex.Quantity
import Hexlude

-- Flex.
data GlueFlex = GlueFlex {factor :: Rational, order :: Int}
  deriving (Show, Eq)

instance Readable GlueFlex where

  describe (GlueFlex 0 0) = "0"
  describe (GlueFlex f 0) = showSP f
  describe (GlueFlex f n) = show f <> " fil" <> show n

instance Semigroup GlueFlex where

  a@(GlueFlex fA oA) <> b@(GlueFlex fB oB) = case compare oA oB of
    GT -> a
    LT -> b
    EQ -> GlueFlex (fA + fB) oA

instance Monoid GlueFlex where

  mempty = GlueFlex 0 0

scaleFlex :: GlueFlex -> TeXInt -> GlueFlex
scaleFlex (GlueFlex f o) (TeXInt i) = GlueFlex (f * fromIntegral i) o

shrinkFlex :: GlueFlex -> TeXInt -> GlueFlex
shrinkFlex (GlueFlex f o) (TeXInt i) = GlueFlex (f / fromIntegral i) o

noFlex :: GlueFlex
noFlex = mempty

finiteFlex :: Rational -> GlueFlex
finiteFlex f = GlueFlex f 0

filFlex :: GlueFlex
filFlex = GlueFlex 1 1

-- Glue.
data Glue a
  = Glue {dimen :: a, stretch :: GlueFlex, shrink :: GlueFlex}

deriving instance Show a => Show (Glue a)

deriving instance Eq a => Eq (Glue a)

negateGlue :: Num a => Glue a -> Glue a
negateGlue (Glue d str shr) = Glue (-d) str shr

instance Num a => Semigroup (Glue a) where

  (Glue dA strA shrA) <> (Glue dB strB shrB) =
    Glue (dA + dB) (strA <> strB) (shrA <> shrB)

instance Num a => Monoid (Glue a) where

  mempty = Glue 0 mempty mempty

instance Readable (Glue Length) where

  describe (Glue d (GlueFlex 0 0) (GlueFlex 0 0)) =
    "{- " <> showSP d <> " -}"
  describe (Glue d str shr) =
    "{" <> showSP d <> ("+" <> show str) <> ("-" <> show shr) <> "}"

scaleLengthGlue :: Glue Length -> TeXInt -> Glue Length
scaleLengthGlue (Glue dim str shr) i =
  Glue (scaleLength dim i) (scaleFlex str i) (scaleFlex shr i)

-- \divide <glue> by 2’ halves all three components of <glue>.
shrinkLengthGlue :: Glue Length -> TeXInt -> Glue Length
shrinkLengthGlue (Glue dim str shr) i =
  Glue (shrinkLength dim i) (shrinkFlex str i) (shrinkFlex shr i)

filGlue :: Integral a => Glue a
filGlue = Glue {dimen = 0, stretch = filFlex, shrink = noFlex}

fixedGlue :: Length -> Glue Length
fixedGlue d = Glue {dimen = d, stretch = noFlex, shrink = noFlex}

scaleMathGlue :: Glue MathLength -> TeXInt -> Glue MathLength
scaleMathGlue (Glue d str shr) i =
  Glue (scaleMathLength d i) (scaleFlex str i) (scaleFlex shr i)

shrinkMathGlue :: Glue MathLength -> TeXInt -> Glue MathLength
shrinkMathGlue (Glue d str shr) i =
  Glue (shrinkMathLength d i) (shrinkFlex str i) (shrinkFlex shr i)
