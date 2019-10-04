module HeX.BreakList.Glue where

import           HeXlude

import           HeX.Quantity

-- Flex.
data GlueFlex = GlueFlex { factor :: !Rational, order :: !Int }
    deriving ( Show, Eq )

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

scaleFlex :: GlueFlex -> Int -> GlueFlex
scaleFlex (GlueFlex f o) i = GlueFlex (f * fromIntegral i) o

shrinkFlex :: GlueFlex -> Int -> GlueFlex
shrinkFlex (GlueFlex f o) i = GlueFlex (f / fromIntegral i) o

noFlex :: GlueFlex
noFlex = mempty

finiteFlex :: Rational -> GlueFlex
finiteFlex f = GlueFlex f 0

filFlex :: GlueFlex
filFlex = GlueFlex 1 1

-- Glue.
data Glue a =
    Glue { dimen :: a, stretch :: GlueFlex, shrink :: GlueFlex }

deriving instance Show a => Show (Glue a)
deriving instance Eq a => Eq (Glue a)

negateGlue :: Num a => Glue a -> Glue a
negateGlue (Glue d str shr) = Glue (-d) str shr

instance Num a => Semigroup (Glue a) where
    (Glue dA strA shrA) <> (Glue dB strB shrB) =
        Glue (dA + dB) (strA <> strB) (shrA <> shrB)

instance Num a => Monoid (Glue a) where
    mempty = Glue 0 mempty mempty

instance Readable (Glue TeXLength) where
    describe (Glue d (GlueFlex 0 0) (GlueFlex 0 0)) =
        "{- " <> showSP d <> " -}"
    describe (Glue d str shr) =
        "{" <> showSP d <> ("+" <> show str) <> ("-" <> show shr) <> "}"


scaleTeXLengthGlue :: (Glue TeXLength) -> Int -> Glue TeXLength
scaleTeXLengthGlue ( (Glue dim str shr)) i =
    Glue (scaleTeXLength dim i) (scaleFlex str i) (scaleFlex shr i)

-- \divide <glue> by 2’ halves all three components of <glue>.
shrinkTeXLengthGlue :: Glue TeXLength -> Int -> Glue TeXLength
shrinkTeXLengthGlue (Glue dim str shr) i =
    Glue (shrinkTeXLength dim i) (shrinkFlex str i) (shrinkFlex shr i)

filGlue :: Integral a => Glue a
filGlue = Glue { dimen = 0, stretch = filFlex, shrink = noFlex }

fixedGlue :: TeXLength -> Glue TeXLength
fixedGlue d = Glue { dimen = d, stretch = noFlex, shrink = noFlex }


scaleMathGlue :: Glue MathLength -> Int -> Glue MathLength
scaleMathGlue (Glue d str shr) i =
    Glue (scaleMathLength d i) (scaleFlex str i) (scaleFlex shr i)

shrinkMathGlue :: Glue MathLength -> Int -> Glue MathLength
shrinkMathGlue (Glue d str shr) i =
    Glue (shrinkMathLength d i) (shrinkFlex str i) (shrinkFlex shr i)
