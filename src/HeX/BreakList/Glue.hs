module HeX.BreakList.Glue where

import           HeXlude

import qualified HeX.Unit as UN

-- Flex.
data GlueFlex = GlueFlex { factor :: !Rational, order :: !Int }
    deriving ( Show, Eq )

instance Readable GlueFlex where
    describe (GlueFlex 0 0) = "0"
    describe (GlueFlex f 0) = UN.showSP f
    describe (GlueFlex f n) = show f <> " fil" <> show n

instance Semigroup GlueFlex where
    a@(GlueFlex fA oA) <> b@(GlueFlex fB oB) = case compare oA oB of
        GT -> a
        LT -> b
        EQ -> GlueFlex (fA + fB) oA

instance Monoid GlueFlex where
    mempty = GlueFlex 0 0

multiplyFlex :: GlueFlex -> TeXIntVal -> GlueFlex
multiplyFlex (GlueFlex f o) i = GlueFlex (f * (fromIntegral i)) o

divFlex :: GlueFlex -> TeXIntVal -> GlueFlex
divFlex (GlueFlex f o) i = GlueFlex (f / (fromIntegral i)) o

noFlex :: GlueFlex
noFlex = mempty

finiteFlex :: Rational -> GlueFlex
finiteFlex f = GlueFlex f 0

filFlex :: GlueFlex
filFlex = GlueFlex 1 1

-- Glue.
data Glue =
    Glue { dimen :: !TeXIntVal, stretch :: !GlueFlex, shrink :: !GlueFlex }
    deriving ( Show, Eq )

instance Readable Glue where
    describe (Glue d (GlueFlex 0 0) (GlueFlex 0 0)) =
        "{- " <> UN.showSP d <> " -}"
    describe (Glue d str shr) =
        "{" <> UN.showSP d <> ("+" <> show str) <> ("-" <> show shr) <> "}"

instance Semigroup Glue where
    (Glue dA strA shrA) <> (Glue dB strB shrB) =
        Glue (dA + dB) (strA `mappend` strB) (shrA `mappend` shrB)

instance Monoid Glue where
    mempty = Glue 0 mempty mempty

multiplyGlue :: Glue -> Int -> Glue
multiplyGlue (Glue dim str shr) i =
    Glue (dim * i) (multiplyFlex str i) (multiplyFlex shr i)

-- \divide <glue> by 2’ halves all three components of <glue>.
divGlue :: Glue -> Int -> Glue
divGlue (Glue dim str shr) i = Glue (dim `quot` i) (divFlex str i) (divFlex shr i)

negateGlue :: Glue -> Glue
negateGlue (Glue d str shr) = Glue (-d) str shr

filGlue :: Glue
filGlue = Glue { dimen = 0, stretch = filFlex, shrink = noFlex }

fixedGlue :: TeXIntVal -> Glue
fixedGlue d = Glue d noFlex noFlex

newtype MathGlue = MathGlue { unMathGlue :: Glue }
    deriving ( Show, Eq, Semigroup, Monoid )

multiplyMathGlue :: MathGlue -> Int -> MathGlue
multiplyMathGlue (MathGlue g) i = MathGlue (multiplyGlue g i)

divMathGlue :: MathGlue -> Int -> MathGlue
divMathGlue (MathGlue g) i = MathGlue (divGlue g i)

negateMathGlue :: MathGlue -> MathGlue
negateMathGlue (MathGlue g) = MathGlue $ negateGlue g
