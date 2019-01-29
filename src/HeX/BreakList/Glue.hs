module HeX.BreakList.Glue where

import           HeXPrelude

import qualified HeX.Unit                      as UN

-- Flex.

data GlueFlex = GlueFlex
    { factor :: !Rational
    , order :: !Int
    } deriving (Show, Eq)

instance Readable GlueFlex where
    describe (GlueFlex 0 0) = "0"
    describe (GlueFlex f 0) = UN.showSP f
    describe (GlueFlex f n) = show f ++ " fil" ++ show n

instance Semigroup GlueFlex where
    a@(GlueFlex fA oA) <> b@(GlueFlex fB oB) =
        case compare oA oB of
            GT -> a
            LT -> b
            EQ -> GlueFlex (fA + fB) oA

instance Monoid GlueFlex where
    mempty = GlueFlex 0 0

noFlex :: GlueFlex
noFlex = mempty

finiteFlex :: Rational -> GlueFlex
finiteFlex f = GlueFlex f 0

filFlex :: GlueFlex
filFlex = GlueFlex 1 1

-- Glue.

data Glue = Glue
    { dimen :: !Int
    , stretch :: !GlueFlex
    , shrink :: !GlueFlex
    } deriving (Show, Eq)

instance Readable Glue where
    describe (Glue d (GlueFlex 0 0) (GlueFlex 0 0)) =
        "{- " ++ UN.showSP d ++ " -}"
    describe (Glue d str shr) =
        "{" ++ UN.showSP d ++ ("+" ++ show str) ++ ("-" ++ show shr) ++ "}"

instance Semigroup Glue where
    (Glue dA strA shrA) <> (Glue dB strB shrB) =
        Glue (dA + dB) (strA `mappend` strB) (shrA `mappend` shrB)

instance Monoid Glue where
    mempty = Glue 0 mempty mempty

negateGlue :: Glue -> Glue
negateGlue (Glue d str shr) = Glue (-d) str shr

filGlue :: Glue
filGlue = Glue {dimen = 0, stretch = filFlex, shrink = noFlex}
