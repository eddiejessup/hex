module HeX.BreakList.Glue where

import qualified HeX.Unit as UN

-- Flex.

data GlueFlex = GlueFlex
  { factor :: Rational
  , order :: Int
  }

instance Show GlueFlex where
  show (GlueFlex 0 0) = "0"
  show (GlueFlex f 0) = UN.showSP f
  show (GlueFlex f n) = show f ++ " fil" ++ show n

instance Semigroup GlueFlex where
  a@GlueFlex {factor = fA, order = oA} <> b@GlueFlex {factor = fB, order = oB} =
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
  { dimen :: Int
  , stretch :: GlueFlex
  , shrink :: GlueFlex
  }

instance Show Glue where
  show (Glue d (GlueFlex 0 0) (GlueFlex 0 0)) = "{- " ++ UN.showSP d ++ " -}"
  show (Glue d str shr) =
    "{" ++ UN.showSP d ++ ("+" ++ show str) ++ ("-" ++ show shr) ++ "}"

instance Semigroup Glue where
  (Glue dA strA shrA) <> (Glue dB strB shrB) =
    Glue (dA + dB) (strA `mappend` strB) (shrA `mappend` shrB)

instance Monoid Glue where
  mempty = Glue 0 mempty mempty

filGlue :: Glue
filGlue = Glue {dimen = 0, stretch = filFlex, shrink = noFlex}
