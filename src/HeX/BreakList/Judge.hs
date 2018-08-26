{-# LANGUAGE DuplicateRecordFields #-}

module HeX.BreakList.Judge where

import qualified HeX.Unit as UN

import HeX.BreakList.Glue

data Fixable
  = Fixable { ratio :: Rational
            , order :: Int }
  | Unfixable

instance Show Fixable where
  show Fixable {ratio = r, order = 0} =
    "Fixable, finite scale: " ++ UN.showFrac r
  show Fixable {ratio = r, order = 1} = "Fixable, fil ratio: " ++ UN.showSP r
  show Fixable {ratio = r, order = n} =
    "Fixable, fil order " ++ show n ++ " ratio: " ++ UN.showSP r
  show Unfixable = "Unfixable"

data LengthJudgment
  = Full
  | Bare
  deriving (Show)

data GlueStatus
  = NaturallyGood
  | NaturallyBad LengthJudgment
                 Fixable
  deriving (Show)

glueStatus :: Int -> Glue -> GlueStatus
glueStatus excessLength (Glue _ _stretch _shrink)
  -- The glue ratio is r = [excess length]/[flex]i.
  -- The natural width x is compared to the desired width w.
 =
  case compare excessLength 0
       -- If x = w, all glue gets its natural width.
        of
    EQ -> NaturallyGood
       -- Otherwise the glue will be modified, by computing a “glue set ratio” r
       -- and a “glue set order” i
    LT -> NaturallyBad Bare $ stretchStatus (fromIntegral excessLength) _stretch
    GT -> NaturallyBad Full $ shrinkStatus (fromIntegral excessLength) _shrink
  where
    stretchStatus e GlueFlex {factor = f, order = o}
      -- No stretchability.
      | f == 0 = Unfixable
      | otherwise = Fixable {ratio = -(e / f), order = o}
    shrinkStatus e GlueFlex {factor = f, order = o}
      -- No shrinkability.
      | f == 0 = Unfixable
      | e > f = Unfixable
       -- r is set to 1 if i = 0 and x − w > z0, because the maximum
       -- shrinkability must not be exceeded.
      | o == 0 = Fixable {ratio = 1, order = o}
      | otherwise = Fixable {ratio = e / f, order = o}

-- glueStatus :: Int -> Glue -> GlueStatus
-- glueStatus excessLength (Glue _ _stretch _shrink) =
--   let regime = compare excessLength 0
--       GlueFlex {factor = flexFactor, order = _order} =
--         if regime == GT
--           then _shrink
--           else _stretch
--     -- The glue ratio is r = [excess length]/[flex]i.
--       gRatio = fromIntegral excessLength / flexFactor
--   -- The natural width x is compared to the desired width w.
--   in case regime
--     -- If x = w, all glue gets its natural width.
--            of
--        EQ -> NaturallyGood
--     -- Otherwise the glue will be modified, by computing a “glue set ratio” r
--     -- and a “glue set order” i
--        LT -> 
--          case flexFactor
--       -- If y0 = y1 = y2 = y3 = 0, there’s no stretchability.
--                of
--            0 -> NaturallyBad Bare Unfixable
--            _ -> NaturallyBad Bare Fixable {ratio = -gRatio, order = _order}
--        GT ->
--          case flexFactor of
--            0 -> NaturallyBad Full Unfixable
--            _ ->
--              if fromIntegral excessLength > flexFactor
--                then NaturallyBad Full Unfixable
--           -- r is set to 1 if i = 0 and x − w > z0, because the maximum
--           -- shrinkability must not be exceeded.
--                else let gRatioShrink =
--                           if _order == 0
--                             then min gRatio 1.0
--                             else gRatio
--                     in NaturallyBad
--                          Full
--                          Fixable {ratio = gRatioShrink, order = _order}
-- TODO: Use types to ensure number is within bounds, such as <= tenK.
data Badness
  = FiniteBadness Int
  | InfiniteBadness

badness :: GlueStatus -> Badness
badness NaturallyGood = FiniteBadness 0
badness (NaturallyBad Full Unfixable) = InfiniteBadness
badness (NaturallyBad Bare Unfixable) = FiniteBadness UN.tenK
-- if i != 0, there is infinite stretchability or shrinkability, so the badness
-- is zero.
-- Otherwise the badness is approximately min(100r3,10000).
badness (NaturallyBad _ Fixable {ratio = r, order = 0}) =
  FiniteBadness $ min UN.tenK $ round $ (r ^ (3 :: Int)) * 100
badness (NaturallyBad _ Fixable {order = _}) = FiniteBadness 0
