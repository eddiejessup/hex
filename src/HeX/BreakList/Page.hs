{-# LANGUAGE DuplicateRecordFields #-}

module HeX.BreakList.Page where

import HeX.BreakList

import qualified HeX.Box as B

data PageBreakJudgment
  -- Penalty >= 10k.
  = DoNotBreak
  -- q >= 10k or b = infinity.
  | BreakPageAtBest
  -- p <= -10k.
  | BreakPageHere
  | TrackCost Int
  deriving (Show)

pageBreakJudgment ::
     [BreakableVListElem] -> BreakItem -> Int -> PageBreakJudgment
pageBreakJudgment cs breakItem desiredHeight =
  inner penalty splitInsertPenalties badness
  where
    badness = listStatusBadness $ listGlueSetRatio desiredHeight cs
    penalty = breakPenalty breakItem
    splitInsertPenalties = 0
    inner _ _ InfiniteBadness = BreakPageAtBest
    inner p q (FiniteBadness b)
      | p >= tenK = DoNotBreak
      | q >= tenK = BreakPageAtBest
      | p <= -tenK = BreakPageHere
      | b == tenK = TrackCost hunK
      | otherwise = TrackCost $ b + p + q

-- Assumes cs is in reading order.
setPage :: Int -> [BreakableVListElem] -> B.Page
setPage desiredHeight cs =
  let _status = listGlueSetRatio desiredHeight cs
  in B.Page $ setListElems _status cs
